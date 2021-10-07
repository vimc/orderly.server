context("workflows")


test_that("workflow missing dependencies errors if report not found", {
  path <- orderly_prepare_orderly_example("minimal", git = TRUE)
  reports <- list(
    list(name = "missing_report")
  )
  expect_error(
    workflow_missing_dependencies(path, reports),
    "Report with name 'missing_report' at git ref 'HEAD' cannot be found.")
})

test_that("can get missing dependencies of a workflow", {
  path <- orderly_prepare_orderly_example("demo", git = TRUE)
  reports <- list(
    list(name = "other")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 other = list()
               )))

  reports <- list(
    list(name = "use_dependency")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 use_dependency = list(scalar("other"))
               )))

  reports <- list(
    list(name = "use_dependency"),
    list(name = "other")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 use_dependency = list(),
                 other = list()
               )))
})

test_that("workflow missing dependencies only looks at depth 1 dependencies", {
  path <- orderly_prepare_orderly_example("demo", git = TRUE)
  reports <- list(
    list(name = "use_dependency_2")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 use_dependency_2 = list(scalar("use_dependency"))
               )))
})

test_that("workflow missing dependencies can handle multiple dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)

  reports <- list(
    list(name = "depend4")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example"), scalar("depend2")))
               ))

  reports <- list(
    list(name = "depend4"),
    list(name = "depend2")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example")),
                 depend2 =  list(scalar("example")))
               ))


  reports <- list(
    list(name = "depend2")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 depend2 = list(scalar("example"))
               )))
})

test_that("can work out dependencies graph", {
  no_deps <- list(
    list(
      name = "depend",
      instance = "production",
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    )
  )
  one_dep <- list(
    list(
      name = "example",
      instance = "production",
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    list(
      name = "depend",
      instance = "production"
    )
  )
  two_deps <- list(
   list(
      name = "example",
      instance = "production",
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    list(
      name = "depend",
      instance = "production"
    ),
    list(
      name = "depend2",
      instance = "production"
    )
  )
  multiple_deps <- list(
    list(
      name = "example",
      instance = "production"
    ),
    list(
      name = "depend2",
      instance = "production"
    ),
    list(
      name = "depend4",
      instance = "production"
    )
  )
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)
  expect_equal(build_dependencies_graph(path, no_deps), list(depend = NA))
  expect_equal(build_dependencies_graph(path, one_dep),
               list(example = NA,
                    depend = "example"))
  expect_equal(build_dependencies_graph(path, two_deps),
               list(example = NA,
                    depend = "example",
                    depend2 = "example"))
  expect_equal(build_dependencies_graph(path, multiple_deps),
               list(example = NA,
                    depend2 = "example",
                    depend4 = c("example", "depend2")))
})

test_that("sorting can be retrieved", {
  graph <- list(b = NA,
                e = "k",
                k = c("b", "i", "j"),
                g = c("k", "h"),
                i = NA,
                j = NA,
                h = "i",
                c = c("j", "h"))
  sort <- topological_sort(graph)
  expect_equal(sort,
               c("b", "i", "j", "k", "h", "e", "g", "c"))
})

test_that("cycled can be detected", {
  expect_error(topological_sort(list(a = "a")),
               "A cyclic dependency detected for a:
  a: depends on a", fixed = TRUE)

  expect_error(topological_sort(list(a = "b", b = "c", c = "a")),
               "A cyclic dependency detected for a, b, c:
  a: depends on b
  b: depends on c
  c: depends on a", fixed = TRUE)

  expect_error(topological_sort(list(a = c("b", "c"), b = "c", c = "a")),
               "A cyclic dependency detected for a, b, c:
  a: depends on b, c
  b: depends on c
  c: depends on a", fixed = TRUE)
})

test_that("workflow representation can be built", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)
  no_deps <- list(
    list(
      name = "depend",
      instance = "production",
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    )
  )
  workflow <- build_workflow(path, no_deps, NULL)
  expect_length(workflow, 1)
  expect_equal(names(workflow[[1]]),
               c("name", "instance", "params", "original_order"))
  expect_equal(workflow[[1]]$name, "depend")
  expect_equal(workflow[[1]]$original_order, 1)

  multiple_deps <- list(
    list(
      name = "example",
      instance = "production"
    ),
    list(
      name = "depend4",
      instance = "production"
    ),
    list(
      name = "depend2",
      instance = "production"
    )
  )
  workflow <- build_workflow(path, multiple_deps, NULL)
  expect_length(workflow, 3)

  expect_equal(names(workflow[[1]]), c("name", "instance", "original_order"))
  expect_equal(workflow[[1]]$name, "example")
  expect_null(workflow[[1]]$depends_on)
  expect_equal(workflow[[1]]$original_order, 1)

  expect_equal(names(workflow[[2]]),
               c("name", "instance", "original_order", "depends_on"))
  expect_equal(workflow[[2]]$name, "depend2")
  expect_equal(workflow[[2]]$depends_on, "example")
  expect_equal(workflow[[2]]$original_order, 3)

  expect_equal(names(workflow[[3]]),
               c("name", "instance", "original_order", "depends_on"))
  expect_equal(workflow[[3]]$name, "depend4")
  expect_equal(workflow[[3]]$depends_on, c("example", "depend2"))
  expect_equal(workflow[[3]]$original_order, 2)
})

test_that("workflow with duplicate reports can be built", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)
  no_deps <- list(
    list(
      name = "depend"
    ),
    list(
      name = "depend"
    )
  )
  workflow <- build_workflow(path, no_deps, NULL)
  expect_length(workflow, 2)
  expect_equal(workflow[[1]]$name, "depend")
  expect_equal(workflow[[1]]$original_order, 1)
  expect_equal(workflow[[2]]$name, "depend")
  expect_equal(workflow[[2]]$original_order, 2)
})

test_that("workflow can be run: simple", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- orderly_runner(path)

  no_deps <- list(
    list(
      name = "example"
    )
  )
  res <- runner$submit_workflow(no_deps)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    tasks <- runner$queue$task_list()
    expect_length(tasks, 1)
  })
  expect_equal(names(res), c("workflow_key", "reports"))
  expect_true(!is.null(res$workflow_key))
  redis_key <- workflow_redis_key(runner$queue$queue_id, res$workflow_key)
  expect_equal(runner$con$SMEMBERS(redis_key), list(res$reports))
  expect_length(res$reports, 1)
  task_id <- get_task_id_key(runner, res$reports)
  expect_equal(tasks, task_id)

  result <- runner$queue$task_wait(task_id)
  status <- runner$status(res$reports, output = TRUE)
  expect_equal(status$status, "success")
  expect_match(status$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_match(status$output, "\\[ data +\\]  source => dat: 20 x 2",
               all = FALSE)
})

test_that("workflow can be run: dependencies", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- orderly_runner(path)

  multiple_deps <- list(
    list(
      name = "example"
    ),
    list(
      name = "depend4"
    ),
    list(
      name = "depend2"
    )
  )
  res <- runner$submit_workflow(multiple_deps)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    tasks <- runner$queue$task_list()
    expect_length(tasks, 3)
  })
  expect_equal(names(res), c("workflow_key", "reports"))
  expect_true(!is.null(res$workflow_key))
  redis_key <- workflow_redis_key(runner$queue$queue_id, res$workflow_key)
  expect_setequal(runner$con$SMEMBERS(redis_key), res$reports)
  expect_length(res$reports, 3)
  task_ids <- vcapply(res$reports, function(id) get_task_id_key(runner, id))
  expect_setequal(tasks, task_ids)

  ## Order of returned tasks is correct
  result_1 <- runner$queue$task_wait(task_ids[[1]])
  status_1 <- runner$status(res$reports[[1]], output = TRUE)
  expect_equal(status_1$status, "success")
  expect_match(status_1$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_match(status_1$output, "\\[ name +\\]  example",
               all = FALSE)

  result_2 <- runner$queue$task_wait(task_ids[[2]])
  status_2 <- runner$status(res$reports[[2]], output = TRUE)
  expect_equal(status_2$status, "success")
  expect_match(status_2$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_match(status_2$output, "\\[ name +\\]  depend4",
               all = FALSE)

  result_3 <- runner$queue$task_wait(task_ids[[3]])
  status_3 <- runner$status(res$reports[[3]], output = TRUE)
  expect_equal(status_3$status, "success")
  expect_match(status_3$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_match(status_3$output, "\\[ name +\\]  depend2",
               all = FALSE)

  ## depend2 run used artefacts from example run
  expect_match(status_3$output,
               paste0("\\[ depends +\\]  example@", status_1$version),
               all = FALSE)

  ## depend4 run using artefacts from example & depend2 runs
  expect_match(status_2$output,
               paste0("\\[ depends +\\]  example@", status_1$version),
               all = FALSE)
  expect_match(status_2$output,
               paste0("\\[ ... +\\]  depend2@", status_3$version),
               all = FALSE)
})

test_that("dependencies are set correctly", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  mock_submit <- mockery::mock("1", "2", "3", cycle = TRUE)
  mock_orderly_runner <- R6::R6Class(
    "mock_orderly_runner",
    inherit = orderly_runner_,
    public = list(
      submit = function(expr, depends_on) {
        mock_submit(expr, depends_on = depends_on)
      }
    ))
  runner <- mock_orderly_runner$new(path, allow_ref = NULL,
                                    queue_id = NULL, workers = 0)

  multiple_deps <- list(
    list(
      name = "example"
    ),
    list(
      name = "depend4"
    ),
    list(
      name = "depend2"
    )
  )
  res <- runner$submit_workflow(multiple_deps)
  args <- mockery::mock_args(mock_submit)
  expect_length(args, 3)
  expect_null(args[[1]]$depends_on)
  expect_equivalent(args[[2]]$depends_on, "1")
  expect_equivalent(args[[3]]$depends_on, c("1", "2"))
})

test_that("dependencies are resolved using git ref", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  mock_submit <- mockery::mock("1", "2", "3", cycle = TRUE)
  mock_orderly_runner <- R6::R6Class(
    "mock_orderly_runner",
    inherit = orderly_runner_,
    public = list(
      submit = function(expr, depends_on) {
        mock_submit(expr, depends_on = depends_on)
      }
    ))
  runner <- mock_orderly_runner$new(path, allow_ref = NULL,
                                    queue_id = NULL, workers = 0)

  ## Remove dependencies on branch
  prev <- git_checkout_branch("test", root = runner$root, create = TRUE)
  depend4_path <- file.path(runner$root,  "src/depend4/orderly.yml")
  writeLines(c("script: script.R",
               "artefacts:",
               "  staticgraph:",
               "    description: A graph of things",
               "    filenames: mygraph.png"),
             depend4_path)
  gert::git_add(".", repo = runner$root)
  gert::git_commit("Remove dependencies", repo = runner$root,
                   author = "Test User <test.user@example.com>")
  git_checkout_branch(prev, root = runner$root)

  multiple_deps <- list(
    list(
      name = "example"
    ),
    list(
      name = "depend4"
    ),
    list(
      name = "depend2"
    )
  )
  res <- runner$submit_workflow(multiple_deps, ref = "test")
  args <- mockery::mock_args(mock_submit)
  expect_length(args, 3)
  expect_null(args[[1]]$depends_on)
  expect_equal(args[[1]][[1]]$ref, "test")
  expect_equal(args[[1]][[1]]$name, "example")
  expect_null(args[[2]]$depends_on)
  expect_equal(args[[2]][[1]]$ref, "test")
  expect_equal(args[[2]][[1]]$name, "depend4")
  expect_equivalent(args[[3]]$depends_on, "1")
  expect_equal(args[[3]][[1]]$ref, "test")
  expect_equal(args[[3]][[1]]$name, "depend2")
})

test_that("workflow status can be calcualted", {
  expect_equal(workflow_combine_status("success"), "success")
  expect_equal(workflow_combine_status("running"), "running")
  expect_equal(workflow_combine_status("queued"), "queued")
  expect_equal(workflow_combine_status("error"), "error")
  report_status <- c("success", "success")
  expect_equal(workflow_combine_status(report_status), "success")
  report_status <- c("success", "running")
  expect_equal(workflow_combine_status(report_status), "running")
  report_status <- c("queued", "running")
  expect_equal(workflow_combine_status(report_status), "running")
  report_status <- c("queued", "deferred")
  expect_equal(workflow_combine_status(report_status), "queued")
  report_status <- c("error", "success")
  expect_equal(workflow_combine_status(report_status), "error")
  report_status <- c("queued", "cancelled")
  expect_equal(workflow_combine_status(report_status), "cancelled")
})

test_that("can get status of a worfklow", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- orderly_runner(path)

  multiple_deps <- list(
    list(
      name = "example"
    ),
    list(
      name = "depend4"
    ),
    list(
      name = "depend2"
    )
  )
  res <- runner$submit_workflow(multiple_deps)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    tasks <- runner$queue$task_list()
    expect_length(tasks, 3)
  })
  results <- lapply(tasks, runner$queue$task_wait)
  status <- runner$workflow_status(res$workflow_key)

  expect_equal(names(status), c("workflow_key", "status", "reports"))
  expect_equal(status$workflow_key, res$workflow_key)
  expect_equal(status$status, "success")
  expect_length(status$reports, 3)
  report_keys <- lapply(status$reports, "[[", "key")
  expect_setequal(report_keys, res$reports)
  expect_equal(status$reports[[1]]$status, "success")
  expect_match(status$reports[[1]]$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_null(status$reports[[1]]$output)
  expect_equal(status$reports[[1]]$queue, list())
  expect_equal(status$reports[[2]]$status, "success")
  expect_match(status$reports[[2]]$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_null(status$reports[[2]]$output)
  expect_equal(status$reports[[2]]$queue, list())
  expect_equal(status$reports[[3]]$status, "success")
  expect_match(status$reports[[3]]$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_null(status$reports[[3]]$output)
  expect_equal(status$reports[[3]]$queue, list())

  ## Output can be included
  status <- runner$workflow_status(res$workflow_key, output = TRUE)
  expect_length(status$reports, 3)
  expect_match(status$reports[[1]]$output, "\\[ success +\\]  :)",
               all = FALSE)
  expect_match(status$reports[[2]]$output, "\\[ success +\\]  :)",
               all = FALSE)
  expect_match(status$reports[[3]]$output, "\\[ success +\\]  :)",
               all = FALSE)
})
