context("workflows")


test_that("workflow missing dependencies errors if report not found", {
  path <- orderly_prepare_orderly_example("minimal")
  reports <- list(
    list(name = "missing_report")
  )
  expect_error(workflow_missing_dependencies(path, reports),
               "Report with name 'missing_report' cannot be found.")
})

test_that("can get missing dependencies of a workflow", {
  path <- orderly_prepare_orderly_example("demo")
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
  path <- orderly_prepare_orderly_example("demo")
  reports <- list(
    list(name = "use_dependency_2")
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 use_dependency_2 = list(scalar("use_dependency"))
               )))
})

test_that("workflow missing dependencies can handle multiple dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE)

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
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    )
  )
  one_dep <- list(
    list(
      name = "example",
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    list(
      name = "depend",
      instance = list(
        source = "production"
      )
    )
  )
  two_deps <- list(
   list(
      name = "example",
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    list(
      name = "depend",
      instance = list(
        source = "production"
      )
    ),
    list(
      name = "depend2",
      instance = list(
        source = "production"
      )
    )
  )
  multiple_deps <- list(
    list(
      name = "example",
      instance = list(
        source = "production"
      )
    ),
    list(
      name = "depend2",
      instance = list(
        source = "production"
      )
    ),
    list(
      name = "depend4",
      instance = list(
        source = "production"
      )
    )
  )
  path <- orderly_prepare_orderly_example("depends", testing = TRUE)
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
  path <- orderly_prepare_orderly_example("depends", testing = TRUE)
  no_deps <- list(
    list(
      name = "depend",
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    )
  )
  workflow <- build_workflow(path, path, no_deps, "key-report-id")
  expect_length(workflow, 1)
  expect_equal(names(workflow[[1]]), c("expr", "envir", "name",
                                       "key", "depends_on", "original_order"))
  expect_type(workflow[[1]]$expr, "language")
  required_params <- c("key_report_id", "key", "root", "name", "parameters",
                       "instance", "ref", "changelog", "poll")
  expect_true(all(required_params %in% ls(workflow[[1]]$envir)))
  expect_equal(get("name", workflow[[1]]$envir), "depend")
  expect_null(workflow[[1]]$depends_on)
  expect_true(!is.null(workflow[[1]]$key))
  expect_equal(workflow[[1]]$original_order, 1)

  multiple_deps <- list(
    list(
      name = "example",
      instance = list(
        source = "production"
      )
    ),
    list(
      name = "depend4",
      instance = list(
        source = "production"
      )
    ),
    list(
      name = "depend2",
      instance = list(
        source = "production"
      )
    )
  )
  workflow <- build_workflow(path, path, multiple_deps, "key-report-id")
  expect_length(workflow, 3)

  expect_equal(names(workflow[[1]]), c("expr", "envir", "name",
                                       "key", "depends_on", "original_order"))
  expect_type(workflow[[1]]$expr, "language")
  expect_true(all(required_params %in% ls(workflow[[1]]$envir)))
  expect_equal(get("name", workflow[[1]]$envir), "example")
  expect_null(workflow[[1]]$depends_on)
  expect_true(!is.null(workflow[[1]]$key))
  expect_equal(workflow[[1]]$original_order, 1)

  expect_equal(names(workflow[[2]]), c("expr", "envir", "name",
                                       "key", "depends_on", "original_order"))
  expect_type(workflow[[2]]$expr, "language")
  expect_true(all(required_params %in% ls(workflow[[2]]$envir)))
  expect_equal(get("name", workflow[[2]]$envir), "depend2")
  expect_equal(workflow[[2]]$depends_on, "example")
  expect_true(!is.null(workflow[[2]]$key))
  expect_equal(workflow[[2]]$original_order, 3)

  expect_equal(names(workflow[[3]]), c("expr", "envir", "name",
                                       "key", "depends_on", "original_order"))
  expect_type(workflow[[3]]$expr, "language")
  expect_true(all(required_params %in% ls(workflow[[3]]$envir)))
  expect_equal(get("name", workflow[[3]]$envir), "depend4")
  expect_equal(workflow[[3]]$depends_on, c("example", "depend2"))
  expect_true(!is.null(workflow[[3]]$key))
  expect_equal(workflow[[3]]$original_order, 2)
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
  expect_equal(runner$con$SMEMBERS(redis_key), list(tasks))
  expect_length(res$reports, 1)
  task_id <- get_task_id_key(runner, res$reports)
  expect_equal(tasks, task_id)

  ## TODO: Test it has actually run successfully
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
  expect_equal(runner$con$SMEMBERS(redis_key), as.list(tasks))
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
      submit = function(job, environment, depends_on) {
        mock_submit(job, environment, depends_on = depends_on)
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
  expect_equal(args[[2]]$depends_on, c(example = "1"))
  expect_equal(args[[3]]$depends_on, c(example = "1", depend2 = "2"))
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
      submit = function(job, environment, depends_on) {
        mock_submit(job, environment, depends_on = depends_on)
      }
    ))
  runner <- mock_orderly_runner$new(path, allow_ref = NULL,
                                    queue_id = NULL, workers = 0)

  ## Remove dependencies on branch
  prev <- git_checkout_branch("test", root = runner$temp_root, create = TRUE)
  depend4_path <- file.path(runner$temp_root,  "src/depend4/orderly.yml")
  writeLines(c("script: script.R",
               "artefacts:",
               "  staticgraph:",
               "    description: A graph of things",
               "    filenames: mygraph.png"),
             depend4_path)
  gert::git_add(".", repo = runner$temp_root)
  gert::git_commit("Remove dependencies", repo = runner$temp_root)
  git_checkout_branch(prev, root = runner$temp_root)

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
  expect_equal(get("ref", args[[1]][[2]]), "test")
  expect_equal(get("name", args[[1]][[2]]), "example")
  expect_null(args[[2]]$depends_on)
  expect_equal(get("ref", args[[2]][[2]]), "test")
  expect_equal(get("name", args[[2]][[2]]), "depend4")
  expect_equal(args[[3]]$depends_on, c(example = "1"))
  expect_equal(get("ref", args[[3]][[2]]), "test")
  expect_equal(get("name", args[[3]][[2]]), "depend2")

  ## Git branch has been restored
  expect_equal(git_branch_name(root = runner$root), "master")
  expect_equal(git_branch_name(root = runner$temp_root), "master")
})
