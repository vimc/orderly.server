context("workflows")

test_that("workflow summary errors if report not found", {
  path <- orderly_prepare_orderly_example("minimal", git = TRUE)
  reports <- list(
    list(name = "missing_report")
  )
  expect_error(
    workflow_summary(path, reports),
    "[pP]ath 'src/missing_report/orderly.yml' does not exist in 'HEAD'")
})

test_that("can get summary of a workflow", {
  path <- orderly_prepare_orderly_example("demo", git = TRUE)
  reports <- list(
    list(name = "other")
  )
  expect_equal(workflow_summary(path, reports),
               list(reports = list(list(name = "other")),
                    ref = NULL,
                    missing_dependencies = list(
                      other = list()
                    )))

  reports <- list(
    list(name = "use_dependency")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "use_dependency")
                 ),
                 ref = NULL,
                 missing_dependencies = list(
                   use_dependency = list("other")
                 )))

  reports <- list(
    list(name = "use_dependency"),
    list(name = "other")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "other"),
                   list(name = "use_dependency",
                        depends_on = "other")),
                 ref = NULL,
                 missing_dependencies = list(
                   use_dependency = list(),
                   other = list()
                 )))
})

test_that("workflow summary can handle multiple instances of the same report", {
  path <- orderly_prepare_orderly_example("demo", git = TRUE)
  reports <- list(
    list(name = "other", params = list(
      nmin = 0.5
    )),
    list(name = "global"),
    list(name = "other", params = list(
      nmin = 1
    )),
    list(name = "connection")
  )

  # original order here is considered to be "other", "global", "connection"
  # the second "other" report is grouped with the first
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "other",
                        params = list(
                          nmin = 0.5
                        )),
                   list(name = "other",
                        params = list(
                          nmin = 1
                        )),
                   list(name = "global"),
                   list(name = "connection")
                 ),
                 ref = NULL,
                 missing_dependencies = list(other = list(),
                                             global = list(),
                                             other = list(),
                                             connection = list())))
})

test_that("workflow summary can handle mutiple instances and dependencies", {
  path <- orderly_prepare_orderly_example("demo", git = TRUE)
  reports <- list(
    list(name = "other", params = list(
      nmin = 0.5
    )),
    list(name = "global"),
    list(name = "use_dependency_2",
         params = list(nmin = 0.5)),
    list(name = "other", params = list(
      nmin = 1
    )),
    list(name = "use_dependency",
         params = list(nmin = 0.5)),
    list(name = "use_dependency_2",
         params = list(nmin = 2)),
    list(name = "use_dependency",
         params = list(nmin = 2)),
    list(name = "connection")
  )

  # original order here is considered to be
  # "other", "global", "use_dependency_2", "use_dependency", "connection"
  # the second "other" report is grouped with the first
  # and the second "use_dependency_2" is grouped with the first
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "other",
                        params = list(
                          nmin = 0.5
                        )),
                   list(name = "other",
                        params = list(
                          nmin = 1
                        )),
                   list(name = "global"),
                   list(name = "use_dependency",
                        params = list(
                          nmin = 0.5
                        ),
                        depends_on = "other"),
                   list(name = "use_dependency",
                        params = list(
                          nmin = 2
                        ),
                        depends_on = "other"),
                   list(name = "use_dependency_2",
                        params = list(
                          nmin = 0.5
                        ),
                        depends_on = "use_dependency"),
                   list(name = "use_dependency_2",
                        params = list(
                          nmin = 2
                        ),
                        depends_on = "use_dependency"),
                   list(name = "connection")
                 ),
                 ref = NULL,
                 missing_dependencies = list(other = list(),
                                             global = list(),
                                             use_dependency_2 = list(),
                                             other = list(),
                                             use_dependency = list(),
                                             use_dependency_2 = list(),
                                             use_dependency = list(),
                                             connection = list())))
})

test_that("workflow summary only looks at depth 1 dependencies", {
  path <- orderly_prepare_orderly_example("demo", git = TRUE)
  reports <- list(
    list(name = "use_dependency_2")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "use_dependency_2")
                 ),
                 ref = NULL,
                 missing_dependencies = list(
                   use_dependency_2 = list("use_dependency")
                 )))
})

test_that("workflow summary can handle multiple dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)

  reports <- list(
    list(name = "depend4")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "depend4")
                 ),
                 ref = NULL,
                 missing_dependencies = list(
                   depend4 = list("example", "depend2"))
               ))

  reports <- list(
    list(name = "depend4"),
    list(name = "depend2")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "depend2"),
                   list(name = "depend4",
                        depends_on = "depend2")
                 ),
                 ref = NULL,
                 missing_dependencies = list(
                   depend4 = list("example"),
                   depend2 = list("example"))
               ))

  reports <- list(
    list(name = "depend4"),
    list(name = "depend2"),
    list(name = "example")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "example"),
                   list(name = "depend2",
                        depends_on = "example"),
                   list(name = "depend4",
                        depends_on = c("depend2", "example"))
                 ),
                 ref = NULL,
                 missing_dependencies = list(
                   depend4 = list(),
                   depend2 = list(),
                   example = list())
               ))


  reports <- list(
    list(name = "depend2")
  )
  expect_equal(workflow_summary(path, reports),
               list(
                 reports = list(
                   list(name = "depend2")
                 ),
                 ref = NULL,
                 missing_dependencies = list(
                   depend2 = list("example")
                 )))
})

test_that("workflow summary returns ref, instance and parameters", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)
  commits <- git_commits(branch = "master", root = path)

  reports <- list(
    list(name = "depend4",
         instance = "production",
         params = list(nmin = 0.5, another_param = "test"))
  )
  expect_equal(workflow_summary(path, reports, commits$id),
               list(
                 reports = list(
                   list(name = "depend4",
                        instance = "production",
                        params = list(
                          nmin = 0.5,
                          another_param = "test"
                        ))
                 ),
                 ref = commits$id,
                 missing_dependencies = list(
                   depend4 = list("example", "depend2"))
               ))
})

test_that("can work out dependencies graph", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)

  no_deps_names <- "depend"
  no_deps_dependencies <- orderly_upstream_dependencies(no_deps_names,
                                                        root = path)
  expect_equal(workflow_dependencies(no_deps_names, no_deps_dependencies),
               list(depend = NA))

  one_dep_names <- c("example", "depend")
  one_dep_dependencies <- orderly_upstream_dependencies(one_dep_names,
                                                        root = path)
  expect_equal(workflow_dependencies(one_dep_names, one_dep_dependencies),
               list(example = NA, depend = "example"))

  two_deps_names <- c("example", "depend", "depend2")
  two_deps_dependencies <- orderly_upstream_dependencies(two_deps_names,
                                                         root = path)
  expect_equal(workflow_dependencies(two_deps_names, two_deps_dependencies),
               list(example = NA,
                    depend = "example",
                    depend2 = "example"))

  multiple_deps_names <- c("example", "depend2", "depend4")
  multiple_deps_dependencies <- orderly_upstream_dependencies(
    multiple_deps_names, root = path)
  expect_equal(workflow_dependencies(multiple_deps_names,
                                     multiple_deps_dependencies),
               list(example = NA,
                    depend2 = "example",
                    depend4 = c("example", "depend2")))
})

test_that("can topological sort", {
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
               c("b", "i", "j", "k", "e", "h", "g", "c"))
})

test_that("sort preserves original order if no dependencies", {
  graph <- list(b = NA,
                e = NA,
                f = NA)
  sort <- topological_sort(graph)
  expect_equal(sort,
               c("b", "e", "f"))

  graph <- list(b = c("e", "k"),
                e = NA,
                f = NA,
                k = NA)
  sort <- topological_sort(graph)
  expect_equal(sort,
               c("e", "k", "b", "f"))

  graph <- list(b = NA,
                e = NA,
                f = "k",
                k = NA,
                g = NA)
  sort <- topological_sort(graph)
  expect_equal(sort,
               c("b", "e", "k", "f", "g"))
})

test_that("sort preserves original order if valid", {
  graph <- list(b = NA,
                e = "b",
                k = c("e", "b"))
  sort <- topological_sort(graph)
  expect_equal(sort,
               c("b", "e", "k"))
})

test_that("cycled can be detected", {

  topological_sort(list(a = NA, b = list(a = "a", c = "c"), c = list(a = "a")))

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
  depend4_path <- file.path(runner$root, "src/depend4/orderly.yml")
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

test_that("can get upstream dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)

  deps <- orderly_upstream_dependencies(c("example", "depend", "depend2"),
                                        root = path)
  expect_equal(deps, list(
    example = NULL,
    depend = "example",
    depend2 = "example"
  ))
})

test_that("orderly workflow works with running a report twice", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE, git = TRUE)
  commits <- git_commits(branch = "master", root = path)

  reports <- list(
    list(name = "depend4",
         params = list(nmin = 0.5, another_param = "test")),
    list(name = "depend4",
         params = list(nmin = 0.5, another_param = "test")),
    list(name = "depend4",
         params = list(nmin = 1, another_param = "test"))
  )

  summary <- workflow_summary(path, reports, "master")
  expect_equal(summary$reports, reports)
  expect_equal(summary$ref, "master")
  expect_equal(summary$missing_dependencies, list(
    depend4 = list("example", "depend2"),
    depend4 = list("example", "depend2"),
    depend4 = list("example", "depend2")
  ))
})

test_that("workflow can be run: dependencies are cancelled on error", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- orderly_runner(path)

  ## We have in this setup
  ##        example
  ##        /  |   \
  ##  depend   |    depend2 (this errors)
  ##           |   /
  ##        depend4
  ## so if depend2 errors we should see depend4 cancel
  ## but depend still run
  err <- "stop('this report fails!')"
  writeLines(err, file.path(path, "src/depend2/script.R"))
  multiple_deps <- list(
    list(
      name = "example"
    ),
    list(
      name = "depend4"
    ),
    list(
      name = "depend2"
    ),
    list(
      name = "depend"
    )
  )
  res <- runner$submit_workflow(multiple_deps)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    tasks <- runner$queue$task_list()
    expect_length(tasks, 4)
  })
  expect_equal(names(res), c("workflow_key", "reports"))
  expect_true(!is.null(res$workflow_key))
  redis_key <- workflow_redis_key(runner$queue$queue_id, res$workflow_key)
  expect_setequal(runner$con$SMEMBERS(redis_key), res$reports)
  expect_length(res$reports, 4)
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
  expect_equal(status_2$status, "impossible")

  result_3 <- runner$queue$task_wait(task_ids[[3]])
  status_3 <- runner$status(res$reports[[3]], output = TRUE)
  expect_equal(status_3$status, "error")
  expect_match(status_3$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_match(status_3$output, "\\[ name +\\]  depend2",
               all = FALSE)

  result_4 <- runner$queue$task_wait(task_ids[[4]])
  status_4 <- runner$status(res$reports[[4]], output = TRUE)
  expect_equal(status_4$status, "success")
  expect_match(status_4$version, "^\\d{8}-\\d{6}-\\w{8}")
  expect_match(status_4$output, "\\[ name +\\]  depend",
               all = FALSE)
})
