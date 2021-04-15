context("workflows")


test_that("workflow missing dependencies errors if report not found", {
  path <- orderly_prepare_orderly_example("minimal")
  reports <- list(
    missing_report = list()
  )
  expect_error(workflow_missing_dependencies(path, reports),
               "Report with name 'missing_report' cannot be found.")
})

test_that("can get missing dependencies of a workflow", {
  path <- orderly_prepare_orderly_example("demo")
  reports <- list(
    other = list()
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 other = list()
               )))

  reports <- list(
    use_dependency = list()
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 use_dependency = list(scalar("other"))
               )))

  reports <- list(
    use_dependency = list(),
    other = list()
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
    use_dependency_2 = list()
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 use_dependency_2 = list(scalar("use_dependency"))
               )))
})

test_that("workflow missing dependencies can handle multiple dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE)

  reports <- list(
    depend4 = list()
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example"), scalar("depend2")))
               ))

  reports <- list(
    depend4 = list(),
    depend2 = list()
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example")),
                 depend2 =  list(scalar("example")))
               ))


  reports <- list(
    depend2 = list()
  )
  expect_equal(workflow_missing_dependencies(path, reports),
               list(missing_dependencies = list(
                 depend2 = list(scalar("example"))
               )))
})

test_that("can work out dependencies graph", {
  no_deps <- list(
    depend = list(
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
    example = list(
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    depend = list(
      instance = list(
        source = "production"
      )
    )
  )
  two_deps <- list(
    example = list(
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    depend = list(
      instance = list(
        source = "production"
      )
    ),
    depend2 = list(
      instance = list(
        source = "production"
      )
    )
  )
  multiple_deps <- list(
    example = list(
      instance = list(
        source = "production"
      )
    ),
    depend2 = list(
      instance = list(
        source = "production"
      )
    ),
    depend4 = list(
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
    depend = list(
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    )
  )
  workflow <- build_workflow(path, no_deps, "key-report-id")
  expect_equal(names(workflow), "depend")
  expect_equal(names(workflow$depend), c("expr", "envir", "key", "depends_on"))
  expect_type(workflow$depend$expr, "language")
  required_params <- c("key_report_id", "key", "root", "name", "parameters",
                       "instance", "ref", "changelog", "poll")
  expect_true(all(required_params %in% ls(workflow$depend$envir)))
  expect_null(workflow$depend$depends_on)
  expect_true(!is.null(workflow$depend$key))

  multiple_deps <- list(
    example = list(
      instance = list(
        source = "production"
      )
    ),
    depend4 = list(
      instance = list(
        source = "production"
      )
    ),
    depend2 = list(
      instance = list(
        source = "production"
      )
    )
  )
  workflow <- build_workflow(path, multiple_deps, "key-report-id")
  expect_equal(names(workflow), c("example", "depend2", "depend4"))

  expect_equal(names(workflow$example), c("expr", "envir", "key", "depends_on"))
  expect_type(workflow$example$expr, "language")
  expect_true(all(required_params %in% ls(workflow$example$envir)))
  expect_null(workflow$example$depends_on)
  expect_true(!is.null(workflow$example$key))

  expect_equal(names(workflow$depend2), c("expr", "envir", "key", "depends_on"))
  expect_type(workflow$depend2$expr, "language")
  expect_true(all(required_params %in% ls(workflow$depend2$envir)))
  expect_equal(workflow$depend2$depends_on, "example")
  expect_true(!is.null(workflow$depend2$key))

  expect_equal(names(workflow$depend4), c("expr", "envir", "key", "depends_on"))
  expect_type(workflow$depend4$expr, "language")
  expect_true(all(required_params %in% ls(workflow$depend4$envir)))
  expect_equal(workflow$depend4$depends_on, c("example", "depend2"))
  expect_true(!is.null(workflow$depend4$key))
})

test_that("workflow can be run: simple", {
  testthat::skip_on_cran()
  skip_on_windows()
  skip_if_no_redis()
  path <- orderly_git_example("depends", testing = TRUE)
  runner <- orderly_runner(path)

  no_deps <- list(
    depend = list(
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    )
  )
  workflow_key <- runner$submit_workflow(no_deps)
  testthat::try_again(5, {
    Sys.sleep(0.5)
    tasks <- runner$queue$task_list()
    expect_length(tasks, 1)
  })
  redis_key <- workflow_redis_key(runner$queue$queue_id, workflow_key)
  expect_equal(runner$con$SMEMBERS(redis_key), list(tasks))

})
