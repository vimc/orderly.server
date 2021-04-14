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
