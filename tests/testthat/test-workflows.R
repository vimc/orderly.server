context("workflows")


test_that("workflow validate erros if report not found", {
  path <- orderly_prepare_orderly_example("minimal")
  tasks <- list(
    missing_task = list(
      ref = "123"
    )
  )
  expect_error(workflow_validate(path, tasks),
               "Task with name 'missing_task' cannot be found.")
})

test_that("workflow can be validated", {
  path <- orderly_prepare_orderly_example("demo")
  tasks <- list(
    other = list(
      ref = "123"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 other = list()
               )))

  tasks <- list(
    use_dependency = list(
      ref = "123"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 use_dependency = list(scalar("other"))
               )))

  tasks <- list(
    use_dependency = list(
      ref = "123"
    ),
    other = list(
      ref = "abc"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 use_dependency = list(),
                 other = list()
               )))
})

test_that("workflow validate only looks at depth 1 dependencies", {
  path <- orderly_prepare_orderly_example("demo")
  tasks <- list(
    use_dependency_2 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 use_dependency_2 = list(scalar("use_dependency"))
               )))
})

test_that("workflow validate can handle multiple dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE)

  tasks <- list(
    depend4 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example"), scalar("depend2")))
               ))

  tasks <- list(
    depend4 = list(
      ref = "123"
    ),
    depend2 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example")),
                 depend2 =  list(scalar("example")))
               ))


  tasks <- list(
    depend2 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_validate(path, tasks),
               list(missing_dependencies = list(
                 depend2 = list(scalar("example"))
               )))
})
