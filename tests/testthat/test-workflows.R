context("workflows")


test_that("workflow missing dependencies errors if report not found", {
  path <- orderly_prepare_orderly_example("minimal")
  tasks <- list(
    missing_task = list(
      ref = "123"
    )
  )
  expect_error(workflow_missing_dependencies(path, tasks),
               "Task with name 'missing_task' cannot be found.")
})

test_that("can get missing dependencies of a workflow", {
  path <- orderly_prepare_orderly_example("demo")
  tasks <- list(
    other = list(
      ref = "123"
    )
  )
  expect_equal(workflow_missing_dependencies(path, tasks),
               list(missing_dependencies = list(
                 other = list()
               )))

  tasks <- list(
    use_dependency = list(
      ref = "123"
    )
  )
  expect_equal(workflow_missing_dependencies(path, tasks),
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
  expect_equal(workflow_missing_dependencies(path, tasks),
               list(missing_dependencies = list(
                 use_dependency = list(),
                 other = list()
               )))
})

test_that("workflow missing dependencies only looks at depth 1 dependencies", {
  path <- orderly_prepare_orderly_example("demo")
  tasks <- list(
    use_dependency_2 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_missing_dependencies(path, tasks),
               list(missing_dependencies = list(
                 use_dependency_2 = list(scalar("use_dependency"))
               )))
})

test_that("workflow missing dependencies can handle multiple dependencies", {
  path <- orderly_prepare_orderly_example("depends", testing = TRUE)

  tasks <- list(
    depend4 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_missing_dependencies(path, tasks),
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
  expect_equal(workflow_missing_dependencies(path, tasks),
               list(missing_dependencies = list(
                 depend4 = list(scalar("example")),
                 depend2 =  list(scalar("example")))
               ))


  tasks <- list(
    depend2 = list(
      ref = "123"
    )
  )
  expect_equal(workflow_missing_dependencies(path, tasks),
               list(missing_dependencies = list(
                 depend2 = list(scalar("example"))
               )))
})

test_that("can work out dependencies graph", {
  no_deps <- list(
    depend = list(
      ref = "123",
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
      ref = "123",
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    depend = list(
      ref = "123",
      instance = list(
        source = "production"
      )
    )
  )
  two_deps <- list(
    example = list(
      ref = "123",
      instance = list(
        source = "production"
      ),
      params = list(
        nmin = 0.5,
        nmax = 1
      )
    ),
    depend = list(
      ref = "123",
      instance = list(
        source = "production"
      )
    ),
    depend2 = list(
      ref = "123",
      instance = list(
        source = "production"
      )
    )
  )
  multiple_deps <- list(
    example = list(
      ref = "123",
      instance = list(
        source = "production"
      )
    ),
    depend2 = list(
      ref = "123",
      instance = list(
        source = "production"
      )
    ),
    depend4 = list(
      ref = "123",
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
