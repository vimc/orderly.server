context("dependencies")

test_that("can get_dependencies", {
  mock_get_direction <- function() {
    "downstream"
  }
  mock_graph <- mockery::mock(list(
      get_direction = mock_get_direction,
      root = list(
        name = "r1",
        id = "1",
        out_of_date = FALSE,
        children = list(
          list(
            name = "r2",
            id = "2",
            out_of_date = TRUE,
            children = list()
          ),
          list(
            name = "r3",
            id = "3",
            out_of_date = FALSE,
            children = list(
              list(
                name = "r4",
                id = "4",
                out_of_date = TRUE,
                children = list()
              )
            )
          )
        )
      )
    )
  )
  with_mock("orderly::orderly_graph" = mock_graph, {
    res <- get_dependencies(path = "test_path",
                            name = "test_name",
                            id = "test_id",
                            direction = "test_direction",
                            propagate = FALSE,
                            max_depth = 10,
                            show_all = FALSE,
                            use = "test_use")
  })
  mockery::expect_called(mock_graph, 1)
  mockery::expect_args(mock_graph, 1,
                       name = "test_name", 
                       id = "test_id",
                       root = "test_path",
                       locate = FALSE,
                       direction = "test_direction",
                       propagage = FALSE,
                       max_depth = 10,
                       show_all = FALSE,
                       use = "test_use")
  expect_equal(res$direction, scalar("downstream"))
  r1 <- res$dependency_tree
  expect_equal(r1$name, scalar("r1"))
  expect_equal(r1$id, scalar("1"))
  expect_equal(r1$out_of_date, scalar(FALSE))
  expect_equal(length(r1$dependencies), 2)
  r2 <- r1$dependencies[[1]]
  expect_equal(r2$name, scalar("r2"))
  expect_equal(r2$id, scalar("2"))
  expect_equal(r2$out_of_date, scalar(TRUE))
  expect_equal(length(r2$dependencies), 0)
  r3 <- r1$dependencies[[2]]
  expect_equal(r3$name, scalar("r3"))
  expect_equal(r3$id, scalar("3"))
  expect_equal(r3$out_of_date, scalar(FALSE))
  expect_equal(length(r3$dependencies), 1)
  r4 <- r3$dependencies[[1]]
  expect_equal(r4$name, scalar("r4"))
  expect_equal(r4$id, scalar("4"))
  expect_equal(r4$out_of_date, scalar(TRUE))
  expect_equal(length(r4$dependencies), 0)
})