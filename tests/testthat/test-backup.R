context("backup")

test_that("backup is run when expected", {
  mock_backup <- mockery::mock(TRUE, cycle = TRUE)
  with_mock("orderly:::orderly_backup" = mock_backup, {
    msg <- capture_messages(backup <- orderly_backup("cfg", 30))
  })
  expect_match(msg, "Backing up orderly db")
  expect_equal(backup$backup_period, 30)
  first_backup_time <- backup$last_backup
  expect_s3_class(first_backup_time, "POSIXct")
  mockery::expect_called(mock_backup, 1)
  mockery::expect_args(mock_backup, 1, backup$config)

  with_mock("orderly:::orderly_backup" = mock_backup, {
    msg <- capture_messages(backup$check_backup())
  })
  ## Backup period has not passed
  expect_equal(msg, character(0))
  expect_equal(backup$last_backup, first_backup_time)
  mockery::expect_called(mock_backup, 1)

  backup$backup_period <- 0
  Sys.sleep(1)  ## Ensure 1 second has passed
  with_mock("orderly:::orderly_backup" = mock_backup, {
    msg <- capture_messages(backup$check_backup())
  })
  ## Backup period has passed
  expect_match(msg, "Backing up orderly db")
  expect_true(backup$last_backup > first_backup_time)
  mockery::expect_called(mock_backup, 2)
  mockery::expect_args(mock_backup, 2, backup$config)
})
