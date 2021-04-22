context("git")

test_that("status", {
  testthat::skip_on_cran()
  path <- orderly_unzip_git_demo()
  expect_equal(git_status(path),
               list(success = TRUE, code = 0,
                    output = character(0), clean = TRUE))
  expect_true(git_is_clean(path))

  writeLines("hello", file.path(path, "hello"))
  expect_equal(git_status(path),
               list(success = TRUE, code = 0,
                    output = "?? hello", clean = FALSE))
  expect_false(git_is_clean(path))
})

test_that("branches", {
  testthat::skip_on_cran()
  path <- orderly_unzip_git_demo()
  sha1 <- git_ref_to_sha("master", path)
  sha2 <- git_ref_to_sha("other", path)

  expect_match(sha1, "^[[:xdigit:]]{40}$")
  expect_match(sha2, "^[[:xdigit:]]{40}$")
  expect_true(sha1 != sha2)

  expect_equal(git_ref_to_sha("HEAD", path), sha1)
  expect_equal(git_branch_name(path), "master")

  expect_identical(git_ref_to_sha("unknown", path),
                   NA_character_)
  expect_error(git_ref_to_sha("unknown", path, TRUE),
               "Git reference 'unknown' not found")
})



test_that("checkout_branch checks", {
  testthat::skip_on_cran()
  path <- orderly_unzip_git_demo()
  filename <- file.path(path, "dirty")
  file.create(filename)
  expect_error(git_checkout_branch("other", root = path),
               "working directory must be clean")
})


test_that("detect missing ref", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()
  path1 <- path[["origin"]]
  path2 <- path[["local"]]

  sha1 <- git_ref_to_sha("HEAD", path1)
  sha2 <- git_ref_to_sha("HEAD", path2)

  expect_true(git_ref_exists(sha1, path1))
  expect_true(git_ref_exists(sha2, path2))

  expect_true(git_ref_exists(sha2, path1))
  expect_false(git_ref_exists(sha1, path2))

  expect_true(git_ref_exists("master", path1))
  expect_true(git_ref_exists("master", path2))

  expect_false(git_ref_exists("origin/master", path1))
  expect_true(git_ref_exists("origin/master", path2))

  expect_false(git_ref_exists("origin/other", path1))
  expect_true(git_ref_exists("origin/other", path2))

  expect_true(git_ref_exists("other", path1))
  expect_true(git_ref_exists("other", path2))

  expect_false(git_ref_exists("foo", path1))
  expect_false(git_ref_exists("foo", path2))
})

test_that("handle failure", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()
  r <- git_run("unknown-command", root = path[["origin"]])
  expect_false(r$success)
  expect_error(
    git_run("unknown-command", root = path[["origin"]], check = TRUE),
    r$output, fixed = TRUE)
})


test_that("can get unmerged branches from git", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()
  ## Create another branch for testing
  prev <- git_checkout_branch("other", root = path[["local"]])
  prev <- git_checkout_branch("new-branch", root = path[["local"]],
                              create = TRUE)

  branches <- git_branches_no_merged(path[["local"]])
  expect_equal(nrow(branches), 1)
  expect_equal(colnames(branches), c("name", "last_commit", "last_commit_age"))
  ## Branch not on remote are not returned
  expect_equal(branches$name, "other")
  expect_match(branches$last_commit,
               "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
  expect_type(branches$last_commit_age, "integer")

  branches <- git_branches_no_merged(path[["local"]], include_master = TRUE)
  expect_equal(nrow(branches), 2)
  expect_equal(colnames(branches), c("name", "last_commit", "last_commit_age"))
  expect_equal(branches$name, c("master", "other"))
})


test_that("gh-pages branch is ignored in list of not merged branches", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()
  ## Create another branch for testing
  prev <- git_checkout_branch("other", root = path[["local"]])
  prev <- git_checkout_branch("gh-pages", root = path[["local"]], create = TRUE)

  branches <- git_branches_no_merged(path[["local"]])
  expect_equal(nrow(branches), 1)
  expect_true(!("gh-pages" %in% branches$name))
})

test_that("can get commit history for a branch", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()

  commits <- git_commits("master", path[["local"]])
  expect_equal(nrow(commits), 1)
  expect_equal(colnames(commits), c("id", "date_time", "age"))
  expect_type(commits$id, "character")
  expect_match(commits$date_time,
               "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
  expect_type(commits$age, "integer")

  other_commits <- git_commits("other", path[["local"]])
  expect_equal(nrow(other_commits), 1)
  expect_equal(colnames(other_commits), c("id", "date_time", "age"))
  expect_type(other_commits$id, "character")
  expect_match(other_commits$date_time,
               "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
  ## Commit from master branch is not returned
  expect_true(commits$id != other_commits$id)
})

test_that("can get report list from git", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()

  commits <- git_commits("master", path[["local"]])
  expect_equal(nrow(commits), 1)
  reports <- get_reports("master", commits$id, path[["local"]])
  expect_equal(reports, c("global", "minimal"))

  other_commits <- git_commits("other", path[["local"]])
  expect_equal(nrow(other_commits), 1)
  other_reports <- get_reports("other", other_commits$id, path[["local"]])
  expect_equal(other_reports, "other")

  ## git_commits works with NULL branch and commit
  default_reports <- get_reports(NULL, NULL, path[["local"]])
  expect_equal(default_reports, reports)
})

test_that("report only shows when pushed to remote", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()

  ## Make a change to other and test that it isn't listed in call to
  ## git_commits
  invisible(git_checkout_branch("other", root = path[["local"]]))
  dir.create(file.path(path[["local"]], "src/new-report"))
  invisible(file.copy(file.path(path[["local"]], "src/minimal"),
                      file.path(path[["local"]], "src/new-report"),
                      recursive = TRUE))
  invisible(git_run(c("add", "."), root = path[["local"]]))
  invisible(git_run(c("commit", "-m", "'add another report'"),
                    root = path[["local"]]))

  other_commits <- git_commits("other", path[["local"]])
  expect_equal(nrow(other_commits), 1)
  other_reports <- get_reports("other", other_commits$id, path[["local"]])
  expect_equal(other_reports, "other")

  ## Push to remote
  invisible(git_run(c("push", "--set-upstream origin other"),
                    root = path[["local"]]))

  other_commits <- git_commits("other", path[["local"]])
  expect_equal(nrow(other_commits), 2)
  other_reports <- get_reports("other", other_commits$id[[1]], path[["local"]])
  expect_equal(other_reports, c("new-report", "other"))
})

test_that("get_reports only shows one sided changes", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()

  ## At the moment we have
  ##
  ## master  (A)-(B)        #nolint
  ##          \
  ## other    (X)
  ## Where B is a commit without a report in it

  ## We want to test divergent branches where both branches contain a report
  ## which doesn't exist on the other branch.
  ## Create something like
  ## master  (A)-(B)-(C)    #nolint
  ##          \
  ## other    (X)
  ##
  ## Where commit C contains a report and test that when listing reports on
  ## commit X that the report added in commit C is not returned.

  ## Add a new commit to master branch containing a report
  invisible(dir.create(file.path(path[["origin"]], "src/new-report")))
  invisible(file.copy(file.path(path[["origin"]], "src/minimal"),
                      file.path(path[["origin"]], "src/new-report"),
                      recursive = TRUE))
  invisible(git_run(c("add", "."), root = path[["origin"]]))
  invisible(git_run(c("commit", "-m", "'add another report'"),
                    root = path[["origin"]]))
  invisible(git_fetch(path[["local"]]))

  commits <- git_commits("master", path[["local"]])
  expect_equal(nrow(commits), 3)
  reports <- get_reports("master", commits$id[[1]], path[["local"]])
  expect_equal(reports, c("global", "minimal", "new-report"))

  other_commits <- git_commits("other", path[["local"]])
  expect_equal(nrow(other_commits), 1)
  other_reports <- get_reports("other", other_commits$id, path[["local"]])
  expect_equal(other_reports, "other")
})

test_that("can get parameters from a report", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()

  ## Write some parameters lines to the upstream yml and commit it
  origin_yml <- file.path(path[["origin"]], "src", "minimal", "orderly.yml")
  yml <- readLines(origin_yml)
  text <- c(yml, c(
    "parameters:",
    "  a: ~",
    "  b:",
    "    default: test",
    "  c:",
    "    default: 2"
  ))
  writeLines(text, origin_yml)
  invisible(git_run(c("add", "."), root = path[["origin"]]))
  invisible(git_run(c("commit", "-m", "'add parameters'"),
                    root = path[["origin"]]))
  invisible(git_fetch(path[["local"]]))

  commits <- git_commits("master", path[["local"]])
  expect_equal(nrow(commits), 3)
  params <- get_report_parameters("minimal", commits$id[1], path[["local"]])
  expect_equal(params, list(
    a = NULL,
    b = list(
      default = "test"
    ),
    c = list(
      default = 2
    )
  ))
  params <- get_report_parameters("global", commits$id[1], path[["local"]])
  expect_equal(params, NULL)

  other_commits <- git_commits("other", path[["local"]])
  expect_equal(nrow(other_commits), 1)
  params <- get_report_parameters("other", other_commits$id, path[["local"]])
  expect_equal(params, list(nmin = NULL))

  ## get report parameters defaults to latest commit if NULL
  default_params <- get_report_parameters("minimal", NULL, path[["origin"]])
  expect_equal(default_params, list(a = NULL,
                                    b = list(default = "test"),
                                    c = list(default = 2)))
})

test_that("get_report_parameters handles errors", {
  expect_error(
    get_report_parameters("rep1", "commit1", "."),
    "Failed to get report parameters for report 'rep1' and commit 'commit1':")

  mockery::stub(get_report_parameters, "git_run", list(
    success = FALSE,
    code = 1,
    output = NULL
  ))
  expect_error(
    get_report_parameters("rep1", "commit1", "."),
    paste0(
      "Failed to get report parameters for report 'rep1' and commit 'commit1':",
      "\nNon zero exit code from git"))

  mockery::stub(get_report_parameters, "git_run", list(
    success = TRUE,
    code = 0,
    output = "[invalid_yml"
  ))
  expect_error(
    get_report_parameters("report1", "commit1", "."),
    paste0(
      "Failed to parse yml for report 'report1' and commit 'commit1':",
      "\nParser error: "))
})

test_that("git commits won't interpret ambiguous hash as number", {
  mock_log <- list(
    success = TRUE,
    code = 0,
    output = c("1234e56,1604998406")
  )
  mockery::stub(git_commits, "git_run", mock_log)
  commits <- git_commits("master")
  expect_type(commits$id, "character")
  expect_equal(commits$id, "1234e56")

  mock_log <- list(
    success = TRUE,
    code = 0,
    output = c("1234e23,1604998406",
               "ab47854,1591708453")
  )
  mockery::stub(git_commits, "git_run", mock_log)
  commits <- git_commits("master")
  expect_type(commits$id, "character")
})

test_that("git_pull updates git", {
  testthat::skip_on_cran()
  path <- orderly_prepare_orderly_git_example()

  commits <- git_commits("master", path[["local"]])
  expect_equal(nrow(commits), 1)

  pull <- git_pull(path[["local"]])
  expect_true(pull$success)

  commits <- git_commits("master", path[["local"]])
  expect_equal(nrow(commits), 2)
})

test_that("can clone a local repo", {
  testthat::skip_on_cran()
  path <- orderly_git_example("minimal")

  destination <- git_clone_local(path)
  expect_true(!identical(path, destination))
  ## gitignored files in source path are not copied
  expect_true(all(list.files(destination) %in% list.files(path)))
})
