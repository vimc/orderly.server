## It's not totally clear to me that these sorts of shenanigans will
## work all that reliably, and testing it is going to be an absolute
## horror show.  It might be somewhat eased by working in detached
## head mode because then it's easy enough to move around the tree
## without doing a whole heap of resets.
git_run <- function(args, root = NULL, check = FALSE) {
  git <- sys_which("git")
  if (!is.null(root)) {
    args <- c("-C", root, args)
  }
  res <- system3(git, args)
  if (check && !res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res
}


git_ref_to_sha <- function(ref, root = NULL, check = FALSE) {
  assert_scalar_character(ref)
  res <- git_run(c("rev-parse", ref), root = root, check = FALSE)
  if (res$success) {
    res$output
  } else if (check) {
    stop(sprintf("Git reference '%s' not found", ref), call. = FALSE)
  } else {
    NA_character_
  }
}

git_ref_exists <- function(ref, root = NULL) {
  assert_scalar_character(ref)
  git_run(c("merge-base", ref, "HEAD"), root = root, check = FALSE)$success
}

git_status <- function(root = NULL, ignore_untracked = FALSE) {
  args <- c("status", "--porcelain",
            if (ignore_untracked) "--untracked-files=no")
  res <- git_run(args, root = root, check = TRUE)
  res$clean <- length(res$output) == 0L
  res
}

git_branch_name <- function(root = NULL) {
  res <- git_run(c("rev-parse", "--abbrev-ref", "HEAD"),
                 root = root, check = TRUE)
  res$output
}

git_is_clean <- function(root, ignore_untracked = FALSE) {
  git_status(root, ignore_untracked)$clean
}

git_checkout_branch <- function(name, force = FALSE, root = NULL,
                                create = FALSE) {
  if (!force && !git_is_clean(root)) {
    stop("working directory must be clean")
    ## if force and unclean we might want to do a hard reset?
  }
  prev <- git_branch_name(root)
  ##      ^ this does not return anything sensible when we were in
  ##        detached head state; detect HEAD and get the hash perhaps?
  args <- c("checkout", if (create) "-b", name)
  orderly::orderly_log("git", sprintf("checkout %s; was %s", name, prev))
  git_run(args, root = root, check = TRUE)
  prev
}

git_clone_local <- function(source, destination = NULL) {
  orderly::orderly_log("git", "clone")
  if (!length(destination)) {
    destination <- tempfile()
    dir_create(destination)
  }
  git_run(c("clone", "--origin=local", source, destination), check = TRUE)
  remote_url <- git_run(c("config", "--get", "remote.origin.url"),
                        root = source, check = TRUE)
  git_run(c("remote", "add", "origin", remote_url$output),
          root = destination, check = TRUE)
  git_run(c("fetch", "origin"), root = destination, check = TRUE)
  destination
}

git_fetch <- function(root = NULL) {
  orderly::orderly_log("git", "fetch")
  git_run("fetch", root = root, check = TRUE)
}

git_pull <- function(root = NULL) {
  orderly::orderly_log("git", "pull")
  git_run("pull", root = root, check = TRUE)
}

git_branches_no_merged <- function(root = NULL, include_default = FALSE,
                                   default_branch = "master") {
  branches <- git_run(c("for-each-ref", "refs/remotes/origin",
                        "--sort=-committerdate",
                        "--format='%(refname:lstrip=3),%(committerdate:unix)'",
                        sprintf("--no-merged=origin/%s", default_branch)),
                      root = root, check = TRUE)$output
  if (isTRUE(include_default)) {
    default <- git_run(c("for-each-ref",
                         sprintf("refs/remotes/origin/%s", default_branch),
                        "--format='%(refname:lstrip=3),%(committerdate:unix)'"),
                      root = root, check = TRUE)$output
    branches <- c(default, branches)
  }
  branches <- utils::read.table(text = branches, stringsAsFactors = FALSE,
                                sep = ",", col.names = c("name", "last_commit"))
  branches <- branches[branches$name != "gh-pages", ]
  branches$last_commit_age <- calculate_age(branches$last_commit)
  branches$last_commit <- convert_unix_to_iso_time(branches$last_commit)
  branches
}

## This gets last 25 commits from default branch
## if not the default branch then gets the unmerged commits (limit 25)
git_commits <- function(branch, root = NULL, default_branch = "master") {
  if (branch == default_branch) {
    args <- c("log", "--pretty='%h,%cd'", "--date=unix", "--max-count=25",
              sprintf("refs/remotes/origin/%s", branch))
  } else {
    ## We want to get all unmerged commits including any merge commits
    ## --cherry-pick A...B omits any commits that introduce the same change
    ## as another commit on the other side
    ## --right-only will list only commits on the right side of A...B
    ## so --cherry-pick --right-only A...B omits commits from B which are on A
    remote_branch <- sprintf("refs/remotes/origin/%s", branch)
    args <- c("log", "--pretty='%h,%cd'", "--date=unix", "--max-count=25",
              "--right-only", "--cherry-pick",
              sprintf("refs/remotes/origin/%s...%s",
                      default_branch, remote_branch),
              remote_branch)
  }
  commits <- git_run(args, root = root, check = TRUE)$output
  ## Force columns to be characters to avoid R reading a git hash
  ## like 1234e56 as 1.234e+26
  commits <- utils::read.table(text = commits, stringsAsFactors = FALSE,
                               sep = ",", col.names = c("id", "date_time"),
                               colClasses = c("character", "integer"))
  commits$age <- calculate_age(commits$date_time)
  commits$date_time <- convert_unix_to_iso_time(commits$date_time)
  ## ID can be parsed as an integer by read.table if by chance the id contains
  ## only numbers
  commits
}

git_latest_commit <- function(branch = "master", root = NULL) {
  args <- c("log", "--pretty='%h'", "-n", "1", branch)
  git_run(args, root = root, check = TRUE)$output
}

get_reports <- function(branch, commit, show_all, default_branch, root) {
  list_all <- show_all || identical(branch, default_branch) || is_empty(branch)
  if (list_all) {
    if (is_empty(commit)) {
      if (is_empty(branch)) {
        branch <- default_branch
      }
      commit <- git_latest_commit(branch, root = root)
    }
    reports <- git_run(c("ls-tree", "--name-only", "-d",
                         sprintf("%s:src/", commit)),
                       root = root, check = TRUE)$output
  } else {
    ## Ideally we would use plumbing function diff-tree here instead of
    ## diff but at time of writing this was not supporting ... syntax
    ## As we have control over the output format this is probably safe to use
    ## the porcelain version
    reports <- git_run(
      c("diff", "--name-only", "--relative=src/",
      sprintf("refs/remotes/origin/%s...%s", default_branch, commit),
      "-- src/"),
      root = root, check = TRUE)$output
    ## We only want to return the reports which have changes i.e. the dirname
    ## of any changed files
    reports <- unique(first_dirname(reports))
  }
  reports
}

get_report_parameters <- function(report, commit, root, branch = "master") {
  tryCatch({
    ## Default to latest commit from 'branch' if missing
    if (is_empty(commit)) {
      commit <- git_latest_commit(branch, root = root)
    }
    yml <- git_run(
      c("show", paste0(commit, file.path(":src", report, "orderly.yml"))),
      root = root, check = TRUE)
    if (!isTRUE(yml$success)) {
      stop("Non zero exit code from git")
    }
  },
  error = function(e) {
    stop(sprintf(
      "Failed to get report parameters for report '%s' and commit '%s':\n%s",
      report, commit, e$message))
  })
  tryCatch(
    report_cfg <- yaml_load(yml$output),
    error = function(e) {
      stop(sprintf("Failed to parse yml for report '%s' and commit '%s':\n%s",
           report, commit, e$message))
    }
  )
  report_cfg$parameters
}
