build_api <- function(runner, path) {
  force(runner)
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_index())
  api$handle(endpoint_git_status(path))
  api$handle(endpoint_git_fetch(path))
  api$handle(endpoint_git_pull(path))
  api$handle(endpoint_git_branches(path))
  api$handle(endpoint_git_commits(path))
  api$handle(endpoint_available_reports(path))
  api$handle(endpoint_report_parameters(path))
  api$handle(endpoint_bundle_pack(path))
  api$handle(endpoint_bundle_import(path))
  api$handle(endpoint_run(runner))
  api$handle(endpoint_status(runner))
  api$handle(endpoint_kill(runner))
  api$handle(endpoint_run_metadata(runner))
  api$setDocs(FALSE)
  api$registerHook("preroute", check_timeout(runner))
  api
}


schema_root <- function() {
  system.file("schema", package = "orderly.server", mustWork = TRUE)
}


returning_json <- function(schema) {
  pkgapi::pkgapi_returning_json(schema, schema_root())
}

## For compatibility only
target_index <- function() {
  list(name = scalar("orderly.server"),
       version = scalar(as.character(utils::packageVersion("orderly.server"))),
       endpoints = c(
         "/",
         "/v1/reports/:key/kill/",
         "/v1/reports/:key/status/",
         "/v1/reports/:name/run/",
         "/v1/reports/git/fetch/",
         "/v1/reports/git/pull/",
         "/v1/reports/git/status/"
       ))
}

endpoint_index <- function() {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/", target_index,
    returning = returning_json("Index.schema"))
}

target_git_status <- function(path) {
  ret <- list(
    branch = scalar(git_branch_name(path)),
    hash = scalar(git_ref_to_sha("HEAD", path))
  )
  status <- git_status(path)
  ret$clean <- scalar(status$clean)
  ret$output <- status$output
  ret
}

endpoint_git_status <- function(path) {
  endpoint_git_status <- pkgapi::pkgapi_endpoint$new(
    "GET", "/v1/reports/git/status/", target_git_status,
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("GitStatus.schema"))
}

target_git_fetch <- function(path) {
  res <- git_fetch(path)
  if (length(res$output) > 0L) {
    orderly::orderly_log("fetch", res$output)
  }
  res$output
}

endpoint_git_fetch <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/git/fetch/", target_git_fetch,
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("GitFetch.schema"))
}

target_git_pull <- function(path) {
  res <- git_pull(path)
  if (length(res$output) > 0L) {
    orderly::orderly_log("pull", res$output)
  }
  res$output
}

endpoint_git_pull <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/git/pull/", target_git_pull,
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("GitPull.schema"))
}

target_git_branches <- function(path) {
  git_branches_no_merged(path, include_master = TRUE)
}

endpoint_git_branches <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/git/branches", target_git_branches,
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("GitBranches.schema"))
}

target_git_commits <- function(path, branch) {
  git_commits(branch, path)
}

endpoint_git_commits <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/git/commits", target_git_commits,
    pkgapi::pkgapi_input_query(branch = "string"),
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("GitCommits.schema"))
}

endpoint_available_reports <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/reports/source", target_available_reports,
    pkgapi::pkgapi_input_query(branch = "string"),
    pkgapi::pkgapi_input_query(commit = "string"),
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("AvailableReports.schema")
  )
}

target_report_parameters <- function(path, report_id, commit) {
  tryCatch(
    parameters <- get_report_parameters(report_id, commit, path),
    error = function(e) {
      pkgapi::pkgapi_stop(e$message, "FAILED_RETRIEVE_PARAMS")
    }
  )
  if (!is.null(parameters) && is.null(names(parameters))) {
    pkgapi::pkgapi_stop(
      sprintf("Failed to parse parameters for report '%s' and commit '%s'",
              report_id, commit),
      "INVALID_FORMAT")
  }
  lapply(names(parameters), function(param) {
    default <- parameters[[param]]$default
    if (!is.null(default)) {
      default <- as.character(default)
    }
    list(
      name = scalar(param),
      default = scalar(default)
    )
  })
}

endpoint_report_parameters <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/reports/<report_id>/parameters", target_report_parameters,
    pkgapi::pkgapi_input_query(commit = "string"),
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("ReportParameters.schema")
  )
}

target_bundle_pack <- function(path, name, parameters = NULL,
                               instance = NULL) {
  if (!is.null(parameters)) {
    parameters <- jsonlite::fromJSON(parameters)
  }
  res <- orderly::orderly_bundle_pack(tempfile(), name, parameters, root = path,
                                      instance = instance)
  on.exit(unlink(res$path))
  bytes <- readBin(res$path, "raw", n = file.size(res$path))
  bytes <- pkgapi::pkgapi_add_headers(
    bytes, list("Content-Disposition" = basename(res$path)))
  bytes
}

endpoint_bundle_pack <- function(path) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/bundle/pack/<name>", target_bundle_pack,
    pkgapi::pkgapi_input_query(instance = "string"),
    pkgapi::pkgapi_input_body_json("parameters", "Parameters.schema",
                                   schema_root()),
    pkgapi::pkgapi_state(path = path),
    returning = pkgapi::pkgapi_returning_binary())
}

target_bundle_import <- function(path, data) {
  data_path <- tempfile(fileext = ".zip")
  on.exit(unlink(data_path))
  writeBin(data, data_path)
  orderly::orderly_bundle_import(data_path, root = path)
  scalar(TRUE)
}

endpoint_bundle_import <- function(path, data) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/bundle/import", target_bundle_import,
    ## NOTE: This is not ideal because it requires
    ## application/octet-stream - see RESIDE-208 for details, can be
    ## updated once we move to porcelain >= 0.1.1
    pkgapi::pkgapi_input_body_binary("data"),
    pkgapi::pkgapi_state(path = path),
    returning = returning_json("BundleImport.schema"))
}

target_run <- function(runner, name, parameters = NULL, ref = NULL,
                       instance = NULL, timeout = 10800) {
  if (!is.null(parameters)) {
    parameters <- jsonlite::fromJSON(parameters)
  }
  key <- runner$submit_task_report(name, parameters, ref, instance,
                                   timeout = timeout)
  list(name = scalar(name),
       key = scalar(key),
       path = scalar(sprintf("/v1/reports/%s/status/", key)))
}

target_available_reports <- function(path, branch, commit) {
  get_reports(branch, commit, path)
}

endpoint_run <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/<name>/run/", target_run,
    pkgapi::pkgapi_input_query(ref = "string",
                               instance = "string",
                               timeout = "integer"),
    pkgapi::pkgapi_input_body_json("parameters", "Parameters.schema",
                                   schema_root()),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Run.schema"))
}

target_status <- function(runner, key, output = FALSE) {
  res <- runner$status(key, output)
  list(
    key = scalar(res$key),
    status = scalar(res$status),
    version = scalar(res$version),
    output = res$output,
    queue = lapply(res$queue, scalar)
  )
}

endpoint_status <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/v1/reports/<key>/status/", target_status,
    pkgapi::pkgapi_input_query(output = "logical"),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Status.schema"))
}

target_kill <- function(runner, key) {
  tryCatch(
    jsonlite::unbox(runner$kill(key)),
    error = function(e) pkgapi::pkgapi_stop(e$message))
}

endpoint_kill <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "DELETE", "/v1/reports/<key>/kill/", target_kill,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Kill.schema"))
}

target_run_metadata <- function(runner) {
  changelog <- runner$config$changelog$id[runner$config$changelog$public]
  if (length(changelog) > 0) {
    changelog <- vcapply(changelog, scalar, USE.NAMES = FALSE)
  }

  server_options <- runner$config$server_options()
  instances <- NULL
  instances_supported <- FALSE
  if (!isTRUE(server_options$primary)) {
    databases <- names(runner$config$database)
    if (length(databases) > 0) {
      instances <- lapply(databases, function(db) {
        instances <- names(runner$config$database[[db]]$instances)
        vcapply(instances, scalar, USE.NAMES = FALSE)
      })
      names(instances) <- databases
    }
    instances_supported <- any(lengths(instances) > 0)
  }

  git_supported <- !isTRUE(server_options$master_only) && isTRUE(runner$has_git)

  list(
    name = scalar(runner$config$server_options()$name),
    instances_supported = scalar(instances_supported),
    git_supported = scalar(git_supported),
    instances = instances,
    changelog_types = changelog
  )
}

endpoint_run_metadata <- function(runner) {
 pkgapi::pkgapi_endpoint$new(
   "GET", "/run-metadata", target_run_metadata,
   pkgapi::pkgapi_state(runner = runner),
   returning = returning_json("RunMetadata.schema")
 )
}

check_timeout <- function(runner) {
  function() {
    runner$check_timeout()
  }
}
