build_api <- function(runner) {
  force(runner)
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_index(runner))
  api$handle(endpoint_rebuild(runner))
  api$handle(endpoint_git_status(runner))
  api$handle(endpoint_git_fetch(runner))
  api$handle(endpoint_git_pull(runner))
  api$handle(endpoint_git_branches(runner))
  api$handle(endpoint_git_commits(runner))
  api$handle(endpoint_run(runner))
  api$handle(endpoint_status(runner))
  api$handle(endpoint_kill(runner))
  api$handle(endpoint_run_metadata(runner))
  api$handle(endpoint_available_reports(runner))
  api$handle(endpoint_report_parameters(runner))
  api$handle(endpoint_bundle_pack(runner))
  api$handle(endpoint_bundle_import(runner))
  ## RESIDE-163: we need to prevent these hooks throwing errors, or
  ## force them to throw errors of the correct type - that needs doing
  ## in pkgapi
  api$registerHook("preroute", function(req)
    tryCatch(runner$poll(), error = function(e) NULL))
  api$setDocs(FALSE)
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
         "/v1/reports/git/status/",
         "/v1/reports/rebuild/"
       ))
}

endpoint_index <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/", target_index,
    returning = returning_json("Index.schema"))
}

target_rebuild <- function(runner) {
  runner$rebuild()
  NULL
}

endpoint_rebuild <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/rebuild/", target_rebuild,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Rebuild.schema"))
}

target_git_status <- function(runner) {
  v <- c("branch", "hash", "clean", "output")
  ret <- runner$git_status()[v]
  ret[v != "output"] <- lapply(ret[v != "output"], scalar)
  ret
}

endpoint_git_status <- function(runner) {
  endpoint_git_status <- pkgapi::pkgapi_endpoint$new(
    "GET", "/v1/reports/git/status/", target_git_status,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitStatus.schema"))
}

target_git_fetch <- function(runner) {
  runner$git_fetch()$output
}

endpoint_git_fetch <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/git/fetch/", target_git_fetch,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitFetch.schema"))
}

target_git_pull <- function(runner) {
  runner$git_pull()$output
}

endpoint_git_pull <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/git/pull/", target_git_pull,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitPull.schema"))
}

target_git_branches <- function(runner) {
  runner$git_branches_no_merged(include_master = TRUE)
}

endpoint_git_branches <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/git/branches", target_git_branches,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitBranches.schema"))
}

target_git_commits <- function(runner, branch) {
  runner$git_commits(branch)
}

endpoint_git_commits <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/git/commits", target_git_commits,
    pkgapi::pkgapi_input_query(branch = "string"),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitCommits.schema"))
}

target_run <- function(runner, name, parameters = NULL, ref = NULL,
                       instance = NULL, update = TRUE, timeout = 600) {
  key <- runner$queue(name, parameters, ref, instance, update,
                      timeout = timeout)
  list(name = scalar(name),
       key = scalar(key),
       path = scalar(sprintf("/v1/reports/%s/status/", key)))
}

endpoint_run <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/<name>/run/", target_run,
    pkgapi::pkgapi_input_query(ref = "string",
                               instance = "string",
                               update = "logical",
                               timeout = "integer"),
    pkgapi::pkgapi_input_body_json("parameters", "Parameters.schema",
                                   schema_root()),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Run.schema"))
}

target_status <- function(runner, key, output = FALSE) {
  res <- runner$status(key, output)
  ret <- list(key = scalar(res$key),
              status = scalar(res$status),
              version = scalar(res$id),
              output = NULL)
  ## VIMC-3654: the 'queue' path here should move elsewhere
  if (output || res$status == "queued") {
    ret$output <- lapply(res$output, as.character)
  }
  ret
}

endpoint_status <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/v1/reports/<key>/status/", target_status,
    pkgapi::pkgapi_input_query(output = "logical"),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Status.schema"))
}

## VIMC-3885: The kill endpoint should not error, but return better
## data here (as in success TRUE/FALSE, message)
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

  databases <- names(runner$config$database)
  instances <- NULL
  if (length(databases) > 0) {
    instances <- lapply(databases, function(db) {
      instances <- names(runner$config$database[[db]]$instances)
      instances <- instances %||% c()
      vcapply(instances, scalar, USE.NAMES = FALSE)
    })
    names(instances) <- databases
  }

  list(
    name = scalar(runner$config$server_options()$name),
    instances_supported = scalar(any(viapply(instances, length) > 0)),
    git_supported = scalar(isTRUE(runner$has_git)),
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

target_available_reports <- function(runner, branch, commit) {
  runner$get_reports(branch, commit)
}

endpoint_available_reports <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/reports/source", target_available_reports,
    pkgapi::pkgapi_input_query(branch = "string"),
    pkgapi::pkgapi_input_query(commit = "string"),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("AvailableReports.schema")
  )
}

target_report_parameters <- function(runner, report_id, commit) {
  tryCatch(
    parameters <- runner$get_report_parameters(report_id, commit),
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

endpoint_report_parameters <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/reports/<report_id>/parameters", target_report_parameters,
    pkgapi::pkgapi_input_query(commit = "string"),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("ReportParameters.schema")
  )
}

target_bundle_pack <- function(runner, name, parameters = NULL,
                               instance = NULL) {
  if (!is.null(parameters)) {
    parameters <- jsonlite::fromJSON(parameters)
  }
  path <- runner$bundle_pack(name, parameters, instance)
  on.exit(unlink(path))
  bytes <- readBin(path, "raw", n = file.size(path))
  bytes <- pkgapi::pkgapi_add_headers(
    bytes, list("Content-Disposition" = basename(path)))
  bytes
}

endpoint_bundle_pack <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/bundle/pack/<name>", target_bundle_pack,
    pkgapi::pkgapi_input_query(instance = "string"),
    pkgapi::pkgapi_input_body_json("parameters", "Parameters.schema",
                                   schema_root()),
    pkgapi::pkgapi_state(runner = runner),
    returning = pkgapi::pkgapi_returning_binary())
}

target_bundle_import <- function(runner, data) {
  path <- tempfile(fileext = ".zip")
  on.exit(unlink(path))
  writeBin(data, path)
  runner$bundle_import(path)
  scalar(TRUE)
}

endpoint_bundle_import <- function(runner, data) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/bundle/import", target_bundle_import,
    ## NOTE: This is not ideal because it requires
    ## application/octet-stream - see RESIDE-208 for details, can be
    ## updated once we move to porcelain >= 0.1.1
    pkgapi::pkgapi_input_body_binary("data"),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("BundleImport.schema"))
}
