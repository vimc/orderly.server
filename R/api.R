build_api <- function(runner, path, backup_period = NULL,
                      rate_limit = 2 * 60,
                      logger = NULL) {
  force(runner)
  api <- porcelain::porcelain$new(logger = logger)
  api$handle(endpoint_root())
  api$handle(endpoint_git_status(path))
  api$handle(endpoint_git_fetch(path))
  api$handle(endpoint_git_pull(path))
  api$handle(endpoint_git_branches(path))
  api$handle(endpoint_git_commits(path))
  api$handle(endpoint_available_reports(path))
  api$handle(endpoint_report_parameters(path))
  api$handle(endpoint_bundle_pack(path))
  api$handle(endpoint_bundle_import(path))
  api$handle(endpoint_report_info(path))
  api$handle(endpoint_run(runner))
  api$handle(endpoint_status(runner))
  api$handle(endpoint_queue_status(runner))
  api$handle(endpoint_kill(runner))
  api$handle(endpoint_dependencies(path))
  api$handle(endpoint_run_metadata(runner))
  api$handle(endpoint_workflow_summary(runner))
  api$handle(endpoint_workflow_run(runner))
  api$handle(endpoint_workflow_status(runner))
  api$setDocs(FALSE)
  backup <- orderly_backup(runner$config, backup_period)
  api$registerHook("preroute", backup$check_backup)
  api$registerHook("preroute", check_timeout(runner, rate_limit))
  api
}

schema_root <- function() {
  system.file("schema", package = "orderly.server", mustWork = TRUE)
}

returning_json <- function(schema) {
  porcelain::porcelain_returning_json(schema, schema_root())
}

target_root <- function() {
  list(name = scalar("orderly.server"),
       version = scalar(as.character(utils::packageVersion("orderly.server"))))
}

endpoint_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/", target_root,
    returning = returning_json("Root.schema"))
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
  endpoint_git_status <- porcelain::porcelain_endpoint$new(
    "GET", "/v1/reports/git/status/", target_git_status,
    porcelain::porcelain_state(path = path),
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
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/reports/git/fetch/", target_git_fetch,
    porcelain::porcelain_state(path = path),
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
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/reports/git/pull/", target_git_pull,
    porcelain::porcelain_state(path = path),
    returning = returning_json("GitPull.schema"))
}

target_git_branches <- function(path) {
  git_branches_no_merged(path, include_master = TRUE)
}

endpoint_git_branches <- function(path) {
  porcelain::porcelain_endpoint$new(
    "GET", "/git/branches", target_git_branches,
    porcelain::porcelain_state(path = path),
    returning = returning_json("GitBranches.schema"))
}

target_git_commits <- function(path, branch) {
  git_commits(branch, path)
}

endpoint_git_commits <- function(path) {
  porcelain::porcelain_endpoint$new(
    "GET", "/git/commits", target_git_commits,
    porcelain::porcelain_input_query(branch = "string"),
    porcelain::porcelain_state(path = path),
    returning = returning_json("GitCommits.schema"))
}

target_available_reports <- function(path, branch = NULL, commit = NULL,
                                     show_all = FALSE) {
  get_reports(branch, commit, show_all, path)
}

endpoint_available_reports <- function(path) {
  porcelain::porcelain_endpoint$new(
    "GET", "/reports/source", target_available_reports,
    porcelain::porcelain_input_query(branch = "string"),
    porcelain::porcelain_input_query(commit = "string"),
    porcelain::porcelain_input_query(show_all = "logical"),
    porcelain::porcelain_state(path = path),
    returning = returning_json("AvailableReports.schema")
  )
}

target_report_parameters <- function(path, report_id, commit = NULL) {
  tryCatch(
    parameters <- get_report_parameters(report_id, commit, path),
    error = function(e) {
      porcelain::porcelain_stop(e$message, "FAILED_RETRIEVE_PARAMS")
    }
  )
  if (!is.null(parameters) && is.null(names(parameters))) {
    porcelain::porcelain_stop(
      sprintf("Failed to parse parameters for report '%s' and commit '%s'",
              report_id, commit %||% "none"),
      "INVALID_FORMAT")
  }
  lapply(names(parameters), function(param) {
    value <- parameters[[param]]$default
    if (!is.null(value)) {
      value <- as.character(value)
    }
    list(
      name = scalar(param),
      value = scalar(value)
    )
  })
}

endpoint_report_parameters <- function(path) {
  porcelain::porcelain_endpoint$new(
    "GET", "/reports/<report_id>/parameters", target_report_parameters,
    porcelain::porcelain_input_query(commit = "string"),
    porcelain::porcelain_state(path = path),
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
  bytes <- porcelain::porcelain_add_headers(
    bytes, list("Content-Disposition" = basename(res$path)))
  bytes
}

endpoint_bundle_pack <- function(path) {
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/bundle/pack/<name>", target_bundle_pack,
    porcelain::porcelain_input_query(instance = "string"),
    porcelain::porcelain_input_body_json("parameters", "Parameters.schema",
                                   schema_root()),
    porcelain::porcelain_state(path = path),
    returning = porcelain::porcelain_returning_binary())
}

target_bundle_import <- function(path, data) {
  data_path <- tempfile(fileext = ".zip")
  on.exit(unlink(data_path))
  writeBin(data, data_path)
  orderly::orderly_bundle_import(data_path, root = path)
  scalar(TRUE)
}

endpoint_bundle_import <- function(path, data) {
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/bundle/import", target_bundle_import,
    ## NOTE: This is not ideal because it requires
    ## application/octet-stream - see RESIDE-208 for details, can be
    ## updated once we move to porcelain >= 0.1.1
    porcelain::porcelain_input_body_binary("data"),
    porcelain::porcelain_state(path = path),
    returning = returning_json("BundleImport.schema"))
}

target_report_info <- function(path, id, name) {
  info <- orderly::orderly_info(id, name, root = path)
  info <- recursive_scalar(info)
  ## Rename parameters to params for consistency with rest of API
  info <- append(info, list(params = info$parameters))
  info$parameters <- NULL
  info
}

endpoint_report_info <- function(path) {
  porcelain::porcelain_endpoint$new(
    "GET", "/v1/report/info", target_report_info,
    porcelain::porcelain_input_query(id = "string"),
    porcelain::porcelain_input_query(name = "string"),
    porcelain::porcelain_state(path = path),
    returning = returning_json("ReportInfo.schema")
  )
}

target_run <- function(runner, name, body = NULL, ref = NULL,
                       instance = NULL, timeout = 60 * 60 * 3) {
  if (!is.null(body)) {
    body <- jsonlite::fromJSON(body)
  }
  changelog <- format_changelog(body$changelog)
  key <- runner$submit_task_report(name, body$params, ref, instance,
                                   changelog = changelog,
                                   timeout = timeout)
  list(name = scalar(name),
       key = scalar(key),
       path = scalar(sprintf("/v1/reports/%s/status/", key)))
}

endpoint_run <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/reports/<name>/run/", target_run,
    porcelain::porcelain_input_query(ref = "string",
                               instance = "string",
                               timeout = "integer"),
    porcelain::porcelain_input_body_json("body", "RunRequest.schema",
                                         schema_root()),
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("Run.schema"))
}

target_status <- function(runner, key, output = FALSE) {
  res <- runner$status(key, output)
  list(
    key = scalar(res$key),
    status = scalar(res$status),
    version = scalar(res$version),
    output = res$output,
    queue = lapply(res$queue, function(item) {
      lapply(item, scalar)
    })
  )
}

endpoint_status <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "GET", "/v1/reports/<key>/status/", target_status,
    porcelain::porcelain_input_query(output = "logical"),
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("Status.schema"))
}

target_queue_status <- function(runner) {
  res <- runner$queue_status()
  recursive_scalar(res)
}

endpoint_queue_status <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "GET", "/v1/queue/status/", target_queue_status,
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("QueueStatus.schema"))
}

target_kill <- function(runner, key) {
  tryCatch(
    jsonlite::unbox(runner$kill(key)),
    error = function(e) porcelain::porcelain_stop(e$message))
}

endpoint_kill <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "DELETE", "/v1/reports/<key>/kill/", target_kill,
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("Kill.schema"))
}

target_dependencies <- function(path, name,
                                id = "latest",
                                direction = "downstream",
                                propagate = TRUE,
                                max_depth = 100,
                                show_all = FALSE,
                                use = "archive") {
  get_dependencies(path = path,
                   name = name,
                   id = id,
                   direction = direction,
                   propagate = propagate,
                   max_depth = max_depth,
                   show_all = show_all,
                   use = use)
}

endpoint_dependencies <- function(path) {
  porcelain::porcelain_endpoint$new(
    "GET", "/v1/reports/<name>/dependencies/", target_dependencies,
    porcelain::porcelain_input_query(id = "string",
                               direction = "string",
                               propagate = "logical",
                               max_depth = "integer",
                               show_all = "logical",
                               use = "string"),
    porcelain::porcelain_state(path = path),
    returning = returning_json("Dependencies.schema"))
}


target_run_metadata <- function(runner) {
  changelog <- runner$config$changelog$id
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

  git_supported <- !isTRUE(server_options$master_only)

  list(
    name = scalar(runner$config$server_options()$name),
    instances_supported = scalar(instances_supported),
    git_supported = scalar(git_supported),
    instances = instances,
    changelog_types = changelog
  )
}

endpoint_run_metadata <- function(runner) {
 porcelain::porcelain_endpoint$new(
   "GET", "/run-metadata", target_run_metadata,
   porcelain::porcelain_state(runner = runner),
   returning = returning_json("RunMetadata.schema")
 )
}

target_workflow_summary <- function(runner, body) {
  body <- jsonlite::fromJSON(body, simplifyDataFrame = FALSE)
  if (!is.null(body$ref)) {
    runner$assert_ref_switching_allowed(body$ref)
  }
  workflow_summary(runner$root, body$reports, body$ref)
}

endpoint_workflow_summary <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/workflow/summary/",
    target_workflow_summary,
    porcelain::porcelain_input_body_json(
      "body", "WorkflowSummaryRequest.schema", schema_root()),
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("WorkflowSummaryResponse.schema"))
}

target_workflow_run <- function(runner, body) {
  body <- jsonlite::fromJSON(body, simplifyDataFrame = FALSE)
  changelog <- format_changelog(body$changelog)
  res <- runner$submit_workflow(body$reports, body$ref, changelog)
  list(
    workflow_key = scalar(res$workflow_key),
    reports = res$reports
  )
}

endpoint_workflow_run <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "POST", "/v1/workflow/run/",
    target_workflow_run,
    porcelain::porcelain_input_body_json("body", "WorkflowRunRequest.schema",
                                         schema_root()),
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("WorkflowRunResponse.schema"))
}

target_workflow_status <- function(runner, workflow_key, output = FALSE) {
  res <- runner$workflow_status(workflow_key, output)
  reports <- lapply(res$reports, function(report) {
    list(
      key = scalar(report$key),
      status = scalar(report$status),
      version = scalar(report$version),
      output = report$output,
      queue = lapply(report$queue, function(item) {
        lapply(item, scalar)
      })
    )
  })
  list(
    workflow_key = scalar(res$workflow_key),
    status = scalar(res$status),
    reports = reports
  )
}

endpoint_workflow_status <- function(runner) {
  porcelain::porcelain_endpoint$new(
    "GET", "/v1/workflow/<workflow_key>/status/",
    target_workflow_status,
    porcelain::porcelain_input_query(output = "logical"),
    porcelain::porcelain_state(runner = runner),
    returning = returning_json("WorkflowStatus.schema"))
}

check_timeout <- function(runner, rate_limit = 2 * 60) {
  throttle(runner$check_timeout, rate_limit)
}
