build_api <- function(runner) {
  force(runner)
  api <- pkgapi::pkgapi$new()
  api$handle(endpoint_index(runner))
  api$handle(endpoint_rebuild(runner))
  api$handle(endpoint_git_status(runner))
  api$handle(endpoint_git_fetch(runner))
  api$handle(endpoint_git_pull(runner))
  api$handle(endpoint_run(runner))
  api$handle(endpoint_status(runner))
  api$handle(endpoint_kill(runner))
  ## TODO: we need to prevent these hooks throwing errors, or force
  ## them to throw errors of the correct type - that needs doing in
  ## pkgapi (RESIDE-163)
  api$registerHook("preroute", function(req)
    tryCatch(runner$poll(), error = function(e) NULL))
  api
}


schema_root <- function() {
  system.file("schema", package = "orderly.server", mustWork = TRUE)
}


returning_json <- function(schema) {
  pkgapi::pkgapi_returning_json(schema, schema_root())
}

target_index <- function() {
  list(name = scalar("orderly.server"),
       version = scalar("0.0.0"),
       endpoints = c("comming", "soon"))
}

endpoint_index <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "GET", "/", target_index,
    returning = returning_json("Index"))
}

target_rebuild <- function(runner) {
  runner$rebuild()
  NULL
}

endpoint_rebuild <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/rebuild/", target_rebuild,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Rebuild"))
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
    returning = returning_json("GitStatus"))
}

target_git_fetch <- function(runner) {
  runner$git_fetch()$output
}

endpoint_git_fetch <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/git/fetch/", target_git_fetch,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitFetch"))
}

target_git_pull <- function(runner) {
  runner$git_pull()$output
}

endpoint_git_pull <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "POST", "/v1/reports/git/pull/", target_git_pull,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("GitPull"))
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
    pkgapi::pkgapi_input_body_json("parameters", "Parameters", schema_root()),
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Run"))
}

target_status <- function(runner, key, output = FALSE) {
  res <- runner$status(key, output)
  ret <- list(key = scalar(res$key),
              status = scalar(res$status),
              version = scalar(res$id),
              output = NULL)
  ## TODO(VIMC-3654): the 'queue' path here should move elsewhere
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
    returning = returning_json("Status"))
}

## TODO: The kill endpoint should not error, but return better data
## here (as in success TRUE/FALSE, message)
target_kill <- function(runner, key) {
  tryCatch(
    jsonlite::unbox(runner$kill(key)),
    error = function(e) pkgapi::pkgapi_stop(e$message))
}

endpoint_kill <- function(runner) {
  pkgapi::pkgapi_endpoint$new(
    "DELETE", "/v1/reports/<key>/kill/", target_kill,
    pkgapi::pkgapi_state(runner = runner),
    returning = returning_json("Kill"))
}
