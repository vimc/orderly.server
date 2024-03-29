get_report_version <- function(db, name, id) {
  sql <- paste(
    "select report as name,",
    "displayname,",
    "id,",
    "date,",
    "description",
    "from report_version",
    "where report_version.report = $1 and report_version.id = $2",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql, list(name, id))
  if (nrow(dat) == 0) {
    porcelain::porcelain_stop(
      sprintf("Unknown report version '%s'", id),
      "NONEXISTENT_REPORT_VERSION",
      status_code = 404L)
  }
  return(dat)
}

get_artefacts_for_version <- function(db, id) {
  sql <- paste(
    "select",
    "       report_version_artefact.'order' as id,",
    "       report_version_artefact.format,",
    "       report_version_artefact.description,",
    "       file_artefact.filename,",
    "       file.size",
    "  from report_version_artefact",
    "  join file_artefact",
    "    on file_artefact.artefact = report_version_artefact.id",
    "  join file",
    "    on file.hash = file_artefact.file_hash",
    " where report_version = $1",
    " order by 'order'",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql, id)

  ## Bit of a pain to prepare this for serialisation nicely:
  process <- function(x) {
    list(id = scalar(x$id[[1]]),
         format = scalar(x$format[[1]]),
         description = scalar(x$description[[1]]),
         files = Map(function(filename, size)
                       list(filename = scalar(filename), size = scalar(size)),
                     x$filename, x$size, USE.NAMES = FALSE))
  }

  lapply(unname(split(dat, dat$id)), process)
}

get_parameters_for_versions <- function(db, versions) {
  sql <- paste(
    "select",
    "       parameters.report_version,",
    "       parameters.name,",
    "       parameters.value",
    "  from parameters",
    sprintf(" where parameters.report_version in (%s)",
            paste(squote(versions), collapse = ", ")),
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql)

  process <- function(x) {
    vals <- lapply(as.list(x$value), function(y) scalar(y))
    names(vals) <- x$name
    vals
  }

  lapply(split(dat, dat$report_version), process)
}

get_instances_for_version <- function(db, id) {
  sql <- paste(
    "select",
    "       report_version_instance.instance,",
    "       report_version_instance.type",
    "  from report_version_instance",
    " where report_version_instance.report_version = $1",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql, id)
  res <- lapply(dat[, 1], function(x) scalar(x))
  names(res) <- dat[, 2]
  res
}

get_resources_for_version <- function(db, id) {
  sql <- paste(
    "select",
    "       file_input.filename as name,",
    "       file.size",
    "  from file_input",
    "  inner join file",
    "  on file_input.file_hash = file.hash",
    " where file_input.report_version = $1",
    " and file_input.file_purpose = 'resource'",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql, id)
  dat
}

get_data_for_version <- function(db, id) {
  sql <- paste(
    "select",
    "       report_version_data.name,",
    "       data.size_csv as csv_size,",
    "       data.size_rds as rds_size",
    "  from report_version_data",
    "  inner join data",
    "  on report_version_data.hash = data.hash",
    " where report_version_data.report_version = $1",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql, id)
  dat
}

get_custom_fields_for_versions <- function(db, versions) {
  sql <- paste(
    "select",
    "       report_version_custom_fields.key,",
    "       report_version_custom_fields.value,",
    "       report_version_custom_fields.report_version",
    "  from report_version_custom_fields",
    sprintf(" where report_version in (%s)",
            paste(squote(versions), collapse = ", ")),
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql)

  process <- function(x) {
    vals <- lapply(as.list(x$value), function(y) scalar(y))
    names(vals) <- x$key
    vals
  }

  lapply(split(dat, dat$report_version), process)
}

generate_latest_versions_for_report <- function(db, reports = NULL) {
  sql <- paste(
    "create temporary table latest_versions_for_reports as",
    "select report_version.report,",
    "       report_version.id as latestVersion,",
    "       max(report_version.date) as maxDate",
    "  from report_version",
    sep = "\n")
  if (!is.null(reports)) {
    sql <- paste(sql, sprintf("where report_version.report in (%s)",
                              paste(squote(reports), collapse = ", ")),
                 sep = "\n")
  }

  sql <- paste(sql, "group by report_version.report", sep = "\n")
  DBI::dbExecute(db, sql)
}

get_all_reports <- function(db, reports = NULL) {
  generate_latest_versions_for_report(db, reports)
  sql <- paste(
    "select report_version.report as 'name',",
    "report_version.displayname as 'display_name',",
    "report_version.id as 'latest_version'",
    "from report_version",
    "join latest_versions_for_reports",
    "on report_version.id = latest_versions_for_reports.latestVersion",
    "order by report_version.report",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql)
}

get_all_versions <- function(db) {
  generate_latest_versions_for_report(db)
  sql <- paste(
    "select report_version.report as 'name',",
    "report_version.displayname as 'display_name',",
    "report_version.id,",
    "report_version.date,",
    "report_version.description,",
    "latest_versions_for_reports.latestVersion as 'latest_version'",
    "from report_version",
    "join latest_versions_for_reports",
    "on report_version.report = latest_versions_for_reports.report",
    "order by report_version.report, report_version.id",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql)
}
