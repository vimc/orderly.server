get_report_version <- function(db, name, id) {
  sql <- paste(
    "select *",
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
