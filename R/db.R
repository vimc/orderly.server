get_report_version <- function(db, name, id) {
  str(name)
  str(id)
  sql <- paste(
    "select *",
    "from report_version",
    "where report_version.report = $1 and report_version.id = $2",
    sep = "\n")
  dat <- DBI::dbGetQuery(db, sql, list(name, id))
  if (length(dat) == 0) {
    porcelain::porcelain_stop(
      sprintf("Unknown report version '%s-%s'", name, id),
      "NONEXISTENT_REPORT_VERSION",
      status_code = 404L)
  }
  return(dat)
}
