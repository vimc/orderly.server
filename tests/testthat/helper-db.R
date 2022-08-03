insert_changelog <- function(db, changelog, public) {
  if (public)
    version_field <- "report_version_public"
  else
    version_field <- "report_version"
  sql <- paste(
    "insert into changelog (from_file,",
    "id,",
    "label,",
    "value,",
    "ordering,",
    "report_version,",
    version_field,
    ")",
    "values (1, $1, $2, $3, $4, $5, $5)", sep = "\n")
  DBI::dbExecute(db, sql, changelog)
}
