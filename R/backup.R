#' Create object for managing orderly db backups and do first backup
#'
#' @param config Orderly config.
#' @param backup_period Period (in seconds) between DB backups.  This
#'   is a guide only as backups happen on a API preroute - if more than
#'   this many seconds have elapsed when the API receives a request then
#'   a backup of the db will be performed.  This creates a copy of
#'   orderly's destination database in\code{backup/db} with the same
#'   filename as the destination database, even if that database typically
#'   lives outside of the orderly tree.  In case of corruption of the
#'   database, this backup can be manually moved into place.  This is only
#'   needed if you are storing information alongside the core orderly
#'   tables (as done by OrderlyWeb). If NULL backup is skipped.
#'
#' @return orderly_backup_ object
#'
#' @keywords internal
orderly_backup <- function(config, backup_period) {
  orderly_backup_$new(config, backup_period)
}

#' Object for managing backing up orderly db
#'
#' @keywords internal
orderly_backup_ <- R6::R6Class(
  "orderly_backup",
  cloneable = FALSE,

  public = list(
    #' @field config Orderly config
    config = NULL,

    #' @field backup_period How frequently should backup be run
    backup_period = NULL,

    #' @field last_backup Date-time when last backup was run
    last_backup = NULL,

    #' @description
    #' Create object, and do first backup.
    #'
    #' @param config Orderly config.
    #' @param backup_period Period (in seconds) between DB backups.  This
    #'   is a guide only as backups happen on a API preroute - if more than
    #'   this many seconds have elapsed when the API receives a request then
    #'   a backup of the db will be performed.  This creates a copy of
    #'   orderly's destination database in\code{backup/db} with the same
    #'   filename as the destination database, even if that database typically
    #'   lives outside of the orderly tree.  In case of corruption of the
    #'   database, this backup can be manually moved into place.  This is only
    #'   needed if you are storing information alongside the core orderly
    #'   tables (as done by OrderlyWeb). If NULL then backup is skipped.
    initialize = function(config, backup_period) {
      self$config <- config
      self$backup_period <- backup_period
      self$do_backup()
    },

    #' @description
    #' Check if a backup is due to be run, and run it if it is due.
    check_backup = function() {
      if (!is.null(self$backup_period) && (
          self$last_backup + self$backup_period < Sys.time())) {
        self$do_backup()
      }
    },

    #' @description
    #' Backup the orderly database.
    do_backup = function() {
      if (is.null(self$backup_period)) {
        return(invisible(FALSE))
      }
      now <- Sys.time()
      message(sprintf("%s - Backing up orderly db", now))
      orderly:::orderly_backup(self$config)
      self$last_backup <- now
      invisible(TRUE)
    }
  )
)
