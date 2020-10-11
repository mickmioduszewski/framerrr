rf <- new.env(parent = emptyenv())

rf$start_time    <- function() Sys.time()
rf$tz            <- function() Sys.timezone()

#' Get framework start time
#'
#' @return The time the package was loaded
#' @export
#'
#' @examples
#' get_start_time()
get_start_time   <- function() {
  rf$start_time
  }
