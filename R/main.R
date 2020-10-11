rf <- new.env(parent = emptyenv())

rf$start_time    <- function() Sys.time()
rf$tz            <- function() Sys.timezone()

#' @title
#' Get `framerrr` framework start time
#'
#' @description
#' `get_start_time` returns an object of class "POSIXct" that contains time
#'   when `framerrr` was loaded
#'
#' @details
#' This is function has no parameters.
#'
#' @return The time the package was loaded.
#' @export
#'
#' @examples
#' get_start_time()
get_start_time   <- function() {
  rf$start_time
  }
