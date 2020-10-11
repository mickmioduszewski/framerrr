rf <- new.env(parent = emptyenv())

#' @title
#' Start the framework
#'
#' @description
#' Sets defaults and starts the framework
#'
#' @details
#' This is function has no parameters.
#'
#' @param project A character vector of length 1 containing the name of the
#'   project.
#' @param dref A character vector with the list of
#'   configuration entries that contain multiple definitions,
#'   each referring to a specific named operating environment.
#' @param tz A time zone to use in date and time calculations.
#' @param log A name of a variable that contains the definition of the log
#'   location for the project.
#' @param global_config A definition of the location of the global
#'   configuration file
#' @param local_config A definition of the location of the local
#'   configuration file
#' @param f_log A definition of the log
#'   location for the project.
#' @param d_output A definition of the output
#'   directory.
#' @param d_input A definition of the input
#'   directory.
#' @param d_internal A definition of the internal
#'   directory
#' @return The configuration environment.
#' @export
#'
#' @examples
#' run_start()
run_start <-
  function(project       = "project",
           dref          = "",
           tz            = Sys.timezone(),
           log           = c("f_log"),
           global_config = file.path(dirname(getwd()), "config", "config.R"),
           local_config  = file.path(getwd(), "config.R"),
           f_log         = list(file = "log.csv", dir = "d_internal"),
           d_output      = c(dir = file.path(dirname(dirname(getwd())),
                                             "AnalyticSoftwareOutput")),
           d_input       = c(dir = file.path(dirname(dirname(getwd())),
                                             "AnalyticSoftwareInput")),
           d_internal    = c(dir = file.path(dirname(dirname(getwd())),
                                             "AnalyticSoftwareInternal"))) {
    rf$project       <- project
    rf$dref          <- dref
    rf$tz            <- Sys.timezone()
    rf$log           <- log
    rf$global_config <- global_config
    rf$local_config  <- local_config
    rf$f_log         <- f_log
    rf$d_output      <- d_output
    rf$d_input       <- d_input
    rf$d_internal    <- d_internal
    return(invisible(rf))
  }
