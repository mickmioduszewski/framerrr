rf <- new.env(parent = emptyenv())

#' @importFrom utils str write.csv read.csv


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
#' @param envir A name for the environment.
#' @param dref A character vector with the list of
#'   configuration entries that contain multiple definitions,
#'   each referring to a specific named operating environment.
#' @param tz A time zone to use in date and time calculations.
#' @param log A name of a variable that contains the definition of the log
#'   location for the project.
#' @param global_config A definition of the location of the global
#'   configuration file. Make it NA if you don't want a global configuration
#'   file.
#' @param local_config A definition of the location of the local
#'   configuration file. Make it NA if you don't want a global configuration
#'   file.
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
#' run_start(global_config = NA, local_config = NA)
run_start <-
  function(project       = "project",
           envir         = "default",
           dref          = vector(mode = "character", length = 0),
           tz            = Sys.timezone(),
           log           = "f_log",
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
    rf$envir         <- envir
    rf$dref          <- dref
    rf$tz            <- Sys.timezone()
    rf$log           <- log
    rf$global_config <- global_config
    rf$local_config  <- local_config
    rf$f_log         <- f_log
    rf$d_output      <- d_output
    rf$d_input       <- d_input
    rf$d_internal    <- d_internal
    rf$start_time    <- Sys.time()
    make_config()
    dereference()
    return(invisible(rf))
  }

run_end <- function(print_duration = TRUE) {
  rf$end_time <- Sys.time()
  if (!is.na(rf$log[[1]])) {
    file_name <- make_fp(rf$log[[1]])
    temp <- read_log(file_name)
    n <- NROW(temp)
    temp$end[n] <- format(rf$end_time, tz = rf$tz, usetz = TRUE)
    temp$duration[n] <- difftime(rf$end_time, rf$start_time, units = "secs")
    utils::write.csv(x = temp, file = file_name, row.names = FALSE)
  }
  duration <- difftime(rf$end_time, rf$start_time)
  if (print_duration) print(duration)
  return(invisible(duration))
}

make_config <- function() {
  if (is.na(rf$global_config)) {
    message("global configuration file ignored as requested")
  } else if (file.exists(rf$global_config)) {
    source(rf$global_config, local = rf)
  } else {
    stop("global configuration does not exist")
  }

  if (is.na(rf$local_config)) {
    message("local configuration file ignored as requested")
  } else if (file.exists(rf$local_config)) {
    source(rf$local_config, local = rf)
  } else {
    stop("local configuration does not exist")
  }
}

dereference <- function() {
  for (i in seq_along(rf$dref)) {
    d <- rf$dref[[i]]

    if (is.na(d) || is.null(d) || d == "") next

    if (!exists(d, envir = rf))
      stop(paste("'dref' refers to", d, "which does not exist"))

    x <- get(d, envir = rf)
    if (!is.list(x))
      stop(paste("'dref' refers to", rf$dref[[i]], "which is not a list"))

    n <- names(x)
    if (!(rf$envir %in% n))
      stop(paste("'dref' refers to", d,
                 "for environment", rf$envir,
                 "but the list has no such element"))
    assign(d, value = x[[rf$envir]], envir = rf)
  }
  return(invisible(NULL))
}

make_log <- function(logical_file) {
  file_name <- make_fp(logical_file)
  new_temp <- data.frame(project = rf$project,
                         start = format(
                           rf$start_time,
                           tz = rf$tz,
                           usetz = TRUE
                         ),
                         end = as.character(NA),
                         duration = 0.0,
                         stringsAsFactors = FALSE)
  if (file.exists(file_name)) {
    temp <- read_log(file_name)
    new_temp <- rbind(temp, new_temp)
  }
  utils::write.csv(x = new_temp, file = file_name, row.names = FALSE)
}

make_fp <- function(logical_file, subdir = NULL) {
  if (!exists(logical_file, envir = rf))
    stop(paste(logical_file,
               "logical file name not defined in configuration=>"))
  file_def <- get(logical_file, envir = rf)
  if (!("file" %in% names(file_def)))
    stop(paste("file definition must contain 'file' element"))
  if (!("dir" %in% names(file_def)))
    stop(paste("file definition must contain 'dir' element"))
  d <- make_dp(file_def$dir, subdir = subdir)
  return(file.path(d, file_def$file))
}

make_dp <- function(logical_dir, subdir = NULL) {
  ### Make directory path if it does not exist. Return a full dir path
  if (!exists(logical_dir, envir = rf))
    stop(paste(logical_dir,
               "logical directory name not defined in configuration=>"))
  dir <- get(logical_dir, envir = rf)
  if (!is.character(dir) || is.null(names(dir)) || !("dir" %in% names(dir)))
    stop(paste(logical_dir, "directory should be a named vector with",
               "'dir' element and an optional 'absolute' element"))
  if ("absolute" %in% names(dir)) {
    dir <- dir[["dir"]]
  } else {
    dir <- file.path(dir[["dir"]], rf$envir, rf$project)
  }
  if (!is.null(subdir)) dir <- file.path(dir, subdir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  return(dir)
}

read_log <- function(file) utils::read.csv(file)

list_env <- function() {
  objects(rf, all.names = TRUE)
}

print_env <- function() {
  x <- rf$list_env()
  for (i in seq_along(x)) {
    rf$get_var(x[[i]])
  }
}

get_var <- function(var_name) {
  if (!exists(var_name, envir = rf)) {
    stop(paste(var_name, "doesn't exist in 'framerrr'"))
  }
  x <- get(var_name, envir = rf)
  t <- typeof(x)
  print(paste(var_name, "is of type", t))
  if (t != "closure") utils::str(x)
  return(invisible(NULL))
}


get_start_time <- function() rf$start_time

#' @title
#' Clean a file name string from illegal characters
#'
#' @description
#' Make a string used as a file name a legal file name on a file system
#'   by removing illegal characters
#'
#' @details
#' File systems tolerate certain characters in a file name. Some characters
#'   are not legal. This function will replace all characters that are not
#'   white listed and replace them with space. Multiple consecutive spaces will
#'   be replaced with a single one and the leading and trailing spaces will
#'   be stripped.
#'
#' @param inf A character vector to be cleaned into a file name component.
#' @param suffix A suffix to be attached if any, e.g. .PDF or .document.
#' @param blacklist A regular expression character class used as a blacklist,
#'   i.e. the negated white list of characters.
#' @return Cleaned file name component.
#' @export
#'
#' @examples
#' clean_file_name(c("fred123", "some file", "bad file##", "w##H$A%t^"))
clean_file_name <- function(inf = "file name",
                            suffix = "",
                            blacklist = paste0("[^ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                               "abcdefghijklmnopqrstuvwxyz",
                                               "0123456789",
                                               "\\s_&\\.\\-]")) {
  out <- gsub(blacklist, " ", inf)
  out <- gsub("\\s+" , " ", out)
  out <- trimws(out, which = "both") # trim white space
  out <- paste(out, suffix, sep = "")
  return(out)
}
