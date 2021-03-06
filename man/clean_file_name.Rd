% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{clean_file_name}
\alias{clean_file_name}
\title{Clean a file name string from illegal characters}
\usage{
clean_file_name(
  inf = "file name",
  suffix = "",
  blacklist = paste("[^", paste(LETTERS, sep = "", collapse = ""), paste(letters, sep =
    "", collapse = ""), paste(0:9, sep = "", collapse = ""), "\\\\s_&\\\\.\\\\-]", sep =
    "", collapse = "")
)
}
\arguments{
\item{inf}{A character vector to be cleaned into a file name component.}

\item{suffix}{A suffix to be attached if any, e.g. .PDF or .document.}

\item{blacklist}{A regular expression character class used as a blacklist,
i.e. the negated white list of characters. It's a very safe set, please
use what you feel is safe in your circumstances.

The default safe
characters are:
\itemize{
\item upper case letters and
\item lower case letters and
\item numbers 0:9 and
\item the dot separator, i.e. full stop and
\item the underscore and
\item the hyphen and
\item the ampersand and
\item the space
}}
}
\value{
Cleaned file name component.
}
\description{
Make a string used as part of a file path legal on a file system
by removing illegal characters. The defaults should give a good portable
result for most modern file systems.
}
\details{
File systems tolerate certain characters in a file name. Some characters
are not legal. This function will replace all characters that are not
white listed with space. Multiple consecutive spaces will
be replaced with a single one and the leading and trailing spaces will
be stripped.

The cleaning is not comprehensive. It deals with characters only.
For example, P, R and N are perfectly legal in Windows but a file named
PRN
might not be very useful. However, for example, it will strip
out | (a pipe) as an illegal character. Linux and others allow a broader
character sets in file names than Windows, but the point of this function
is to create a safe portable name.
}
\examples{
clean_file_name(c("fred123", "some file", "bad & good##", "w##H$A\%t^"))
}
