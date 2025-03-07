% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yolo.R
\name{install_yolo}
\alias{install_yolo}
\title{Install and Update YOLO}
\usage{
install_yolo(python_version = "3.12.5")
}
\arguments{
\item{python_version}{A character string indicating the version of Python you
would like YOLO to run on (default: "3.12.5"). YOLO is currently compatible
with Python 3.8.0 to 3.12.8. Not all versions of Python will necessarily
work on your system, but the chosen default works on most systems that
we tested so far.}
}
\value{
If the installation/update completes successfully, a data frame
indicating the location of the YOLO installation and its version number.
}
\description{
This function automates the installation/updating of YOLO and
all its Python dependencies in a dedicated Python virtual environment for
use with the \link{trackRai} apps.
}
\note{
If the requested version of Python is not activated on your system, this
function will attempt to install it first before creating the dedicated
Python virtual environment.
}
\seealso{
\code{\link[=remove_yolo]{remove_yolo()}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
