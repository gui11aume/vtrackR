\name{vtrackR-package}
\alias{vtrackR-package}
\alias{vtrackR}
\docType{package}
\title{
Version tracking for R production scripts.
}
\description{
Generate headers for tracking the version of the script that produced a file, as well as the user, machine and environment.
}
\details{
\tabular{ll}{
Package: \tab vtrackR\cr
Version: \tab 0.1\cr
Date: \tab 2011-10-11\cr
License: \tab GPL (>=2)\cr
URL: \tab https://github.com/gui11aume/vtrackR\cr
Built: \tab R 2.12.0; ; 2011-10-03 16:57:04 UTC; unix\cr
}

Index:
\preformatted{
                        Version Tracking Header
}
The package consists of two essential functions, \code{vtag} which
produces an attribute with version tracking information for production
scripts, and objects, and \code{vcheck} which checks whether an object
has been altered since it received its vtag.
}
\author{
Guillaume Filion, Wouter Meuleman.

Maintainer: Guillaume Filion <guillaume.filion@gmail.com>
}
\keyword{version}
\seealso{
   \code{\link{digest}}
}
