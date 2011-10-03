\name{vheader}
\alias{vheader}
\title{Version Tracking Header}
\description{
   Produce a header for version tracking.
}
\usage{
   vheader(call)
}
\arguments{
   \item{call}{a \code{call} object.}
}
\details{
   The \code{vheader} function should always be used within a
   function definition and always called as \code{vheader(match.call())}.
}
\value{
   A string (character \code{vector} of length 1) with information
   regarding the call, the working directory, the logged-in user,
   the date, the R version, platform and system being used, the
   version of the function's package and the SHA1 digests of all
   objects in the call (including the function itself).
}
\note{
   The SHA1 digests are usually not the ones of the character
   representations of the objects, but the SHA1 digests of the
   objects as loaded in the R session. You can check the SHA1
   digest of any object in the session by calling the \code{SHA1}
   function.
}
\author{
   Guillaume Filion <guillaume.filion@gmail.com>.
}
\seealso{
   \code{\link{match.call}}, \code{\link{SHA1}} and the package
   \code{digest} for the implementation of the SHA1 digest.
}
\examples{
# Insert in a function definition.
f <- function(...) {
   require(vtrackR);
   cat(vheader(match.call()));
}
f();
f(1, x="grand", y=sort);
}
\keyword{version}
\keyword{call}
