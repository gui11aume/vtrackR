\name{vtag}
\alias{vtag}
\title{Version Tracking Tag}
\description{
   Tag an object with version tracking information.
}
\usage{
vtag(x)
}
\arguments{
   \item{x}{any R object.}
}
\details{
   The \code{vtag} function should be used in function definitions.
   The typical use of vtags is to replace 'return(x)' statements
   by 'return(vtag(x))'.
}
\value{
   Returns a copy of x with "vtag" attribute.
}
\seealso{
   \code{\link{vcheck}}
}
\author{
   Guillaume Filion <guillaume.filion@gmail.com>, Wouter Meuleman.
}
\examples{
f <- function(x) return (vtag(x^2))
y <- f(3)
y
vcheck(y)
write(y, file='tmp.txt')
readLines(file='tmp.txt')
}
\keyword{vtag}
\keyword{attribute}
\keyword{version}
