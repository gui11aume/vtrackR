\name{vcheck}
\alias{vcheck}
\title{Check vtags}
\description{
   Check if an object is in syc with its vtag.
}
\usage{
vcheck(x)
}
\arguments{
   \item{x}{any R object.}
}
\details{
   Check whether an object has been modified since it obtained its
   vtag. The check is performed by comparing the 'self SHA1' that
   every vtagged object should have and the SHA1 digest of the
   object as loaded in the R session. If they differ, the object
   has been modified without tracking.
}
\note{
   Be careful that two objects can be identical in appearance but
   have a different representation in the R session. In particular
   this is the case of \code{double} and \code{integer} vectors/
}
\value{
   TRUE if the object is intact, FALSE otherwise.
}
\seealso{
   \code{\link{vtag}}
}
\author{
   Guillaume Filion <guillaume.filion@gmail.com>, Wouter Meuleman.
}
\examples{
vcheck(vtag(1))
x <- vtag(letters[1:10])
vcheck(x)
x[3] <- "C"
vcheck(x)
x[3] <- "c"
vcheck(x)
}
\keyword{vtag}
\keyword{attribute}
\keyword{version}
