\name{vheader}
\alias{vheader}
\title{Version Tracking Header}
\description{
   Produce a header for version tracking.
}
\usage{
   vheader(x, comment.char = "#", extrafields = "")
}
\arguments{
   \item{x}{any R object with a vtag attribute.}
   \item{comment.char}{a character to prepend to every line of thei
      header.}
   \item{extrafields}{a \code{character} vector with the names of 
      the vtag fields to add to the vheader.}
}
\details{
   The \code{vheader} function is called by default in output
   functions, such as \code{write}, \code{cat} and \code{write.table}.

   the special \code{comments} meta-field of the \code{vtag} attribute
   can be updated by the user to output additional comments in the
   vheader of an file.
}
\value{
   A string (character \code{vector} of length 1) with information
   regarding the call, the working directory, the logged-in user,
   the date, the R version, platform and system being used, the
   version of the function's package and the SHA1 digests of all
   objects in the call (including the function itself).
}
\author{
   Guillaume Filion <guillaume.filion@gmail.com>, Wouter Meuleman.
}
\examples{
x <- vtag(1)
cat(vheader(x))

attr(x, "vtag")[["comments"]][["foo"]] <- "bar"
cat(vheader(x))
}
\keyword{version}
