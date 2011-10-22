\name{addcomment}
\alias{addcomment}
\title{Add user comment to object}
\description{
   Add a user comment to vtag that is printed in vheader
}
\usage{
   addcomment(x, field, comment)
}
\arguments{
   \item{x}{any R object, with or without a vtag attribute.}
   \item{field}{a \code{character} vector specifying in which
      field to place the comment.}
   \item{comment}{any R object.} 
}
\details{
   The \code{addcomment} function does not return. It only has
   the side effect of adding the given comment to the attributes
   of \code{x}.

   The comments are placed in the \code{comments} meta-field of
   the \code{vtag} attribute of \code{x}.
}
\value{
   Does not return.
}
\author{
   Guillaume Filion <guillaume.filion@gmail.com>, Wouter Meuleman.
}
\examples{
x <- vtag(1)
cat(vheader(x))

addcomment(x, "foo", "bar")
cat(vheader(x))
}
\keyword{version}
\keyword{comment}
