\name{SHA1}
\alias{SHA1}
\title{SHA1 Digest R Objects}
\description{
   Compute the SHA1 digest of an object in the R session.
}
\usage{
SHA1(x, wo.vtag = TRUE)
}
\arguments{
   \item{x}{any R object.}
   \item{wo.vtag}{Indicates whether the vtag should be removed before
   computing the SHA1 digest.}
}
\details{
   For a \code{character} vector \code{x}, \code{SHA1(x)} is equivalent
   to \code{digest::digest(x, algo="sha1", serialize=FALSE)}. For other
   types, it is equivalent to \code{digest::digest(x, algo="sha1")}.
}
\value{
   The SHA1 digest is a 40-character string (character \code{vector}
   of length 1) of hexadecimal decimal symbols, i.e. numbers from 0
   to 9 and letters from "a" to "f".

   The SHA1 digest is computed by a cryptographic hash function so
   that even close inputs have very different digests. Two objects
   with the same SHA1 digest are extremely likely to be identical,
   which allows to ascertain that the content of a variable has
   not changed over time or between different R sessions.
}
\note{
   The SHA1 digest is computed from the objects as they are loaded in
   memory, and not from their character representation.
}
\examples{
# Different representations have different SHA1 digest.
SHA1(as.integer(1))
SHA1(as.double(1))
SHA1(as.character(1))
}
