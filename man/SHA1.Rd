\name{SHA1}
\alias{SHA1}
\title{SHA1 Digest R Objects}
\description{
   Compute the SHA1 digest of a object in the R session.
}
\usage{
SHA1(obj, ...)
}
\arguments{
   \item{obj}{any R object.}
   \item{...}{parameters passed to digest::digest.}
}
\details{
   \code{SHA1} is a wrapper for \code{digest::digest(obj, algo="sha1")}.
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
\seealso{
   \code{\link{digest::digest}}
}
\examples{
# Different representations have different SHA1 digest.
SHA1(as.integer(1));
SHA1(as.double(1));
SHA1(as.character(1));

# Files have yet another representation.
write(1, file="tmp.txt");
SHA1(scan("tmp.txt", what=0));
SHA1(scan("tmp.txt", what=""));
SHA1("tmp.txt", file=TRUE);
}
