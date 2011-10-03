SHA1 <- function(obj, ...) {
# Wrapper for the SHA1 digest from the digest package.
   require(digest);
   return (digest(obj=obj, algo='sha1', ...));
}
