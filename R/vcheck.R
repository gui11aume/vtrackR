vcheck <- function(X) {
# Check the SHA1 digest of 'X' against its vtag attribute.
# Returns TRUE if the same, FALSE otherwise.
# We may want to give a warning/error as well.

   # Assert that the digest package is installed and loaded.
   stopifnot(require (digest));

   # Restore warnings upon exit (will turn them off later).
   opt <- options();
   on.exit(options(opt));

   # Check vtag.
   if (is.null(attr(X, 'vtag')) ||
         is.null(attr(X, 'vtag')[["self SHA1"]])) {
      warning("object has no vtag or no SHA1 digest");
   }
   else {
      sha1_vtag <- attr(X, "vtag")[["self SHA1"]];
      # Erase vtag, as it modifies the SHA1 digest.
      attr(X, "vtag") <- NULL;
      sha1_X <- digest(X, "sha1");
      return(identical(sha1_vtag, sha1_X));
   }

  return(FALSE);

}
