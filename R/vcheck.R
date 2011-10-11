vcheck <- function(x) {
# Check the SHA1 digest of 'x' against its vtag attribute.
# Returns TRUE if the same, FALSE otherwise.
# We may want to give a warning/error as well.

   # Check vtag.
   if (is.null(attr(x, 'vtag')) ||
         is.null(attr(x, 'vtag')[["self"]][["self SHA1"]])) {
      warning("object has no vtag or no SHA1 digest");
   }
   else {
      sha1_vtag <- attr(x, "vtag")[["self"]][["self SHA1"]];
      # Erase vtag, as it modifies the SHA1 digest.
      attr(x, "vtag") <- NULL;
      sha1_x <- SHA1(x);
      return(identical(sha1_vtag, sha1_x));
   }

  return(FALSE);

}
