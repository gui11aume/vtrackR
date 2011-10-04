vcheck <- function(X) {
# Check the SHA1 digest of 'X' against its stored vtag attribute, if present.
# Returns TRUE if the same, FALSE otherwise.
# We may want to give a warning/error as well.

  # Assert that the digest package is installed and loaded.
  stopifnot(require (digest));

  # Restore warnings upon exit (will turn them off later).
  opt <- options();
  on.exit(options(opt));

  # Check vtag.
  if (!is.null(X)) {
    if (is.null(attributes(X)$vtag) || is.null(attributes(X)$vtag[["object SHA1"]])) {
      warning("Can't check: no vtag exists for this object!");
    } else {
      vtag_sha1 <- attr(X, "vtag")[["object SHA1"]];
      attr(X, "vtag") <- NULL;
      obj_sha1 <- digest(X, "sha1");
      return(identical(vtag_sha1, obj_sha1));
    }
  }

  return(FALSE);
}


