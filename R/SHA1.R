SHA1 <- function(x, wo.vtag = TRUE) {
   # Light version of the 'digest' function from package
   # digest. Thanks to Dirk Eddelbuettel for the code.
   # This should produce the same SHA1 digest as
   # 'digest::digest(x, "sha1")' if x is not a string, and
   # 'digest::digest(x, "sha1", serialize=FALSE)' if it is.
   skip <- 0;
   if (wo.vtag) {
      # Remove potential vtags, before computing SHA1 digest.
      attr(x, "vtag") <- NULL;
   }
   if (!is.character(x) || length(x) > 1) {
      # Serialize 'x' if not character.
      x <- serialize(x, connection=NULL, ascii=FALSE);
      skip <- 14;
   }

   return (.Call("sha1", x, as.integer(skip), PACKAGE="vtrackR"));
   
}
