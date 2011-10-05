write <- function(x, file = "data",
      ncolumns = if(is.character(x)) 1 else 5,
      append = FALSE, sep = " ") {
   # Same prototype as the 'base::write()' function for
   # a complete replacement.
   if (append) {
      # Just an ppend: call 'base::write()'.
      base::write(x=x, file=file, ncolumns=ncolumns,
            append=TRUE, sep=sep);
      return(NULL);
   }
   # Not an append, there should be a vheader.
   if (is.null(attr(x, "vtag"))) {
      # Oops... Forgot to vtag the variable?
      warning('"x" has no vtag, writing session vheader');
      attr(x, "vtag") <- vsessionInfo();
      attr(x, "vtag")[["args"]][["self SHA1"]] <-
            digest(x, algo="sha1");
   }
   # Now 'x' has a vtag, format a vheader and write.
   base::write(vheader(x), file=file, ncolumns=1,
         append = FALSE, sep=sep);
   # And finally write 'x'.
   # TODO: should trigger a warning, what to do with that?
   base::write(x, file=file, ncolumns=ncolumns,
         append=TRUE, sep=sep);
   return(NULL);
}
