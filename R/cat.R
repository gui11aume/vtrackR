cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
      append = FALSE) {

   isroot <- Sys.info()[["user"]] == "root";

   # Same prototype as 'base::cat()'.
   if (append || file == "" || isroot) {
      # Just an append, a screen display, or root user:
      # call 'base::write()'.
      base::cat(..., file=file, sep=sep, fill=fill, labels=labels,
            append=append);
   }
   else {
      # Otherwise there should be a vheader.
      if (length(list(...)) > 1) {
         # Cannot track properly if several objects are written.
         stop("can write only 1 object per call")
      }
      x <- ..1;
      if (is.null(attr(x, "vtag"))) {
         # Oops... Forgot to vtag the variable?
         warning('no vtag, writing session vheader');
         attr(x, "vtag") <- vsessionInfo();
         attr(x, "vtag")[["self"]][["self SHA1"]] <- SHA1(x);
      }
      # Now 'x' has a vtag, format a vheader and write.
      base::cat(vheader(x), file=file, sep=sep, fill=fill,
         labels=labels, append=append);
      
      # And finally write 'x'.
      base::cat(x, file=file, sep=sep, fill=fill,
         labels=labels, append=TRUE);
   }
}
