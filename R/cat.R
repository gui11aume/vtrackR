cat <- function(x, file = "", sep = " ", fill = FALSE, labels = NULL,
      append = FALSE) {

   isroot <- Sys.info()[["user"]] == "root";

   # Same prototype as 'base::cat()'.
   if (append || file == "" || isroot) {
      # Just an append, a screen display, or root user:
      # call 'base::write()'.
      base::cat(x, file=file, sep=sep, fill=fill, labels=labels,
            append=append);
   }
   else {
      # Otherwise there should be a vheader.
      if (is.null(attr(x, "vtag"))) {
         # Oops... Forgot to vtag the variable?
         warning('no vtag, writing session vheader');
         attr(x, "vtag") <- vsessionInfo();
         attr(x, "vtag")[["self"]][["self SHA1"]] <- SHA1(x);
         x.arg <- as.list(match.call())$x;
         if (is.symbol(x.arg)) {
            addcomment(x, "history", paste(
               relevanthistory(as.character(x.arg)),
               collapse = "; "
            ));
         }
      }
      # Now 'x' has a vtag, format a vheader and write.
      base::cat(vheader(x), file=file, sep=sep, fill=fill,
         labels=labels, append=append);
      
      # And finally write 'x'.
      base::cat(x, file=file, sep=sep, fill=fill,
         labels=labels, append=TRUE);
   }
}
