write.table <- function(x, file = "", append = FALSE, quote = TRUE,
   sep = " ", eol = "\n", na = "NA", dec = ".", row.names = TRUE,
   col.names = TRUE, qmethod = c("escape", "double"), ...) {

   isroot <- Sys.info()[["user"]] == "root";

   passed <- list(
      quote = quote,
      sep = sep,
      eol = eol,
      na = na,
      dec = dec,
      row.names = row.names,
      col.names = col.names,
      qmethod = qmethod,
      ...
   );

   if (append || isroot) {
      # Just an append or root: call 'utils::write()'.
      do.call(
         utils::write.table,
         args=c(list(x=x, file=file, append=append), passed)
      );
   }  
   else {
      # Otherwise there should be a vheader.
      if (is.null(attr(x, "vtag"))) {
         # Oops... Forgot to vtag the variable?
         warning('"x" has no vtag, writing session vheader');
         attr(x, "vtag") <- vsessionInfo();
         attr(x, "vtag")[["self"]][["self SHA1"]] <- SHA1(x);
         x.arg <- as.list(match.call())$x;
         if (is.symbol(x.arg)) {
            addcomment(x, "history",
                  paste(relevanthistory(as.character(x.arg)),
                         collapse = "; "
            ));
         }
      }
      # Now 'x' has a vtag, format a vheader and write.
      base::cat(vheader(x), file=file)

      # And finally write 'x'...
      # ... but this will trigger a warning that we need to catch.

      ignoreAppendW <- function(w) {
         # Catch the "appending column names to file" warning.
         if (grepl("appending column names to file", w$message)) {
            invokeRestart("muffleWarning");
         }
      }
      withCallingHandlers(
         tryCatch(
            expr = do.call(
               utils::write.table,
               args=c(list(x=x, file=file, append=TRUE), passed)
            ),
            error = function(e) e
         ),
         warning = ignoreAppendW
      );
   }
}
