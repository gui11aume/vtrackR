write <- function(x, file = "data",
   ncolumns = if (is.character(x)) 1 else 5,
   append = FALSE, sep = " ") {

   isroot <- Sys.info()[["user"]] == "root";

   if (!append && is.null(attr(x, "vtag")) && !isroot) {
      # No vtag, create a session vheader.
      warning('no vtag, writing session vheader');
      attr(x, "vtag") <- vsessionInfo();
      attr(x, "vtag")[["self"]][["self SHA1"]] <- SHA1(x);
   }
   base::cat(vheader(x), file=file);
   # Port the wrapper from base package.
   base::cat(x, file = file, sep = c(rep.int(sep, ncolumns - 1), "\n"),
      append = TRUE);
}
