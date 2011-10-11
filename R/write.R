write <- function(x, file = "data",
   ncolumns = if (is.character(x)) 1 else 5,
   append = FALSE, sep = " ") {
   # Port the wrapper from base package.
   cat(x, file = file, sep = c(rep.int(sep, ncolumns - 1), "\n"),
      append = append);
}
