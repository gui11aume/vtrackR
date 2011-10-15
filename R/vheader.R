vheader <- function(x, comment.char="#", extrafields="") {
# Return a vheader from the vtag attribute of 'x'.
# The vheader is a character vector of length one, where
# each line is prefixed by a comment character.

   if (is.null(attr(x, "vtag"))) {
      # If x has no vtag, return NULL.
      return(NULL);
   }
   
   # The header should contain the fields from "environment",
   # "context", "self", and 'extrafields' if present.
   if (comment.char != "") {
      # Append a space after comment character (for esthetics).
      comment.char <- paste(comment.char, " ", sep="");
   }
   pairlist <- attr(x, "vtag")[c(
         "environment",
         "context",
         "self",
         extrafields
      )];

   # Special treatment for the function source if present.
   if (!is.null(pairlist$context[["function source"]])) {
      # Prettify the code.
      source <- paste("\n", paste(sub(
            "^",
            comment.char,
            pairlist$context[["function source"]]
         ),
         collapse="\n"
      ), sep = "");
      pairlist$context[["function source"]] <- source;
   }

   return (paste(collapse="",
      sapply(pairlist, FUN = function(pairs) {
         # Concatenate key/value pairs.
         paste(
            sub("^", comment.char, names(pairs)),
            sub("$", "\n", pairs),
            sep=": ",
            collapse="");
         })
   ));
}
