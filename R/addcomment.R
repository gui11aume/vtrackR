addcomment <- function(x, field, comment) {
# Macro to add a comment to a vtagged object x.

   if (!is.character(field)) {
      stop("field must be a character");
   }

   if (is.null(attr(x, "vtag")[["comments"]])) {
      attr(x, "vtag")[["comments"]] <- list();
   }

   # Update x's vtag in this environment.
   attr(x, "vtag")[["comments"]][[field]] <- comment;

   # And overwrite x in the parent environment.
   x.arg <- as.list(match.call())[["x"]];
   if(is.symbol(x.arg)) {
      assign(as.character(x.arg), x, envir=parent.frame(1));
   }

}
