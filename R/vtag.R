vtag <- function(x) {
# Add a 'vtag' attribute to an object. In function definitions,
# replace 'return(x)' by 'return(vtag(x))'.

   # Obtain environment and session info.
   info <- vsessionInfo();

   # Retrieve the parent call, ie the call to the function
   # where 'vtag()' is embedded.
   parent_call <- match.call(call=sys.call(sys.parent()));

   info[["context"]] <- list();
   # Deparse and prettify the call.
   info$context[["call"]] <- gsub("[[:space:]][[:space:]]*", " ",
      paste(deparse(parent_call), collapse=""));

   # The parent function is in 'parent_args[1]', and its
   # arguments in 'parent_args[2]', 'parent_args[3]', ...
   parent_args <- as.list(parent_call);
   closure <- get(as.character(parent_args[1]), pos=parent.frame());

   # Get the 'package' of the parent (can be .GlobalEnv).
   package <- environmentName(environment(closure));

   # Put SHA1 of arguments in sub-list to avoid name collissions.
   for (arg in parent_args) {
      # Try to retrieve object FROM PARENT-OF-PARENT ENVIRONMENT. 
      if (is.symbol(arg)) {
         info$context[[paste(arg,"SHA1")]] <- SHA1(
               get(as.character(arg), envir=parent.frame(n=2))
         );
      }
   }

   if (package != "R_GlobalEnv") {
      info$context[["package"]] <- package;
      info$context[["version"]] <-
         packageDescription(package)[["Version"]];
      # Add the (git) revision info if present.
      if (!is.null(packageDescription(package)[["Revision"]])) {
         info$context[["revision"]] <-
            packageDescription(package)[["Revision"]];
      }
   }
   else {
      # The function is not tracked. Copy the source.
      info$context[["function source"]] <- 
         deparse(body(fun=sys.function(sys.parent())));
   }

   # Add info as attribute (including SHA1 of x itself).
   info[["self"]][["self SHA1"]] <- SHA1(x);

   # Collect history.
   info[["history"]][["full"]] <- gethistory();


   attr(x, "vtag") <- info;

   return(x);
}
