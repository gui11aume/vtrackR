vtag <- function(...) {
# Add a 'vtag' attribute to an object. In function definitions,
# replace 'return(x)' by 'return(vtag(X=x))'.

   # Technical section to emulate the prototype 'vtag(X)'.
   if (length(list(...)) != 1) {
      stop('vtag takes exactly one argument');
   }
   argname <- names(list(...));
   if (is.null(argname) || argname == "X") {
      X <- ..1;
   }
   else {
      stop('argument "X" is missing, with no default');
   }

   # Obtain environment and session info.
   info <- vsessionInfo();

   # Retrieve the parent call, ie the call to the function
   # where 'vtag()' is embedded.
   parent_call <- match.call(call=sys.call(sys.parent()));
   info[["context"]] <- list();
   # Deparse and prettify the call.
   info$context[["call"]] <- sub("[[:space:]][[:space:]]*", " ",
      paste(deparse(parent_call), collapse=""));

   # Convert the parent call to characters. The parent
   # function is in 'parent_args[1]', and its arguments in
   # 'parent_args[2]', 'parent_args[3]', ...
   parent_args <- as.character(parent_call);

   # Get the 'package' of the parent (can be .GlobalEnv).
   package <- environmentName(environment(get(parent_args[1],
      pos=parent.frame())));

   # Put SHA1 of arguments in sub-list to avoid name collissions.

   for (arg in parent_args) {
      # Try to retrieve object FROM PARENT-OF-PARENT ENVIRONMENT. 
      # Will (silently) fail if not properly named, e.g. positional
      # data argument like in 'f(2)'.
      arg_obj <- NULL;
      try(
          expr = arg_obj <- get(arg, envir=parent.frame(n=2)),
          silent = TRUE
      );

      # Add SHA1 digest if argument is named (otherwise skip).
      if (!is.null(arg_obj)) {
      # Clear vtag before calculating SHA1 digest, if it exists.
         if (!is.null(attributes(arg_obj)$vtag)) {
            attr(arg_obj, "vtag") <- NULL;
         }
         # Calculate and add SHA1 digest.
         info$context[[paste(arg,"SHA1")]] <- SHA1(arg_obj);
      }
   }

   if (package != "R_GlobalEnv") {
      info$context[[paste(package, "version")]] <-
         packageDescription(package)[["Version"]]
   }

   # Add info as attribute (including SHA1 of X itself).
   if (!is.null(attributes(X)$vtag)) attr(X, "vtag") <- NULL;
   info[["self"]][["self SHA1"]] <- SHA1(X);
   attr(X, "vtag") <- info;

   return(X)
}
