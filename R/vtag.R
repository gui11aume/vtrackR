vtag <- function(...) {
# Produce a vtag with version tracking information and add as
# attribute 'vtag'.
# Invoke as 'vtag(obj)' within a function and use output as 
# new instance of 'obj', including a (new) 'vtag' attribute.

  # Assert that the digest package is installed and loaded.
  stopifnot(require (digest));

  # Restore warnings upon exit (will turn them off later).
  opt <- options();
  on.exit(options(opt));

  # Object to be tagged.
  X <- list(...)$X;

  # Retrieve the parent call.
  # TODO: check what happens if it doesn't exist.
  parent_call <- match.call(call=sys.call(sys.parent()));

  # Convert the parent call to characters. The called closure
  # (function) is in 'parent_args[1]', and the arguments in
  # 'parent_args[2]', 'parent_args[3]', ...
  parent_args <- as.character(parent_call);

  # Get the package the function is taken from.
  package <- environmentName(environment(get(parent_args[1],
      pos=parent.frame())));

  # Gather base information.
  info <- list(
      "platform" = paste(
            Sys.info()[c("sysname", "nodename", "release")],
            collapse = " "),
      "directory" = getwd(),
      "user" = Sys.info()[["login"]],
      "date" = strftime(Sys.time()),
      "R version" = paste(
            R.version[c("major", "minor")],
            collapse = "."
      )
  );


  # Save parent call to function.
  info[["call"]] <- deparse(parent_call);

  ## Put SHA1 value of arguments in sub-list to avoid name collissions.
  info[["args"]] <- list();

  # Turn off warnings in what follows.
  options('warn' = -1);

  for (arg in parent_args) {
     # Try to retrieve object FROM PARENT-OF-PARENT ENVIRONMENT. 
     # Will (silently) fail if not properly named.
     arg_obj <- NULL;
     try(arg_obj <- get(arg, envir=parent.frame(n=2)), silent=TRUE);
     

     # Add SHA1 digest if argument is named (otherwise skip).
     if (!is.null(arg_obj)) {
     # Clear vtag before calculating SHA1 digest, if it exists.
        if (!is.null(attributes(arg_obj)$vtag)) {
            attr(arg_obj, "vtag") <- NULL;
        }
        # Calculate and add SHA1 digest.
        info$args[[paste(arg,"SHA1")]] <- digest(arg_obj, "sha1");
     }
  }

  try (
    # Skip if called closure is not in a package.
    info[[paste(package, "version")]] <-
          packageDescription(package)[["Version"]],
      silent = TRUE
  );

  # Add info as attribute (including SHA1 value of object itself).
  if (!is.null(X)) {
     if (!is.null(attributes(X)$vtag)) attr(X, "vtag") <- NULL;
     info[["object SHA1"]] <- digest(X, "sha1");
     attr(X, "vtag") <- info;
  }

  invisible(X);

}


