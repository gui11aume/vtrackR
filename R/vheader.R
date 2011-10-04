vheader <- function(call) {
# Produce a header with version tracking information.
# Invoke as 'header(match.call())' within a function and
# use output as header of output files.

  # Assert that the digest package is installed and loaded.
  stopifnot(require (digest));

  # Restore warnings upon exit (will turn them off later).
  opt <- options();
  on.exit(options(opt));

  # Convert the call to characters. The called closure
  # (function) is in 'args[1]', and the arguments in
  # 'args[2]', 'args[3]', ...
  args <- as.character(call)

  # Get the package the function is taken from.
  package <- environmentName(environment(get(args[1])));

  # Gather base information.
  info <- list(
      "call" = deparse(call),
      "platform" = paste(
            Sys.info()[c("sysname", "nodename", "release")],
            collapse = " ")
      "directory" = getwd(),
      "user" = Sys.info()[["login"]],
      "date" = strftime(Sys.time()),
      "R version" = paste(
            R.version[c("major", "minor")],
            collapse = "."
      ),
  );

  # Turn of warnings in what follows.
  options('warn' = -1);

  try (
    # Skip if called closure is not in a package.
    info[[paste(package, "version")]] <-
          packageDescription(package)[["Version"]],
      silent = TRUE
  );

  for (arg in args) {
     try (
        # Add SHA1 digest if argument is named (otherwise skip).
        info[[paste(arg,"SHA1")]] <- digest(get(arg), "sha1"),
          silent = TRUE
     );
  }
  
  # Produce the header by pasting key/value pairs from 'info'.
  return (paste(
    sub("^", "# ", names(info)),
    sub("$", "\n", info),
    sep = ": ",
    collapse = ""
  ));
}
