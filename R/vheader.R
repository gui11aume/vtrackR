vheader <- function(call) {
# Produce a header with version tracking information.
# Invoke as 'header(match.call())' within a function and
# use output as header of output files.

   # Turn off warnings for this function. Restore
   # options upon exit.
   opt <- options();
   options('warn' = -1);
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
      "directory" = getwd(),
      "user" = Sys.info()[["login"]],
      "date" = strftime(Sys.time()),
      "R version" = paste(
            R.version[c("major", "minor")],
            collapse = "."
      ),
      "platform" = paste(
            Sys.info()[c("sysname", "nodename", "release")],
            collapse = " ")
   );

   try (
      # Skip if called closure is not in a package.
      info[[paste(package, "version")]] <-
            packageDescription(package)[["Version"]],
      silent = TRUE
   );

   require (digest);
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
