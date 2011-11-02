save <- function(..., list = character(),
   file = stop("'file' must be specified"),
   ascii = FALSE, version = NULL, envir = parent.frame(),
   compress = !ascii, compression_level,
   eval.promises = TRUE, precheck = TRUE) {

   isroot <- Sys.info()[["user"]] == "root";

   # Arguments prior to R version 2.13.
   passed <- list(
         file=file, ascii=ascii, version=version,
         compress=compress,
         eval.promises=eval.promises, precheck=precheck
   );
   # Argument added in R version 2.13.
   if (!missing(compression_level)) {
      passed[["compression_level"]] <- compression_level;
   }

   # Two lines copied from base::save.
   names <- as.character(substitute(list(...)))[-1L];
   list <- c(list, names);

   # Create a temp environment for vtagging objects.
   tmp.env <- new.env(parent=envir);
   # Create session vtags on the fly and warn if needed.
   for (name in list) {
      if (is.null(attr(get(name), "vtag")) && !isroot) {
         warning(paste(
            "argument", name, "has no vtag: writing session vheader."
         ));
         info <- vsessionInfo();
         # Copy in this environment and vtag the object.
         assign(
            name,
            "attr<-"(get(name), "vtag", info),
            envir = tmp.env
         );
      }
   }

   # Back to 'base::save()'.
   do.call(base::save, args=c(list, passed, list(envir=tmp.env)))

}
