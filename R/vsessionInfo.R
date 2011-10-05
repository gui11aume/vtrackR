vsessionInfo <- function() {
# Function that gathers information about the current 
# environment, user, date/time as well as the R-session 
# itself (incl. loaded packages). Returns info as list().
   info <- list();
   info[["environment"]] <- list(
      "platform" = paste(
            Sys.info()[c("sysname", "nodename", "release")],
            collapse = " "),
      "directory" = getwd(),
      "user" = Sys.info()[["login"]],
      "date" = strftime(Sys.time())
   );

   info[["session"]] <- sessionInfo();

   return(info);
}

