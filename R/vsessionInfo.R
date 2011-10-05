vsessionInfo <- function() {
# Function that gathers information about the current 
# environment, user, date/time as well as the R-session 
# itself (incl. loaded packages). Returns info as list().
   info <- list(
      "platform" = paste(
            Sys.info()[c("sysname", "nodename", "release")],
            collapse = " "),
      "directory" = getwd(),
      "user" = Sys.info()[["login"]],
      "date" = strftime(Sys.time()),
      "sessionInfo" = paste(
            R.version[c("major", "minor")],
            collapse = "."
      )
   );

   return(info);
}

