relevanthistory <- function(argname) {
# Return the lines of history that contain the argument.

   # Lines copied from function 'utils::history'.
   # By the way, isn't it a strange piece of code?
   file1 <- tempfile("Rrawhist");
   savehistory(file1);
   rawhist <- readLines(file1);
   unlink(file1);

   return (grep(argname, rawhist, value = TRUE));

}
