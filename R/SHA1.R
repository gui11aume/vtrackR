SHA1 <- function(x) {
   # Light version of the 'digest' function from package
   # digest. Thanks to Dirk Eddelbuettel for the code.
   # Serialize 'x' if not character.
   skip <- 0;
   if (!is.character(x)) {
      x <- serialize(x, connection=NULL, ascii=FALSE);
      skip <- 14;
   }
   return (.Call("sha1", x, as.integer(skip), PACKAGE="vtrackR"));
   
}
