# So, here is a function that takes as
# input the name of a package. It tests whether the package has been downloaded -
# "installed" - from the R code repository. If it has not yet been downloaded/installed,
# the function takes care of this. Then we use a new function, called require(), to
# prepare the package for further use. Let’s call our function "EnsurePackage" because
# it ensures that a package is ready for us to use.

EnsurePackage <- function(x){
  x <- as.character(x)
  
  if(!require(x,character.only=TRUE)) {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only = TRUE)
  }
}