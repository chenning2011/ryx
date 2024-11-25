#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param x object inherited from ryx function
#'@param ... (optional), any additional parameters
#'@returns summary information for ryx object
#'@import MASS
#'
#'@examples
#'library(MASS)
#'x <- ryx(Boston, y = "medv")
#'summary(x)
#'x <- ryx(iris, y = "Sepal.Length")
#'summary(x)


summary.ryx <- function(x,..){
  #checking correct object type
    if(!inherits(x, "ryx")){
      stop("This functon requires an object created by ryx")
    }

  #first line in summary
  cat("Correlating", x$y, "with", x$x, "\n")

  #create median absolute correlation, take median of the r values
  medianr <- median(abs(x$df$r))

  #find range of r values
  maxr <- max(x$df$r)
  minr <- min(x$df$r)

  #second line in summary
  cat("The median absolute correlation was", round(medianr, 3), "with a range from", round(minr, 3), "to", round(maxr, 3))

  #count number of x variables - length of x
  lenx <- length(x$x)
  #find the number of them that are significant at 0.05 level - need to identify those where p is less than 0.05
  sigx <- x$x[x$df$p < 0.05]

  #third line in summary
  cat("\n", length(sigx), "out of", lenx, "variables where significant at the p < 0.05 level.")
}
