#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param x object inherited from ryx function
#'@param ... (optional), any additional parameters
#'@returns dataframe with correlations of y variable with all x-variables, p-value, and level of significance for each x variable.
#'@import MASS
#'
#'@examples
#'library(MASS)
#'x <- ryx(Boston, y = "medv")
#'x
#'x <- ryx(iris, y = "Sepal.Length")
#'x

print.ryx <- function(x,...){
  if(!inherits(x, "ryx")){
    stop("This functon requires an object created by ryx")
  }

  #add title
  cat("Correlations of", x$y, "with\n")

  #remove the cor column
  print(x$df, row.names =  F)
}
