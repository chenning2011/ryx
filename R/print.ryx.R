#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param y a numeric variable
#'@param x string, a list of numeric variables. Defaults to all variables in the dataset provided.
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
