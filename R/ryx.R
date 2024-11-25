#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param y a numeric variable
#'@param x string, a list of numeric variables. Defaults to all variables in the dataset provided.
#'@returns dataframe with correlations of y variable with all x-variables, p-value, and level of significance for each x variable.
#'
#'@examples
#'ryx(mtcars, "mpg")
#'ryx(iris, "Sepal.Length")
#'library(MASS)
#'ryx(Boston, "medv")


ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}
