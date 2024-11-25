#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param y a numeric variable
#'@param x string, a list of numeric variables. Defaults to all variables in the dataset provided.
#'@returns method used, dataset used, variables used for outliers detected, indices of any detected outliers, scores for the outliers, and values for optional parameters
#'@import ggplot2
#'@import Routliers
#'@import outliers
#'@import dplyr
#'@import dbscan
#'@import isotree
#'@import FNN
#'
#'@examples
#'multiOutliers(mtcarsOutliers, method="mahalanobis", alpha=0.1)
#'multiOutliers(mtcarsOutliers, method="LoF", minPts=5)
#'multiOutliers(mtcarsOutliers, method="kNN", k=5, threshold=.95)
#'multiOutliers(mtcarsOutliers, method="iForest", ntrees = 50)


print.ryx <- function(x,..){
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
