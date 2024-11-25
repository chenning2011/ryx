#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param y a numeric variable
#'@param x string, a list of numeric variables. Defaults to all variables in the dataset provided.
#'@returns method used, dataset used, variables used for outliers detected, indices of any detected outliers, scores for the outliers, and values for optional parameters
#'
#'@examples
#'multiOutliers(mtcarsOutliers, method="mahalanobis", alpha=0.1)
#'multiOutliers(mtcarsOutliers, method="LoF", minPts=5)
#'multiOutliers(mtcarsOutliers, method="kNN", k=5, threshold=.95)
#'multiOutliers(mtcarsOutliers, method="iForest", ntrees = 50)

