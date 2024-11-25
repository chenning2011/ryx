#'@title Correlation Table
#'@description Creates a correlation table using numeric variables.
#'@export
#'@param data a data frame
#'@param x object inherited from ryx function
#'@param ... (optional), any additional parameters
#'@returns method used, dataset used, variables used for outliers detected, indices of any detected outliers, scores for the outliers, and values for optional parameters
#'@import ggplot2
#'@import dplyr
#'@import MASS
#'
#'@examples
#'library(MASS)
#'x <- ryx(Boston, "medv")
#'plot(x)


plot.ryx <- function(x,...){
  if(!inherits(x, "ryx")){
    stop("This functon requires an object created by ryx")
  }
  library(dplyr)
  library(ggplot2)

  #arranging data in correct order
  df <- x$df %>%
    arrange(abs(r))

  #turning variable into ordered factor in order of abs(r)
  df <- df %>%
    mutate(var2 = factor(variable, ordered = T, levels = df$variable))

  #creating neg/pos var
  df <- df %>%
    mutate(negative = (ifelse(df$r < 0, "negative", "positive")))

  #creating y name
  yname <- as.character(x$y)

  #plotting code
  ggplot(data = df)+
    geom_point(aes(x = abs(r), y = var2, color = negative), size = 3)+ #adding points, with negative as var
    geom_segment(aes(x= 0, xend = abs(r), y = var2, yend = var2), color = "gray")+ #adding lines
    theme_bw()+ #changing the theme
    theme(panel.grid.major = element_line(linetype = "dashed"), #changing theme
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.y.left = element_line(color = "lightgray"),
          axis.line.x.top = element_line(color = "lightgray"),
          axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank())+
    scale_x_continuous(limits = c(0, 1), n.breaks = 10, sec.axis = dup_axis(name = NULL))+ #changing x-axis scale
    scale_color_manual("Direction", values = c("red", "blue"))+ #changing colors of negative/positive
    labs(x = "Correlation (absolute value)", y = "Variables", title = paste("Correlations with", yname)) #adding label
}



