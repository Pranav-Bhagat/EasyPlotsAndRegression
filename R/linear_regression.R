#' A function that creates a linear regression for a dataframe
#' 
#' @param df
#' @keywords plot
#' @export
#' @examples
#' linear_regression()



linear_regression <- function(df){
  if(ncol(df) != 2){
    d <- "Please enter a dataframe that has two columns with the first column being the independent variable and the second being the dependent variable"
  }
  else {
    names(df) <- c("x", "y")
    lm <- lm(y ~ x, data=df)
    d <- summary(lm)
  }
  return(d)
}
