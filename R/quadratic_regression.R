#' A function that creates a quadratic regression for a dataframe
#' 
#' @param df
#' @keywords plot
#' @export
#' @examples
#' quadratic_regression()



quadratic_regression <- function(df){
  if(ncol(df) != 2){
    d <- "Please enter a dataframe that has two columns with the first column being the independent variable and the second being the dependent variable"
  }
  else {
    names(df) <- c("x", "y")
    df$x2 <- df$x^2
    quad <- lm(y ~ x + x2, data=df)
    d <- summary(quad)
  }
  return(d)
}
