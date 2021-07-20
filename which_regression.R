#' A function that helps user decide which regression is best for a dataframe
#' 
#' @param df
#' @keywords plot
#' @export
#' @examples
#' which_regression()


which_regression <- function(df){
  if(ncol(df) != 2){
    d <- "Please enter a dataframe that has two columns with the first column being the independent variable and the second being the dependent variable"
  }
  else {
    names(df) <- c("x", "y")
    lm <- lm(y ~ x, data=df)
    
    df$x2 <- df$x^2
    quad <- lm(y ~ x + x2, data=df)
  
    if (summary(lm)$r.squared < summary(quad)$r.squared){
      d <- summary(quad)
      model <- lm$model
    }
    else { 
      d <- summary(lm)
      model <- df$model }
  }
  return(d)
  return(model)
}
