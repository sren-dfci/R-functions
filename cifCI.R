
# cmprsk package only returns point estimate and variance for each time point,
# this function calculates the 95% CI with an assigned time point.

cifCI <- function(.cuminc, .tp) {
  require(cmprsk)
  .ls <- timepoints(.cuminc, .tp) # a list
  .df <- data.frame(.ls[["est"]], .ls[["var"]])
  colnames(.df) <- c("est", "var")
  row_names <- rownames(.df)
  .df$lower <- .df$est - 1.96 * sqrt(.df$var)
  .df$upper <- .df$est + 1.96 * sqrt(.df$var)
  return(.df)
}