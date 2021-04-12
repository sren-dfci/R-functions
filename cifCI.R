cifCI <- function(.cuminc, .tp) {
  require(cmprsk)
  .ls <- timepoints(.cuminc, .tp) # a list
  .df <- data.frame(.ls[["est"]], .ls[["var"]])
  colnames(.df) <- c("est", "var")
  .df$lower <- .df$est - 1.96 * sqrt(.df$var)
  .df$upper <- .df$est + 1.96 * sqrt(.df$var)
  return(.df)
}