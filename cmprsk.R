# get estimated CIF at a specific timepoint
cifCI <- function(.cuminc, .tp) {
  require(cmprsk)
  .ls <- timepoints(.cuminc, .tp) # a list
  .df <- data.frame(.ls[["est"]], .ls[["var"]])
  colnames(.df) <- c("est", "var")
  .df$lower <- .df$est - 1.96 * sqrt(.df$var)
  .df$upper <- .df$est + 1.96 * sqrt(.df$var)
  return(.df)
}

# convert the cuminc object to a dataframe (for plotting)
convert_cuminc <- function(.cuminc) {
  nms <- names(.cuminc)
  event_names <- nms[nms != "Tests"]
  .df <- data.frame(grp = NULL, event = NULL, time = NULL, est = NULL)
  for (v in event_names) {
    grp <- strsplit(v, "\\s")[[1]][1]
    event <- strsplit(v, "\\s")[[1]][2]
    time <- .cuminc[[v]]$time
    est <- .cuminc[[v]]$est
    tmp <- data.frame(
      "grp" = grp, "event" = event, "time" = time, "est" = est
    )
    .df <- rbind(.df, tmp)
  }
  return(.df)
}

round_p_value <- function(p) {
  ifelse(
    p < .001,
    rep("<0.001", length(p)),
    ifelse(
      (.001 <= p & p <= .01) | (.045 <= p & p <= 0.05),
      round(p, 3),
      round(p, 2)
    )
  )
}

summ_crr = function(.crr, .digits = 3) {
  .coef <- .crr$coef
  .std <- sqrt(diag(.crr$var))
  .p <- 2 * (1 - pnorm(abs(.coef) / .std))
  .lower_ci <- exp(.coef - 1.96 * .std)
  .upper_ci <- exp(.coef + 1.96 * .std)
  .res <- data.frame(
    hr = round(exp(.coef), .digits),
    lo = round(.lower_ci, .digits),
    up = round(.upper_ci, .digits),
    pv = round_p_value(.p)
  )
  rownames(.res) <- names(.coef)
  return(.res)
}

