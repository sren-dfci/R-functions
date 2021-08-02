round_p_value <- function(p) {
  ifelse(
    p < .001,
    rep("<0.001", length(p)),
    ifelse(
      (.001 <= p & p <= .009) | (.045 <= p & p <= 0.05),
      round(p, 3),
      round(p, 2)
    )
  )
}