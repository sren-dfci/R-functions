fstats <- function(.col, .digits = NULL) {
  .tbl <- table(.col)
  .tbl_freq <- as.data.frame(.tbl)$Freq
  .tbl_perc <- as.data.frame(prop.table(.tbl))$Freq
  .digits <- ifelse(is.null(.digits), 0, .digits)
  .tbl_perc <- round(.tbl_perc * 100, .digits)
  return(paste0(.tbl_freq, " (", .tbl_perc, "%)"))
}

nstats <- function(.col, .digits = NULL) {
  .digits <- ifelse(is.null(.digits), 0, .digits)
  .q <- round(
    quantile(.col, seq(0, 1, 0.25), na.rm = T),
    digits = .digits
  )
  .m <- round(mean(.col, na.rm = T), digits = .digits)
  r1 <- paste(.q, collapse = ", ")
  return(paste0(r1, "; ", .m))
}

missStats <- function(.col, .digits = NULL) {
  .digits <- ifelse(is.null(.digits), 0, .digits)
  .n <- sum(is.na(.col))
  .N <- length(.col)
  .perc <- round(.n / .N * 100, .digits)
  return(paste0(.n, " (", .perc, "%)"))
}

summaryTable <- function(.df, .vars, .strata_var = NULL, .digits = NULL) {
  # , .add_na = c("ifany", "no")
  if (!"data.frame" %in% class(.df)) {
    stop(".df needs to be a data.frame")
  }
  # only support one strata column
  if (length(.strata_var) > 1) {
    stop("only support one strata variable")
  }
  # if .vars not specified, use all columns
  if (missing(.vars)) {
    .vars <- colnames(.df)
    if (!is.null(.strata_var)) {
      .vars <- .vars[.vars != .strata_var]
    }
  }
  # .add_na <- match.arg(.add_na)
  # extract all levels from all vars
  .levels <- vector("list", length(.vars) + 1)
  names(.levels) <- c("N", .vars)
  # save outcomes to list
  .outcomes <- vector("list", length(.vars) + 1)
  names(.outcomes) <- c("N", .vars)
  # N
  .levels[["N"]] <- ""
  if (is.null(.strata_var)) {
    .outcomes[["N"]] <- nrow(.df)
  } else {
    for (s in sort(unique(.df[[.strata_var]]))) {
      .outcomes[["N"]][[s]] <- sum(.df[[.strata_var]] == s, na.rm = TRUE)
    }
  }
  # iterate over cols
  for (v in .vars) {
    .col <- .df[[v]]
    ## Levels
    # numeric variables
    if (is.numeric(.col)) {
      # if any missing
      if (anyNA(.col)) {
        vlevels <- c("quantile; mean", "missing")
      } else {
        vlevels <- "quantile; mean"
      }
    # character to factor
    } else if (is.character(.col)) {
      .col <- factor(.col, levels = sort(unique(.col[!is.na(.col)])))
    # logical to factor
    } else if (is.logical(.col)) {
      .col <- factor(.col, levels = c(TRUE, FALSE))
    }
    # factor or ordered factor
    if (is.factor(.col) || is.ordered(.col)) {
      # whether add missing to factor variables or not
      .col <- addNA(.col, ifany = TRUE)
      vlevels <- levels(.col)
    }
    # add levels to the list
    .levels[[v]] <- vlevels
    ## Statistics
    # if has strata variable
    if (!is.null(.strata_var)) {
      for (s in sort(unique(.df[[.strata_var]]))) {
        .sub_col <- .col[.df[[.strata_var]] == s]
        if ("quantile; mean" %in% vlevels) {
          if ("missing" %in% vlevels) {
            .outcome <- c(
              nstats(.sub_col, .digits),
              missStats(.sub_col, .digits)
            )
          } else {
            .outcome <- nstats(.sub_col, .digits)
          }
        } else {
          .outcome <- fstats(.sub_col, .digits)
        }
        .outcomes[[v]][[s]] <- .outcome
      }
    } else {
      if ("quantile; mean" %in% vlevels) {
        if ("missing" %in% vlevels) {
          .outcome <- c(
            nstats(.col, .digits),
            missStats(.col, .digits)
          )
        } else {
          .outcome <- nstats(.col, .digits)
        }
      } else {
        .outcome <- fstats(.col, .digits)
      }
      .outcomes[[v]] <- .outcome
    }
  }
  if (!is.null(.strata_var)) {
    .outcomes_df <- data.frame(
      do.call(rbind, lapply(.outcomes, function(x) do.call(cbind, x)))
    )
    names(.outcomes_df) <- paste0(
      .strata_var, ": ",
      sort(unique(.df[[.strata_var]]))
    )
  } else {
    .outcomes_df <- data.frame(as.vector(do.call("c", .outcomes)))
    names(.outcomes_df) <- "Values"
  }
  .outcomes_df <- cbind(
    data.frame(
      Var = rep(names(.levels), lengths(.levels)),
      Levels = as.vector(unlist(.levels))
    ),
    .outcomes_df
  )
  return(.outcomes_df)
}
