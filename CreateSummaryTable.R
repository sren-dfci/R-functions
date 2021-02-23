fstats <- function(.col, .digits = NULL) {
  .tbl <- table(.col)
  .tbl_freq <- as.data.frame(.tbl)$Freq
  .tbl_perc <- as.data.frame(prop.table(.tbl))$Freq
  .digits <- ifelse(is.null(.digits), 0, .digits)
  .tbl_perc <- round(.tbl_perc * 100, .digits)
  return(paste0(.tbl_freq, " (", .tbl_perc, "%)"))
}

nstats <- function(.col, .digits = NULL, .median = TRUE) {
  if (.median) {
    .q <- quantile(.col, c(0, 0.5, 1), na.rm = TRUE)
  } else {
    .m <- mean(.col, na.rm = TRUE)
    .sd <- sd(.col, na.rm = TRUE)
    .q <- c(.m - 1.96 * .sd, .m, .m + 1.96 * .sd)
  }
  .digits <- ifelse(is.null(.digits), 0, .digits)
  .q <- round(.q, .digits)
  return(paste0(.q[2], " (", .q[1], " - ", .q[3], ")"))
}

missStats <- function(.col, .digits = NULL) {
  .digits <- ifelse(is.null(.digits), 0, .digits)
  .n <- sum(is.na(.col))
  .N <- length(.col)
  .perc <- round(.n / .N * 100, .digits)
  return(paste0(.n, " (", .perc, "%)"))
}

summaryTable <- function(.df, .vars, .strata_var = NULL, .mean_vars = NULL,
                 .digits = NULL, .add_na = c("ifany", "no")) {
  # only support one strata column
  # if .vars not specified, use all columns
  if (missing(.vars)) {
    .vars <- colnames(.df)
    if (!is.null(.strata_var)) {
      .vars <- .vars[.vars != .strata_var]
    }
  }
  .add_na <- match.arg(.add_na)
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
    # numeric variables
    if (is.numeric(.col)) {
      # median or mean
      vlevels <- ifelse(
        v %in% .mean_vars, # grepl("age", v, ignore.case = TRUE) ||
        "Mean (SD)",
        "Median (Range)"
      )
      # whether add missing or not
      if (.add_na == "ifany" && anyNA(.col)) {
        vlevels <- c(vlevels, "missing")
      }
    } else if (is.character(.col)) {
      .col <- factor(.col, levels = sort(unique(.col[!is.na(.col)])))
    } else if (is.logical(.col)) {
      .col <- factor(.col, levels = c(TRUE, FALSE))
    }
    if (is.factor(.col) || is.ordered(.col)) {
      # whether add missing to factor variables or not
      if (.add_na == "ifany") {
        .col <- addNA(.col, ifany = TRUE)
      }
      vlevels <- levels(.col)
    }
    # add levels to the list
    .levels[[v]] <- vlevels
    # if has strata variable
    if (!is.null(.strata_var)) {
      for (s in sort(unique(.df[[.strata_var]]))) {
        .sub_col <- .col[.df[[.strata_var]] == s]
        if ("Median (Range)" %in% vlevels) {
          if (.add_na == "ifany" && anyNA(.col)) {
            .outcome <- c(
              nstats(.sub_col, .digits, TRUE), 
              missStats(.sub_col, .digits)
            )
          } else {
            .outcome <- nstats(.sub_col, .digits, TRUE)
          }
        } else if ("Mean (SD)" %in% vlevels) {
          if (.add_na == "ifany" && anyNA(.col)) {
            .outcome <- c(
              nstats(.sub_col, .digits, FALSE), 
              missStats(.sub_col, .digits)
            )
          } else {
            .outcome <- nstats(.sub_col, .digits, FALSE)
          }
        } else {
          .outcome <- fstats(.sub_col, .digits)
        }
        .outcomes[[v]][[s]] <- .outcome
      }
    } else {
      if ("Median (Range)" %in% vlevels) {
        if (.add_na == "ifany" && anyNA(.col)) {
            .outcome <- c(
              nstats(.col, .digits, TRUE), 
              missStats(.col, .digits)
            )
          } else {
            .outcome <- nstats(.col, .digits, TRUE)
          }
      } else if ("Mean (SD)" %in% vlevels) {
        if (.add_na == "ifany" && anyNA(.col)) {
            .outcome <- c(
              nstats(.col, .digits, FALSE), 
              missStats(.col, .digits)
            )
          } else {
            .outcome <- nstats(.col, .digits, FALSE)
          }
      } else {
        .outcome <- fstats(.col, .digits)
      }
      .outcomes[[v]] <- .outcome
    }
  }
  .outcomes_df <- if (!is.null(.strata_var)) {
    data.frame(
      do.call(rbind, lapply(.outcomes, function(x) do.call(cbind, x)))
    )
  } else {
    data.frame(as.vector(do.call(c, .outcomes)))
  }
  .outcomes_df <- cbind(
    data.frame(
      Var = rep(names(.levels), lengths(.levels)), 
      Levels = as.vector(unlist(.levels))
    ),
    .outcomes_df
  )
  setNames(.outcomes_df, c("Var", "Levels", names(.outcomes[[1]])))
}