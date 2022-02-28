summarize_factor_var <- function(col, digits, sort = FALSE) {
  n_miss <- sum(is.na(col))
  if (n_miss == length(col)) {
    level <- "na"
    n_p <- paste0(n_miss, " (", round(100, digits), ")")
  } else {
    col_notna <- col[!is.na(col)]
    tbl <- table(col_notna)
    tbl <- as.data.frame(tbl)
    names(tbl) <- c("level", "freq")
    level <- as.character(tbl$level)
    n <- tbl$freq
    if (sort) {
      level <- level[order(n, decreasing = TRUE)]
      n <- sort(n, decreasing = TRUE)
    }
    level <- c(level, "na")
    n <- c(n, n_miss)
    n_total <- sum(n)
    p <- n / n_total * 100
    p <- round(p, digits)
    n_p <- paste0(n, " (", p, ")")
  }
  output <- data.frame(level = level, value = n_p)
  output
}

summarize_num_var <- function(col, digits) {
  n_miss <- sum(is.na(col))
  level <- c("median (range)", "mean (IQR)", "na")
  if (n_miss == length(col)) {
    value <- c("", "", paste0(n_miss, " (", round(100, digits), ")"))
  } else {
    m <- round(mean(col, na.rm = TRUE), digits)
    min <- round(min(col, na.rm = TRUE), digits)
    max <- round(max(col, na.rm = TRUE), digits)
    med <- round(quantile(col, 0.5, na.rm = TRUE), digits)
    liqr <- round(quantile(col, 0.25, na.rm = TRUE), digits)
    uiqr <- round(quantile(col, 0.25, na.rm = TRUE), digits)
    p_miss <- n_miss / length(col) * 100
    p_miss <- round(p_miss, digits)
    value <- c(
      paste0(med, " (", min, " - ", max, ")"),
      paste0(m, " (", liqr, " - ", uiqr, ")"),
      paste0(n_miss, " (", p_miss, ")")
    )
  }
  output <- data.frame(level = level, value = value)
  output
}

create_summ_table <- function(df, vars = NULL, strata_var = NULL, sort = FALSE,
                              digits = 1) {
  require(tibble)
  if (!(is.data.frame(df) & !is_tibble(df))) {
    stop("df needs to be either a data.frame")
  }
  if (!is.null(strata_var)) {
    if (length(strata_var) > 1) {
      stop("only support one strata variable")
    }
    if (!strata_var %in% names(df)) {
      stop("strata var not found")
    }
    df[[strata_var]] <- as.character(df[[strata_var]])
  }
  if (!is.null(vars)) {
    miss_vars <- vars[!vars %in% names(df)]
    if (length(miss_vars) > 0) {
      stop(paste(paste(miss_vars, collapse = ", "), "not found"))
    }
  } else {
    vars <- colnames(df)
  }
  if (!is.null(strata_var)) {
    vars <- vars[vars != strata_var]
  }
  if (is.null(strata_var)) {
    df_output <- data.frame(level = "N", value = nrow(df))
  } else {
    stratas <- sort(unique(df[[strata_var]]))
    n <- list(all = nrow(df))
    for (s in stratas) {
      n[[s]] <- sum(df[[strata_var]] == s, na.rm = TRUE)
    }
    strata_output <- cbind(data.frame(level = "N"), data.frame(n))
  }
  for (v in vars) {
    col <- df[[v]]
    if (is.character(col)) {
      col <- factor(col, levels = sort(unique(col)))
    } else if (is.logical(col)) {
      col <- factor(col, levels = c(TRUE, FALSE))
    }
    if (is.numeric(col)) {
      col_output <- summarize_num_var(col, digits)
    } else if (is.factor(col) |
      is.ordered(col) | sum(is.na(col)) == length(col)) {
      col_output <- summarize_factor_var(col, digits, sort)
    } else {
      stop(paste0(v, " data type is not supported"))
    }
    if (is.null(strata_var)) {
      df_output <- rbind(
        df_output, data.frame(level = v, value = ""), col_output
      )
    } else {
      temp <- list(all = c("", col_output$value))
      for (s in stratas) {
        sub_col <- col[df[[strata_var]] == s]
        if (is.numeric(sub_col)) {
          sub_col_output <- summarize_num_var(sub_col, digits)
        } else if (is.factor(sub_col) |
          is.ordered(sub_col) | sum(is.na(sub_col)) == length(sub_col)) {
          sub_col_output <- summarize_factor_var(sub_col, digits, FALSE)
        }
        sub_col_output <- sub_col_output[
          match(sub_col_output$level, col_output$level),
        ]
        temp[[s]] <- c("", sub_col_output$value)
      }
      temp <- data.frame(temp)
      strata_output <- rbind(
        strata_output,
        cbind(
          data.frame(level = c(v, col_output$level)),
          temp
        )
      )
    }
  }
  if (is.null(strata_var)) {
    df_output <- df_output[
      !(df_output$level == "na" & df_output$value == "0 (0)"),
    ]
    return(df_output)
  } else {
    strata_output <- strata_output[
      !(strata_output$level == "na" & strata_output$all == "0 (0)"),
    ]
    names(strata_output)[3:ncol(strata_output)] <- stratas
    return(strata_output)
  }
  # if (is.null(strata_var)) {
  #   output <- .summarize_wo_strata(df, vars, sort, digits)
  #   output <- output[!(output$level == "na" & output$value == "0 (0)"), ]
  # } else {
  #   strata_values <- sort(unique(df[[strata_var]]))
  #   strata_output <- lapply(strata_values, function(v) {
  #     df_subset <- df[df[[strata_var]] == v, ]
  #     subset_col <- .summarize_wo_strata(df_subset, vars, FALSE, digits)$value
  #     subset_col
  #   })
  #   names(strata_output) <- strata_values
  #   strata_output <- as.data.frame(do.call(cbind, strata_output))
  #   df_output <- .summarize_wo_strata(df, vars, FALSE, digits)
  #   output <- cbind(df_output, strata_output)
  #   names(output) <- c("level", "all", strata_values)
  #   output <- output[!(output$level == "na" & output$all == "0 (0)"), ]
  # }
  # output
}


# .summarize_wo_strata <- function(df, vars, sort, digits) {
#   level <- "N"
#   value <- nrow(df)
#   for (v in vars) {
#     col <- df[[v]]
#     if (is.character(col)) {
#       col <- factor(col, levels = sort(unique(col)))
#     } else if (is.logical(col)) {
#       col <- factor(col, levels = c(TRUE, FALSE))
#     }
#     if (is.numeric(col)) {
#       col_output <- summarize_num_var(col, digits)
#     } else if (is.factor(col) |
#       is.ordered(col) | sum(is.na(col)) == length(col)) {
#       col_output <- summarize_factor_var(col, digits, sort)
#     } else {
#       stop(paste0(v, " data type is not supported"))
#     }
#     level <- c(level, v, col_output$level)
#     value <- c(value, "", col_output$value)
#   }
#   output <- data.frame(level = level, value = value)
#   output
# }