# ------------------  Tox attribution check  -----------------------
# Description:
# This function checks toxicity attribution issues within each cohort.
# For each cohort, please specify the drugs they were treated with and
# drugs that they shouldn't be treated with.

# Arguments:
# d_tox_start: the description of the timepoint when toxicity started
# dt_tox_start: the timepoint of toxicity started
# d_tox_end: the description of the timepoint when toxicity ended
# dt_tox_end: the timepoint of toxicity ended
# ongoing: TRUE/FALSE, if TRUE, won't check missing in dt_tx_end
# drugs_ex: column names of the drugs that shouldn't be given to this cohort
# drugs: column names of the drugs that were given to this cohort
# dt_tx_start: the timepoint of treatment started. If there are multiple drugs,
# could be either a single column name or a vector of column names with the
# same length of drugs
# dt_tx_end: same as dt_tx_start
# threshold: allow treatment-related toxicities to happen within threshold days
# after treatment ended


# Return:
# The subset of df which contains issues
# The column "comments" contains issues such as tox start/end date missing
# Then each drug has a separate column named as "<drug name>_comments",
# containing attribution-related issues

# package "haven" could import sas7bdat data, with "DT_" columns as date format
# please check these columns before running the functions



tox_check <- function(df, subset = NULL,
                      d_tox_start = "D_DTTOXSTART",
                      dt_tox_start = "DT_DTTOXSTART",
                      d_tox_end = "D_DTTOXEND",
                      dt_tox_end = "DT_DTTOXEND",
                      ongoing = FALSE,
                      drugs_ex = NULL, drugs,
                      dt_tx_start,
                      dt_tx_end,
                      threshold) {
  require(tidyverse)
  # if use subset
  if (!is.null(subset)) {
    if (length(subset) != nrow(df)) {
      stop("length of subset needs to be the same as the number of rows")
    }
    df <- df[subset, ]
    cat(nrow(df), "rows included\n\n")
  }
  # run the checking function
  # use Package haven, date should be in the date format
  # drugs, drugs_ex, dt_tx_start, dt_tx_end and threshold can be vectors
  # drugs_ex can be NULL
  vector_args <- list(drugs, dt_tx_start, dt_tx_end, threshold)
  names(vector_args) <- c(
    "drugs", "dt_tx_start", "dt_tx_end", "threshold"
  )
  # circumstance 1: single drug (the other three args can only be single value)
  if (length(drugs) == 1) {
    for (i in names(vector_args)[-1]) {
      if (length(vector_args[[i]]) != 1) {
        stop(paste(i, "needs to be a single value"))
      }
    }
  }
  # circumstance 2: multiple drugs, the other three args can be either a single
  # value or a vector the same length as drugs
  if (length(drugs) > 1) {
    for (i in names(vector_args)[-1]) {
      if (length(vector_args[[i]]) == 1) {
        warning(paste("a single", i, "used for all drugs"))
        vector_args[[i]] <- rep(vector_args[[i]], length(drugs))
      } else if (length(vector_args[[i]]) != length(drugs)) {
        stop(paste(i, "needs to be the same length as drugs"))
      }
    }
  }
  # --- Start checking --- #
  # check tox start/end date issues
  df$comments <- apply(df, 1, function(row) {
    tmp <- c()
    # missing toxicity start date in non-baseline toxicities
    if (!str_detect(row[d_tox_start], regex("baseline", ignore_case = TRUE)) &
      is.na(row[dt_tox_start])) {
      tmp <- c(tmp, "Tox start date missing/error")
    }
    # missing toxicity end date in non-continuing toxicities
    if (!str_detect(row[d_tox_end], regex("continu", ignore_case = TRUE)) &
      is.na(row[dt_tox_end])) {
      tmp <- c(tmp, "Tox end date missing/error")
    }
    # tox start date > tox end date
    if ((row[dt_tox_start] > row[dt_tox_end]) %in% TRUE) {
      tmp <- c(tmp, "Tox end date earlier than start date")
    }
    # drugs shouldn't be received by this cohort
    if (!is.null(drugs_ex)) {
      # should be NA, or not applicable or no relation
      drugs_ex_issue <- drugs_ex[
        !row[drugs_ex] %in% c(
          NA, "No Relation", "Not Applicable", "Unrelated", ""
        )
      ]
      if (length(drugs_ex_issue)) {
        tmp <- c(tmp, paste0(drugs_ex_issue, " should not have attribution"))
      }
    }
    # combine all comments into a single string
    paste(tmp, collapse = "; ")
  })
  # check tox attribution, the vector_args is a list
  # with each arg the same length as the number of drugs
  # iterate through each drug, corresponding start/end date
  # and threshold
  drug_comments <- vector("list", length(vector_args$drugs))
  # save comments for each drug as elements of this list
  for (i in seq_along(vector_args$drugs)) {
    drug <- vector_args$drugs[i]
    dt_drug_start <- vector_args$dt_tx_start[i]
    dt_drug_end <- vector_args$dt_tx_end[i]
    drug_threshold <- vector_args$threshold[i]
    drug_comments[[i]] <- apply(df, 1, function(row) {
      tmp <- c()
      # drug start date missing
      if (is.na(row[dt_drug_start])) {
        tmp <- c(tmp, "Treatment start date missing")
      }
      # drug end date missing
      if (!ongoing) {
        if (is.na(row[dt_drug_end])) {
          tmp <- c(tmp, "Treatment end date missing")
        }
      }
      # missing tox attribution
      if (row[drug] == "" | is.na(row[drug])) {
        tmp <- c(tmp, "Tox attribution missing")
      }
      # tox started before trt started, shouldn't have attribution
      if ((row[dt_tox_start] < row[dt_drug_start] &
        !row[drug] %in% c("No Relation", "Not Applicable")) %in% TRUE) {
        tmp <- c(tmp, "Tox started before trt started w/ attribution")
      }
      # tox ended before trt started, shouldn't have attribution
      if ((row[dt_tox_end] < row[dt_drug_start] &
        !row[drug] %in% c("No Relation", "Not Applicable")) %in% TRUE) {
        tmp <- c(tmp, "Tox ended before trt started w/ attribution")
      }
      # tox start date > off txt date + threshold, with attribution
      tox_offtx_diff <- difftime(
        row[dt_tox_start], row[dt_drug_end],
        units = "days"
      )
      if ((tox_offtx_diff > drug_threshold &
        !row[drug] %in% c("No Relation", "Not Applicable")) %in% TRUE) {
        tmp <- c(tmp, "Tox started after off treatment w/ attribution")
      }
      # combine all comments into a single string
      paste(tmp, collapse = "; ")
    })
  }
  # append the list to the dataframe
  df[paste0(vector_args$drugs, "_comments")] <- drug_comments
  df_check <- df %>%
    filter(rowSums(across(ends_with("comments"), ~ .x != "")) > 0)
  df_clean <- df %>%
    filter(rowSums(across(ends_with("comments"), ~ .x != "")) == 0)
  cat("output", nrow(df_check), "out of", nrow(df), "records in total \n\n")
  list(check = df_check, clean = df_clean)
}



# # --- check args --- #
#   # make sure all elements in the args can be found
#   args_elements <- c(
#     d_tox_start, dt_tox_start, d_tox_end, dt_tox_end,
#     drugs, dt_tx_start, dt_tx_end
#   )
#   if (!is.null(drugs_ex)) c(args_elements, drugs_ex)
#   args_missing <- args_elements[!args_elements %in% names(df)]
#   if (length(args_missing)) {
#     stop(paste(paste(args_missing, collapse = "; "), "not found"))
#   }
#   # d_tox_start, dt_tox_start, d_tox_end, dt_tox_end only allow single value
#   single_args <- list(d_tox_start, dt_tox_start, d_tox_end, dt_tox_end)
#   names(single_args) <- c(
#     "d_tox_start", "dt_tox_start", "d_tox_end", "dt_tox_end"
#   )
#   for (i in names(single_args)) {
#     if (length(single_args[[i]]) > 1) {
#       stop(paste0(i, " needs to be a single value"))
#     }
#   }
#   # ongoing needs to be logical
#   if (!is.logical(ongoing)) {
#     stop("ongoing needs to be logical")
#   }
#   # drugs, drugs_ex, dt_tx_start, dt_tx_end and threshold can be vectors
#   # drugs_ex can be NULL
#   vector_args <- list(drugs, dt_tx_start, dt_tx_end, threshold)
#   names(vector_args) <- c(
#     "drugs", "dt_tx_start", "dt_tx_end", "threshold"
#   )
#   for (i in vector_args) {
#     if (missing(i)) {
#       stop(paste(i, "needs to be specified"))
#     }
#   }
#   # circumstance 1: single drug (the other three args can only be single value)
#   if (length(drugs) == 1) {
#     for (i in names(vector_args)[-1]) {
#       if (length(vector_args[[i]]) != 1) {
#         stop(paste(i, "needs to be a single value"))
#       }
#     }
#   }
#   # circumstance 2: multiple drugs, the other three args can be either a single
#   # value or a vector the same length as drugs
#   if (length(drugs) > 1) {
#     for (i in names(vector_args)[-1]) {
#       if (length(vector_args[[i]]) == 1) {
#         warning(paste("a single", i, "used for all drugs"))
#         vector_args[[i]] <- rep(vector_args[[i]], length(drugs))
#       } else if (length(vector_args[[i]]) != length(drugs)) {
#         stop(paste(i, "needs to be the same length as drugs"))
#       }
#     }
#   }