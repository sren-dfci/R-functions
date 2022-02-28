tg_de_summ <- function(df, case, cycle, lesion, lesion_len) {
  df <- df[order(df[[case]], df[[cycle]], df[[lesion]]), ]
  unique_cases <- sort(unique(df[[case]]))
  # placeholder for outputs
  case_num <- c()
  cycle_num <- c()
  n_lesion <- c()
  lesion_seq <- c()
  sum_lens <- c()
  n_lesion_diff <- c()
  new_lesion <- c()
  base_len <- c()
  min_len <- c()
  # temp vars
  tmp_n_lesion <- NULL
  tmp_min_len <- NULL
  known_lesions <- NULL
  # iter over casenum
  for (i in unique_cases) {
    # cycles for this case
    cycles_i <- sort(unique(df[[cycle]][df[[case]] == i]))
    n_cycle_i <- length(cycles_i)
    case_num <- c(case_num, rep(i, n_cycle_i))
    cycle_num <- c(cycle_num, cycles_i)
    for (j in cycles_i) {
      # lesions for this cycle
      lesions_ij <- df[[lesion]][df[[case]] == i & df[[cycle]] == j]
      lens_ij <- df[[lesion_len]][df[[case]] == i & df[[cycle]] == j]
      lesion_seq_ij <- paste(lesions_ij, collapse = "-")
      n_lesion_ij <- length(lesions_ij)
      sum_lens_ij <- sum(lens_ij)
      lesion_seq <- c(lesion_seq, lesion_seq_ij)
      n_lesion <- c(n_lesion, n_lesion_ij)
      sum_lens <- c(sum_lens, sum_lens_ij)
      # initial baseline values
      if (j == 0) {
        # record baseline lesion sequence, n lesions and sum of lens
        tmp_n_lesion <- n_lesion_ij
        tmp_min_len <- sum_lens_ij
        known_lesions <- lesions_ij
        n_lesion_diff <- c(n_lesion_diff, 0)
        new_lesion <- c(new_lesion, 0)
        base_len <- c(base_len, rep(sum_lens_ij, n_cycle_i))
        min_len <- c(min_len, sum_lens_ij)
      } else {
        # determine n lesion change
        n_lesion_diff <- c(n_lesion_diff, n_lesion_ij - tmp_n_lesion)
        if (n_lesion_ij != tmp_n_lesion) tmp_n_lesion <- n_lesion_ij
        # determine new lesion
        if (sum(!lesions_ij %in% known_lesions)) {
          new_lesion <- c(new_lesion, 1)
          # add new lesion number to it
          known_lesions <- sort(unique(c(known_lesions, lesions_ij)))
        } else {
          new_lesion <- c(new_lesion, 0)
        }
        # min sum len
        if (!is.na(sum_lens_ij) & sum_lens_ij < tmp_min_len) {
          tmp_min_len <- sum_lens_ij
        }
        min_len <- c(min_len, tmp_min_len)
      }
    }
  }
  output <- data.frame(
    tmp1 = case_num,
    tmp2 = cycle_num,
    n_lesion = n_lesion,
    n_lesion_diff = n_lesion_diff,
    lesion_seq = lesion_seq,
    new_lesion = new_lesion,
    sum_lens = sum_lens,
    base_len = base_len,
    min_len = min_len
  )
  names(output)[1:2] <- c(case, cycle)
  # calculate absolute and relative change in tumor length
  output$abs_diff_base <- round(output$sum_lens - output$base_len, 3)
  output$abs_diff_min <- round(output$sum_lens - output$min_len, 3)
  output$perc_diff_base <- round(
    output$abs_diff_base / output$base_len * 100, 1
  )
  output$perc_diff_min <- ifelse(
    output$min_len == 0,
    100,
    round(output$abs_diff_min / output$min_len * 100, 1)
  )
  output$resp_no_abs <- ifelse(
    output$sum_lens == 0,
    "CR",
    ifelse(
      output$perc_diff_base < -30,
      "PR",
      ifelse(
        output$n_lesion_diff > 0 |
          output$new_lesion == 1 |
          output$perc_diff_min > 20,
        "PD",
        "SD"
      )
    )
  )
  output$resp_abs_cm <- ifelse(
    output$n_lesion_diff <= 0 &
      output$new_lesion == 0 &
      output$perc_diff_min > 20 &
      output$abs_diff_min < 0.5,
    "SD",
    output$resp_no_abs
  )
  output$resp_abs_mm <- ifelse(
    output$n_lesion_diff <= 0 &
      output$new_lesion == 0 &
      output$perc_diff_min > 20 &
      output$abs_diff_min < 5,
    "SD",
    output$resp_no_abs
  )
  output
}

ntg_de_summ <- function(df, case, cycle, lesion, lesion_status) {
  df <- df[order(df[[case]], df[[cycle]], df[[lesion]]), ]
  unique_cases <- sort(unique(df[[case]]))
  # placeholder for outputs
  case_num <- c()
  cycle_num <- c()
  n_lesion <- c()
  lesion_seq <- c()
  n_lesion_diff <- c()
  new_lesion <- c()
  ntg_resp <- c()
  # tmp vars
  tmp_n_lesion <- NULL
  known_lesions <- NULL
  # iter over casenum
  for (i in unique_cases) {
    # cycles for case i
    cycles_i <- sort(unique(df[[cycle]][df[[case]] == i]))
    n_cycle_i <- length(cycles_i)
    case_num <- c(case_num, rep(i, n_cycle_i))
    cycle_num <- c(cycle_num, cycles_i)
    # iter over cycles
    for (j in cycles_i) {
      # lesions for case i cycle j
      lesions_ij <- df[[lesion]][df[[case]] == i & df[[cycle]] == j]
      lesion_stat_ij <- df[[lesion_status]][df[[case]] == i & df[[cycle]] == j]
      lesion_seq_ij <- paste(lesions_ij, collapse = "-")
      n_lesion_ij <- length(lesions_ij)
      lesion_seq <- c(lesion_seq, lesion_seq_ij)
      n_lesion <- c(n_lesion, n_lesion_ij)
      # initial baseline values
      if (j == 0) {
        tmp_n_lesion <- n_lesion_ij
        known_lesions <- lesions_ij
        n_lesion_diff <- c(n_lesion_diff, 0)
        new_lesion <- c(new_lesion, 0)
        ntg_resp <- c(ntg_resp, "SD")
      } else {
        n_lesion_diff <- c(n_lesion_diff, n_lesion_ij - tmp_n_lesion)
        if (n_lesion_ij != tmp_n_lesion) tmp_n_lesion <- n_lesion_ij
        # determine new lesion
        if (sum(!lesions_ij %in% known_lesions)) {
          new_lesion <- c(new_lesion, 1)
          # add new lesion number to it
          known_lesions <- sort(unique(c(known_lesions, lesions_ij)))
        } else {
          new_lesion <- c(new_lesion, 0)
        }
        # determine lesion status
        ntg_resp <- c(ntg_resp, ifelse(
          any(lesion_stat_ij == "Unknown"),
          "Unknown",
          ifelse(
            any(
              lesion_stat_ij %in% c(
                "Baseline Lesion has Unequivocal Progression",
                "New Lesion (not present at BL)"
              )
            ),
            "PD",
            ifelse(
              all(lesion_stat_ij == "Baseline Lesion Disappeared"),
              "CR",
              ifelse(
                all(lesion_stat_ij %in% c(
                  "Baseline Lesion Present", "Baseline Lesion Disappeared"
                )),
                "SD",
                "Unknown"
              )
            )
          )
        ))
      }
    }
  }
  output <- data.frame(
    tmp1 = case_num,
    tmp2 = cycle_num,
    n_lesion = n_lesion,
    n_lesion_diff = n_lesion_diff,
    lesion_seq = lesion_seq,
    new_lesion = new_lesion,
    ntg_resp = ntg_resp
  )
  names(output)[1:2] <- c(case, cycle)
  output$ntg_resp <- ifelse(
    output$n_lesion == 0,
    "CR",
    ifelse(
      output$n_lesion_diff > 0 | output$new_lesion == 1,
      "PD",
      output$ntg_resp
    )
  )
  output
}

# update 11/4, if a row has unacceptable values, don't stop but mark it
recist_rules <- function(tg_resp,
                         tg_new_lesion = FALSE,
                         tg_all_eval = TRUE,
                         ntg_resp,
                         ntg_new_lesion = FALSE,
                         ntg_all_eval = TRUE) {
  # if (any(!tg_resp %in% c("CR", "PR", "SD", "PD", "no lesion", NA))) {
  #   stop("tg_resp has values not accepted")
  # }
  # if (any(!ntg_resp %in% c(
  #   "CR", "Non-CR/non-PD", "SD", "PD",
  #   "no lesion", NA
  # ))) {
  #   stop("ntg_resp has values not accepted")
  # }
  # if (any(!tg_new_lesion %in% c(TRUE, FALSE, NA))) {
  #   stop("tg_new_lesion has values not accepted")
  # }
  # if (any(!ntg_new_lesion %in% c(TRUE, FALSE, NA))) {
  #   stop("ntg_new_lesion has values not accpeted")
  # }
  # if (any(!tg_all_eval %in% c(TRUE, FALSE, NA))) {
  #   stop("tg_all_eval has values not accpeted")
  # }
  # if (any(!ntg_all_eval %in% c(TRUE, FALSE, NA))) {
  #   stop("ntg_all_eval has values not accpeted")
  # }
  if (length(tg_new_lesion) == 1) {
    tg_new_lesion <- rep(tg_new_lesion, length(tg_resp))
  }
  if (length(tg_all_eval) == 1) {
    tg_all_eval <- rep(tg_all_eval, length(tg_resp))
  }
  if (length(ntg_new_lesion) == 1) {
    ntg_new_lesion <- rep(ntg_new_lesion, length(tg_resp))
  }
  if (length(ntg_all_eval) == 1) {
    ntg_all_eval <- rep(ntg_all_eval, length(tg_resp))
  }
  # missing in ntg_resp (only matters in the circumstance 2)
  ntg_resp <- ifelse(
    is.na(ntg_resp) &
      ntg_all_eval %in% TRUE &
      ntg_new_lesion %in% FALSE,
    "no lesion",
    ntg_resp
  )
  # missing in tg_resp
  # if tg_new_lesion is TRUE, don't need impute
  # if tg_all_eval is FALSE, go to cirumstance 1, else assume no target lesion
  tg_resp <- ifelse(
    is.na(tg_resp),
    ifelse(
      tg_new_lesion %in% FALSE & tg_all_eval %in% TRUE,
      "no lesion", # go to circumstance 2
      ifelse(
        tg_all_eval %in% FALSE,
        "", # go to circumstance 1
        tg_resp
      )
    ),
    tg_resp
  )
  # conservatively dealing with missing new_lesion/all_eval
  tg_new_lesion[is.na(tg_new_lesion)] <- FALSE
  tg_all_eval[is.na(tg_all_eval)] <- TRUE
  ntg_new_lesion[is.na(ntg_new_lesion)] <- FALSE
  ntg_all_eval[is.na(ntg_all_eval)] <- TRUE
  # patterns should not exist
  # tg_resp is missing, but tg_all_eval is FALSE and tg_new_lesion is TRUE
  # circumstance 1: both target and non-target
  # circumstance 2: only non-target
  ovresp <- ifelse(
    # if any column has unaccepted values
    !tg_resp %in% c("CR", "PR", "SD", "PD", "no lesion", NA) |
      !ntg_resp %in% c("CR", "Non-CR/non-PD", "SD", "PD", "no lesion", NA) |
      !tg_new_lesion %in% c(TRUE, FALSE, NA) |
      !ntg_new_lesion %in% c(TRUE, FALSE, NA) |
      !tg_all_eval %in% c(TRUE, FALSE, NA) |
      !ntg_all_eval %in% c(TRUE, FALSE, NA),
    "query",
    ifelse(
      # this mainly deal with the circumstance where tg_resp is missing
      tg_new_lesion %in% TRUE | ntg_new_lesion %in% TRUE,
      "PD",
      ifelse(
        # circumstance 1: tg_resp exists
        !(is.na(tg_resp) | tg_resp %in% "no lesion"),
        ifelse(
          # any lesion has PD
          tg_resp %in% "PD" | ntg_resp %in% "PD",
          "PD",
          ifelse(
            # if target not all evaluated
            tg_all_eval %in% FALSE,
            "NE",
            ifelse(
              # if non-target is not PD, then overall = target
              tg_resp %in% c("PR", "SD"),
              tg_resp,
              ifelse(
                # if both are CR, then CR
                tg_resp %in% "CR" & ntg_resp %in% c("CR", "no lesion"),
                "CR",
                "PR"
              )
            )
          )
        ),
        # circumstance 2: tg_resp doesn't exist
        ifelse(
          # if any non-target PD
          ntg_resp %in% "PD",
          "PD",
          ifelse(
            # if not all evaluated
            ntg_all_eval %in% FALSE,
            "NE",
            ifelse(
              ntg_resp %in% "no lesion",
              "",
              ntg_resp
            )
          )
        )
      )
    )
  )
  ovresp
}


resp_str_clean <- function(x) {
  # x cleaned
  xc <- x
  xc[grepl("Complete", xc, ignore.case = TRUE)] <- "CR"
  xc[grepl("Partial", xc, ignore.case = TRUE)] <- "PR"
  xc[grepl("Non-CR/non-PD", xc, ignore.case = TRUE)] <- "SD"
  xc[grepl("Stable Disease", xc, ignore.case = TRUE)] <- "SD"
  xc[grepl("Progression", xc, ignore.case = TRUE)] <- "PD"
  xc[grepl("Relapse", xc, ignore.case = TRUE)] <- "PD"
  xc[grepl("No ", xc, ignore.case = TRUE)] <- "no lesion"
  xc[grepl("New", xc, ignore.case = TRUE)] <- "PD"
  print(table(x, xc, useNA = "ifany"))
  xc
}




# recist_test_df <- expand.grid(
#   tg_resp = c("CR", "PR", "PD", "SD", NA),
#   tg_new_lesion = c(TRUE, FALSE, NA),
#   tg_all_eval = c(TRUE, FALSE, NA),
#   ntg_resp = c("CR", "PD", "Non-CR/non-PD", "NE", NA),
#   ntg_new_lesion = c(TRUE, FALSE, NA),
#   ntg_all_eval = c(TRUE, FALSE, NA)
# )

# recist_test_df$ovresp <- with(
#   recist_test_df,
#   recist_rules(
#     tg_resp, tg_new_lesion, tg_all_eval,
#     ntg_resp, ntg_new_lesion, ntg_all_eval
#   )
# )