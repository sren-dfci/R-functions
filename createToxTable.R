createToxTable <- function(.df, .strata_var = NULL) {
  require(data.table)
  setDT(.df)
  # filter by attribution
  .df <- .df[.df$D_TOXATTR %in% c("Definite", "Probable", "Possible"), ]
  # filter by tox grade
  .df <- .df[.df$C_TOXGRADETOX >= 2, ]
  # empty list to save results
  .outcomes <- vector("list", uniqueN(.df$TOXDESC)) + 1
  names(.outcomes) <- c("N", uniqueN(.df$TOXDESC))
  # N
  if (is.null(.strata_var)) {
    .outcomes[["N"]] <- uniqueN(.df$casenum)
  } else {
    for (s in sort(unique(.df[[.strata_var]]))) {
      .outcomes[["N"]][[s]] <- uniqueN(.df$casenum[.df[[.strata_var]] == s])
    }
  }
  # TOXDESC
  if (is.null(.strata_var)) {
    # for each tox
    for (d in unique(.df$TOXDESC)) {
      # filter this tox only
      .sub <- .df[TOXDESC == d][,
        .(max_grade = max(C_TOXGRADETOX)),
        casenum
      ]
      # unique N with this tox
      .outcomes[[d]][["All level"]] <- uniqueN(.sub$casenum)
      for (m in sort(unique(.sub$max_grade))) {
        .n <- uniqueN(.sub$casenum[.sub$max_grade == m])
        .p <- round(.n / uniqueN(.sub$casenum) * 100, 2)
         .outcomes[[d]][[paste0("Grade ", m, " (%)")]] <- paste0(.n, " (", .p, ")")
      }
    }
  } else {
    for (s in sort(unique(.df[[.strata_var]]))) {
      for (d in unique(.df$TOXDESC)) {
        .sub <- .df[.df[[.strata_var]] == s, ][TOXDESC == d][,
          .(max_grade = max(C_TOXGRADETOX)),
          casenum
        ]
        .outcomes[[s]][[d]][["All level"]] <- uniqueN(.sub$casenum)
        for (m in sort(unique()))
      }
    }
  }
}



# assume only grade > 2 and related tox are kepy
# assume each id, and tox only keep the max tox grade
createToxTbl <- function(.df, .id, .toxdesc, .toxgrade, .grp = NULL) {
  n_tox <- length(unique(.df[[.toxdesc]]))
  tox <- sort(unique(.df[[.toxdesc]]))
  n_grade <- length(unique(.df[[.toxgrade]]))
  grade <- sort(unique(.df[[.toxgrade]]))
  .outcome <- vector("list", n_tox + 1)
  names(.outcome) <- c("N", tox)
  # N
  .outcome[["N"]] <- length(unique(.df[[.id]]))
  # tox
  for (tt in tox) {
    .outcome[[tt]][["All level"]] <- {
      .n <- length(unique(
        .df[[.id]][.df[[.toxdesc]] == tt]))
      .perc <- round(.n / .outcome[["N"]] * 100, 2)
      paste0(.n, " (", .perc, ")")
    }
    for (g in grade) {
      .outcome[[tt]][[paste0("grade ", g)]] <- {
        .n <- length(unique(
          .df[[.id]][.df[[.toxdesc]] == tt & .df[[.toxgrade]] == g]
        ))
        .perc <- round(.n / .outcome[["N"]] * 100, 2)
        paste0(.n, " (", .perc, ")")
      }
    }
  }
  .outcome[["toxicity"]] <- data.frame(do.call(
    rbind, lapply(.outcome[tox], do.call, what = "c")
  ))
  return(.outcome[c("N", "toxicity")])
}

do.call(
  rbind,
  lapply(a[c("Nausea", "Platelets")], function(x) do.call("c", x))
)
createToxTbl <- function(.df, .id, .toxdesc, .toxgrade, .grp = NULL) {
  n_tox <- length(unique(.df[[.toxdesc]]))
  tox <- sort(unique(.df[[.toxdesc]]))
  n_grade <- length(unique(.df[[.toxgrade]]))
  grade <- sort(unique(.df[[.toxgrade]]))
  if (!is.null(.grp)) {
    n_grp = length(unique(.df[[.grp]]))
    grp <- sort(unique(.df[[.grp]]))
  } else {
    n_grp <- 0
  }
  .outcome <- vector("list", n_tox + 1)
  names(.outcome) <- c("N", tox)
  if (n_grp == 0) {
    .outcome[["N"]] <- length(unique(.df[[.id]]))
    for (tt in tox) {
      
    }
  }
  
  # N
  if (n_grp == 0) {
    .outcome[["N"]] <- 
  } else {
    for (g in grp) {
      .outcome[["N"]][[g]] <- length(unique(.df[[.id]][.df[[.grp]] == g]))
    }
  }
  singleArm <- function(.df)
    # iterate over tox desc
    for (tt in tox) {
      # if no arm
      if (n_grp == 0) {
        # all level
        .outcome[[tt]][["All level"]] <- {
          .n <- length(unique(
            .df[[.id]][.df[[.toxdesc]] == tt]))
          .perc <- round(.n / .outcome[["N"]] * 100, 2)
          paste0(.n, " (", .perc, ")")
        }
        for (g in grade) {
          .outcome[[tt]][[paste0("grade ", g)]] <- {
            .n <- length(unique(
              .df[[.id]][.df[[.toxdesc]] == tt & .df[[.toxgrade]] == g]
            ))
          }
        }
        
        
        
      } else {
        
      }
    }
}