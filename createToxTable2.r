create_tox_table <- function(df, N) {
  # max grade by case
  max_grade <- aggregate(
    C_TOXGRADETOX ~ casenum + TOXDESC + TOXCATEGORY,
    data = df,
    FUN = max
  )
  names(max_grade)[4] <- "max_grade"
  # each tox, grade, count number of cases
  ncase <- aggregate(
    casenum ~ TOXDESC + TOXCATEGORY + max_grade,
    data = max_grade,
    FUN = function(x) length(unique(x))
  )
  names(ncase)[4] <- "n"
  # change tox grade name
  ncase$max_grade <- paste("Grade", ncase$max_grade)
  # if max grade = 1, then change to grade < 2
  ncase$max_grade[ncase$max_grade == "Grade 1"] <- "Grade < 2"
  # sort max grade
  ncase$max_grade <- factor(
    ncase$max_grade,
    levels = c("Grade < 2", "Grade 2", "Grade 3", "Grade 4", "Grade 5")
  )
  ncase_order <- ncase[
    order(ncase$TOXCATEGORY, ncase$TOXDESC),
  ]
  # long to wide
  ncase_wide <- reshape(
    ncase_order,
    idvar = c("TOXDESC", "TOXCATEGORY"),
    v.names = "n",
    timevar = "max_grade",
    direction = "wide"
  )
  # remove the "n." ahead of each column name and sort
  grade_colnames <- names(ncase_wide)[3:ncol(ncase_wide)]
  grade_colnames <- substr(grade_colnames, 3, 100)
  names(ncase_wide)[3:ncol(ncase_wide)] <- grade_colnames
  # sub column names without grade < 2
  grade2_colnames <- grade_colnames[grade_colnames != "Grade < 2"]
  # sort column names
  ncase_wide_sort <- ncase_wide[, c("TOXCATEGORY", "TOXDESC", sort(grade_colnames))]
  # add ALL GRADE column
  ncase_wide_sort$`All Grade` <- rowSums(
    ncase_wide_sort[, grade_colnames, drop = FALSE],
    na.rm = TRUE
  )
  # add ALL GRADE but grade < 2
  ncase_wide_sort$`All Grade >= 2` <- rowSums(
    ncase_wide_sort[, grade2_colnames, drop = FALSE],
    na.rm = TRUE
  )
  # add row-wise percentage
  for (i in 3:ncol(ncase_wide_sort)) {
    col_name <- names(ncase_wide_sort)[i]
    n_tox <- ncase_wide_sort[[col_name]]
    perc_tox <- round(n_tox / N * 100, 1)
    ncase_wide_sort[[col_name]] <- ifelse(
      !is.na(n_tox),
      paste0(n_tox, " (", perc_tox, ")"),
      ""
    )
  }
  return(ncase_wide_sort)
  # df_s %>%
  #   filter(C_TOXGRADETOX > 1) %>%
  #   group_by(casenum, TOXDESC, TOXCATEGORY) %>%
  #   summarise(max_grade = max(C_TOXGRADETOX)) %>%
  #   ungroup() %>%
  #   group_by(TOXDESC, TOXCATEGORY, max_grade) %>%
  #   summarise(n = n_distinct(casenum))
}