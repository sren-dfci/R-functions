tox_summ <- function(df,
                     n_all_pts,
                     id_var = "casenum",
                     tox_desc_var = "TOXDESC",
                     tox_cat_var = "TOXCATEGORY",
                     tox_attr_var = "C_TOXATTR",
                     tox_grade_var = "C_TOXGRADETOX",
                     tox_attr_options = 2:4,
                     tox_grade_options = 2:5) {
  # filter by tox attribution and grade
  df_select <- df[df[[tox_attr_var]] %in% tox_attr_options &
    df[[tox_grade_var]] %in% tox_grade_options, ]
  # max grade for each patient and each toxcode
  df_maxgrade_per_pt <- aggregate(
    as.formula(paste0(
      tox_grade_var,
      " ~ ", id_var, " + ", tox_desc_var, " + ", tox_cat_var
    )),
    df_select,
    max
  )
  # unique max toxgrades and toxcategory
  # the parent level is toxcategory, then toxdesc, then (all_grade, grade=1, )
  unique_max_grades <- sort(unique(df_maxgrade_per_pt[[tox_grade_var]]))
  unique_tox_cats <- sort(unique(df_maxgrade_per_pt[[tox_cat_var]]))
  ls_result <- vector("list", length(unique_tox_cats))
  names(ls_result) <- unique_tox_cats
  # iterate through each toxcategory, toxdesc, get all_grade, grade=1
  for (l in names(ls_result)) {
    # df for this toxcat only
    .sub <- df_maxgrade_per_pt[df_maxgrade_per_pt[[tox_cat_var]] == l, ]
    unique_tox_desc <- sort(unique(.sub[[tox_desc_var]]))
    for (d in unique_tox_desc) {
      # df for this toxcat and toxdesc only
      .ssub <- .sub[.sub[[tox_desc_var]] == d, ]
      .grades <- c("all_grade", unique_max_grades)
      for (g in .grades) {
        if (g == "all_grade") {
          n <- nrow(.ssub)
        } else {
          n <- sum(.ssub[[tox_grade_var]] == g)
        }
        p <- round(n / n_all_pts * 100, 1)
        np <- paste0(n, " (", p, ")")
        ls_result[[l]][[d]][[g]] <- np
      }
    }
  }
  df_result <- do.call(
    "rbind",
    lapply(
      ls_result, function(l) {
        do.call("rbind", lapply(l, function(x) do.call("cbind", x)))
      }
    )
  )
  df_toxnames <- data.frame(
    tox_cat = rep(names(ls_result), sapply(ls_result, length)),
    tox_desc = as.vector(do.call("c", sapply(ls_result, names)))
  )
  df_result <- cbind(df_toxnames, df_result)
  names(df_result)[1:2] <- c(tox_cat_var, tox_desc_var)
  names(df_result)[4:ncol(df_result)] <- paste0(
    "grade=", names(df_result)[4:ncol(df_result)]
  )
  df_result
}