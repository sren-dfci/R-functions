# tox_summ <- function(df,
#                      n_pts,
#                      case = "casenum",
#                      tox_desc = "TOXDESC",
#                      tox_cat = "TOXCATEGORY",
#                      tox_grade = "C_TOXGRADETOX") {
#   # max grade for each patient and each toxcode
#   f <- as.formula(paste0(tox_grade, "~", case, "+", tox_desc, "+", tox_cat))
#   max_grade_per_pt <- aggregate(f, df, max)
#   names(max_grade_per_pt)[names(max_grade_per_pt) == tox_grade] <- "max_grade"
#   # unique max toxgrades and toxcategory
#   # the parent level is toxcategory, then toxdesc, then (all_grade, grade=1, )
#   grades <- sort(unique(max_grade_per_pt$max_grade))
#   cats <- sort(unique(max_grade_per_pt[[tox_cat]]))
#   ls_r <- vector("list", length(cats))
#   names(ls_r) <- cats
#   # iterate through each toxcategory, toxdesc, get all_grade, grade=1
#   for (i in cats) {
#     # df for this toxcat only
#     tmp <- max_grade_per_pt[max_grade_per_pt[[tox_cat]] == i, ]
#     desc <- sort(unique(tmp[[tox_desc]]))
#     for (d in desc) {
#       # df for this toxcat and toxdesc only
#       ssub <- tmp[tmp[[tox_desc]] == d, ]
#       gg <- c("all_grade", grades)
#       for (g in gg) {
#         if (g == "all_grade") {
#           n <- nrow(ssub)
#         } else {
#           n <- sum(ssub[[tox_grade]] == g)
#         }
#         p <- round(n / n_pts * 100, 1)
#         np <- paste0(n, " (", p, ")")
#         ls_r[[i]][[d]][[g]] <- np
#       }
#     }
#   }
#   df_result <- do.call(
#     "rbind",
#     lapply(
#       ls_r, function(l) {
#         do.call("rbind", lapply(l, function(x) do.call("cbind", x)))
#       }
#     )
#   )
#   df_toxnames <- data.frame(
#     tox_cat = rep(names(ls_r), sapply(ls_r, length)),
#     tox_desc = as.vector(do.call("c", sapply(ls_r, names)))
#   )
#   df_result <- cbind(df_toxnames, df_result)
#   names(df_result)[1:2] <- c(tox_cat, tox_desc)
#   names(df_result)[4:ncol(df_result)] <- paste0(
#     "grade=", names(df_result)[4:ncol(df_result)]
#   )
#   df_result
# }


create_tox_table <- function(df,
                             n_pts,
                             case = "casenum",
                             tox_desc = "TOXDESC",
                             tox_cat = "TOXCATEGORY",
                             tox_grade = "C_TOXGRADETOX") {
  # max grade for each patient and each toxcode
  f1 <- as.formula(paste0(tox_grade, "~", case, "+", tox_desc, "+", tox_cat))
  max_grade_per_pt <- aggregate(f1, df, max)
  names(max_grade_per_pt)[names(max_grade_per_pt) == tox_grade] <- "max_grade"
  f2 <- as.formula(paste0(case, "~", tox_cat, "+", tox_desc, "+max_grade"))
  n_per_max_grade <- aggregate(f2, max_grade_per_pt, length)
  names(n_per_max_grade)[names(n_per_max_grade) == case] <- "n"
  o <- order(n_per_max_grade[[tox_cat]], n_per_max_grade[[tox_desc]])
  n_per_max_grade <- n_per_max_grade[o, ]
  w <- reshape(
    n_per_max_grade,
    v.names = "n", timevar = "max_grade", idvar = c(tox_cat, tox_desc),
    direction = "wide"
  )
  w[is.na(w)] <- 0
  row.names(w) <- NULL
  names(w) <- gsub("n.", "grade_", names(w))
  w <- w[, c(names(w)[1:2], sort(names(w)[3:ncol(w)]))]
  w$all_grade <- rowSums(w[, 3:ncol(w)])
  w[, 3:ncol(w)] <- lapply(w[, 3:ncol(w)], function(col) {
    v <- paste0(col, " (", round(col / n_pts * 100, 1), ")")
    v
  })
  w[, c(1:2, ncol(w), 3:(ncol(w) - 1))]
}