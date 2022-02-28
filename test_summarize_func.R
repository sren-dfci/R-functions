source("/Users/siyangren/Dropbox (Partners HealthCare)/code repository/Functions/R-functions/summarize_variable.R")
# test summarize_factor_var by vector, data.frame column, all NA
# test summarize_num_var the same way
# test create_summ_table by all possible data types

# summarize_factor_var ---------------------------------
vec <- c(rep("a", 3), rep("b", 2), rep("d", 3), "a")
vec_na <- c(vec, NA, NA)
summarize_factor_var(vec, 0)
summarize_factor_var(vec, 2)
summarize_factor_var(vec, 2, sort = TRUE)
summarize_factor_var(NA, 2)

# summarize_num_var -----------------------------------
set.seed(11)
vec_num <- rnorm(9)
vec_num_na <- c(NA, NA, vec_num)
mean(vec_num) # -0.1440561
min(vec_num) # -1.516553
max(vec_num) # 1.323606
median(vec_num) # -0.04572296
summarize_num_var(vec_num, 2)
summarize_num_var(vec_num_na, 2)

# create_summ_table -----------------------------------
df <- data.frame(
  chr = vec_na,
  num = vec_num_na,
  na = NA,
  grp_num = c(rep(1, 6), rep(2, 5)),
  grp_chr = c(rep("cc", 4), rep("dd", 7))
)

create_summ_table(df, c("chr", "num"), "grp_num", digits = 3)
create_summ_table(df, c("chr", "num"), "grp_chr", digits = 2)
create_summ_table(df, c("chr", "num", "na"), sort = TRUE)
