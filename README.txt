For numeric variables, there are two options: median (range) and mean (sd).

For categorical variables, if it's a binomial variable with 0, 1 as label, you need to convert it to character first.

Options:
* .vars: specify which variables you want to include in the table, if NULL, all variables will be included
* .strata_var: specify the group variable
* .mean_vars: by default, the function will calculate median (range), if you want to calculate mean (sd) instead, add the variable here
* .digits: default as 0