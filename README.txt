For numeric variables, it will show quantiles (0, 25, 50, 75, 100) and mean.
For categorical variables, if it's a binomial variable with 0, 1 as label, you need to convert it to character first.
If there is any NA in the variable, it will automatically show in the output, otherwise, it will not show up.

Options:
* .vars: specify which variables you want to include in the table, if NULL, all variables will be included
* .strata_var: specify the group variable, maximum of one supported
* .mean_vars: by default, the function will calculate median (range), if you want to calculate mean (sd) instead, add the variable here
* .digits: default as 0
