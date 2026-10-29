# 28. Annuity Present Value
# What is the present value of an annuity of $6,500 per year, with the first
# cash flow received three years from today and the last one received 25 years
# from today? Use a discount rate of 7 percent.
library(scales)

# The number of payments
term <- 25 - 2

# The annuity factor formula
pvia <- function(r, t, m=1) (1 - (1 + r/m)^(-(t*m)))/(r/m)

# Present value of annuity (dated back two years).
label_dollar()((6500 * pvia(.07, term)) / (1 + .07)^2)
