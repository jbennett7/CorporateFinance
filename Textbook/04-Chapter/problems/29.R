# 29. Annuity Present Values
# What is the value today of a 15-year annuity that pays $650 a year? The
# annuity's first payment occurs six years from today. The annual interest rate
# is 11 percent for Years 1 through 5, and 13 percent thereafter.
library(scales)

# Annuity factor
pvia <- function(r, t) (1 - (1 + r)^(-t)) / r

# value at t=5 (first payment t=6)
label_dollar()(payments <- 650 * pvia(.13, 15))

# discount to t=0 at 11%
label_dollar()(discounted <- payments / (1 + .11)^5)

