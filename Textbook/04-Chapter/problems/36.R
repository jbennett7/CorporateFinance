# 36. Calculating the Number of Payments
# You're prepared to make monthly payments of $350, beginning at the end of
# this month, into an account that pays 10 percent interest compounded monthly.
# How many payments will you have made when your account balance reaches
# $35,000?
library(scales)

# future annuity factor
fvia <- function(interest_rate, term)
    ((1 + interest_rate)^term - 1) / interest_rate

fv_shortfall <- function(interest_rate, cash_flow, future_value, term)
    cash_flow * fvia(interest_rate, term) - future_value

# monthly rate for 10% APR
rate <- .1 / 12
cash_flow <- 350
future_value <- 35000

# Term is derived from the future value of an annuity formula
# V = C x (((1 + r)^T - 1) / r) => T = ln((Vr/C) + 1) / ln(1 + r)
ceiling(term <- log((future_value * rate) / cash_flow + 1) / log(1 + rate))

ceiling(term_uniroot <- uniroot(fv_shortfall,
    interest_rate = rate, cash_flow = cash_flow, future_value = future_value,
    interval = c(0, 100), tol = 1e-8)$root)

# Check to ensure we found the correct term
cash_flow * fvia(rate, ceiling(term)) >= future_value
cash_flow * fvia(rate, ceiling(term) - 1) < future_value
