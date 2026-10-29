# 46. Calculating Present Value of a Perpetuity
# Given an interest rate of 6.1 percent per year, what is the value at
# Date t = 7 of a perpetual stream of $2,500 annual payments that begins at
# Date t = 15?
library(scales)

cat("Interest rate",
    label_percent(accuracy=0.01)(interest_rate <- .061), "\n")

cat("The payments begin at t =", first_payment_date <- 15,
    "and we are valuing the payments at t =", valuation_date <- 7, "\n")

cat("Cash flow",
    label_dollar(accuracy=0.01)(cash_flow <- 2500), "\n")

cat("Value of perpetuity at t =", valuation_date,
    label_dollar(accuracy=0.01)(value_at_valuation_date <- cash_flow / interest_rate *
        (1 + interest_rate)^-(first_payment_date - valuation_date - 1)), "\n")

# Check
check_value <- sum(cash_flow * (1 + interest_rate)^(-(first_payment_date:500))) *
               (1 + interest_rate)^valuation_date

cat("TEST: check_value should be within half a cent of value_at_valuation_date",
    abs(check_value - value_at_valuation_date) < 5e-3, "\n")
