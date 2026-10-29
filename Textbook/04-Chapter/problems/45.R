# 45. Comparing Cash Flow Streams
# You have your choice of two investment accounts. Investment A is a 15-year
# annuity that features end-of-month $1,500 payments and has an interest rate
# of 8.7 percent compounded monthly. Investment B is an 8 percent continuously
# compounded lump-sum investment, also good for 15 years. How much money would
# you need to invest in B today for it to be worth as much as Investment A 15
# years from now?
library(scales)

# Investment A
cash_flow <- 1500
interest_rate_A <- .087/12
term_A <- 15*12

# Future value annuity factor
fvia <- function(rate, term) ((1 + rate)^term - 1) / rate
# Present value annuity factor
pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

cat("The future value of investment A is",
    label_dollar(accuracy=0.01)(future_value_A <- cash_flow *
                                fvia(interest_rate_A, term_A)), "\n")

interest_rate_B <- .08
term_B <- 15
cat("Invest",
    label_dollar(accuracy=0.01)(investment_B <- future_value_A /
                                exp(interest_rate_B * term_B)),
    "today in order for B to equal the value of A.\n")

cat("The present value of investment A is",
    label_dollar(accuracy=0.01)(pv_investment_A <- cash_flow *
                                pvia(interest_rate_A, term_A)), "\n")

cat("\nInvestment B is larger than investment A's present value\n   ",
    "because the growth factor of A",
    (1 + interest_rate_A)^term_A,
    "is larger\n    than the growth factor of B",
    exp(interest_rate_B * term_B), "\n")

# Check
fv_A_check <- pv_investment_A * (1 + interest_rate_A)^term_A
cat("TEST: ensure fv_A_check equal future_value_A",
    abs(future_value_A - fv_A_check) < 5e-3, "\n")
