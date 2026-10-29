# 43. Present Value and Multiple Cash Flows
# What is the present value of $5,000 per year, at a discount rate of 6
# percent, if the first payment is received 6 years from now and the last
# payment is received 25 years from now?
library(scales)

cat("Cash flow per year",
    label_dollar(accuracy=0.01)(cash_flow <- 5e3),
    "\n")

cat("Discount rate",
    label_percent(accuracy=0.01)(discount_rate <- .06),
    "\n")

cat("The cash flows begin", start <- 6, "years from now\n")

cat("The last payment is received", end <- 25, "years from now\n")

pvia      <- function(rate, term) (1 - (1 + rate)^-term) / rate
pv_factor <- function(rate, time) (1 + rate)^-time

cat("The present value of the cash flows is",
    label_dollar(accuracy=0.01)(pv_cash_flow <-
        cash_flow * pvia(discount_rate, end - start + 1) *
        pv_factor(discount_rate, start - 1)), "\n")

# Check
cash_flow_check <- c(rep(0, start-1), rep(cash_flow, end - start + 1))
pv_check <- sum(cash_flow_check * (1 + discount_rate)^(-(1:end)))
cat("TEST: pv_cash_flow and pv_check within half a cent",
    abs(pv_check - pv_cash_flow) < 5e-3, "\n")

# Just another way to check. Keep so I have a copy
pv_check_2 <- sum(cash_flow_check * cumprod(rep((1 + discount_rate)^-1, end)))
cat("TEST: pv_cash_flow and pv_check_2 within half a cent",
    abs(pv_check_2 - pv_cash_flow) < 5e-3, "\n")
