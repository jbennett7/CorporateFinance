# 48. Calculating Present Value
# A 5-year annuity of ten $5,300 semiannual payments will begin 9 years from
# now, with the first payment coming 9.5 years from now. If the discount rate
# is 12 percent compounded monthly, what is the value of this annuity five
# years from now? What is the value three years from now? What is the current
# value of the annuity?
library(scales)

payment <- 5300
number_of_payments <- 10
annuity_begins <- 9
# The discount rate is the annual interest rate
apr <- .12
# The monthly rate 
monthly_rate <- apr/12
# Exponent = compounding periods per year / payments per year
# We calculate the semi-annual rate as a rate for a pay period of six months
# compounded monthly.
semiannual_rate <- (1 + monthly_rate)^(12/2) - 1
pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate
present_value <- function(rate, months) (1 + rate)^-months

cat("The present value of the annuity at its starting point of year",
    annuity_begins, "is",
    label_dollar(accuracy=0.01)
        (pv_9 <- payment * pvia(semiannual_rate, number_of_payments)), "\n")

cat("The present value five years from now",
    label_dollar(accuracy=0.01)
        (pv_5 <- pv_9 * present_value(monthly_rate, (annuity_begins-5)*12)),
    "\n")

cat("The present value three years from now",
    label_dollar(accuracy=0.01)
        (pv_3 <- pv_9 * present_value(monthly_rate, (annuity_begins-3)*12)),
    "\n")

cat("The present value today is",
    label_dollar(accuracy=0.01)
        (pv_0 <- pv_9 * present_value(monthly_rate, annuity_begins*12)), "\n")

# Check
payments <- c(rep(0, annuity_begins*12),
              rep(c(rep(0, 12/2-1), payment), number_of_payments))
pv_0_check <- sum(payments * (1 + monthly_rate)^-(1:length(payments)))
cat("TEST: that the difference between pv_0_check and pv_0 is within half a cent",
    abs(pv_0_check - pv_0) < 5e-3, "\n")
