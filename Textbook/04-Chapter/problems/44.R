# 44. Variable Interest Rates
# A 15-year annuity pays $1,500 per month, and payments are made at the end of
# each month. If the interest rate is 12 percent compounded monthly for the
# first seven years, and 6 percent compounded monthly thereafter, what is the
# present value of the annuity?
library(scales)

cat("Cash flows", label_dollar(accuracy=0.01)(cash_flow <- 1500), "\n")

cat("Interest rate",
    label_percent(accuracy=0.01)((interest_rate_1 <- .12/12)*12),
    "for the first", (term_1 <- 7*12)/12, "years, compounded monthly\n")

cat("Interest rate",
    label_percent(accuracy=0.01)((interest_rate_2 <- .06/12)*12),
    "for the rest of the", (full_term <- 15*12)/12,
    "year annuity, compounded monthly\n")

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate
pv_factor <- function(rate, time) (1 + rate)^-time

cat("The present value of the annuity is",
    label_dollar(accuracy=0.01)(pv_annuity <-
        # Cash flows from month 1 to month 7*12
        cash_flow * pvia(interest_rate_1, term_1) +

        # Cash flows from month 7*12+1 to month 15*12
        cash_flow * pvia(interest_rate_2, full_term - term_1) *
        # Discounted at interest_rate_1 back 7*12 months
        pv_factor(interest_rate_1, term_1)),
    "\n")

# Check
rate_vector <- c(rep((1 + interest_rate_1)^-1, term_1),
                 rep((1 + interest_rate_2)^-1, full_term - term_1))

pv_annuity_check <- cash_flow * sum(cumprod(rate_vector))

cat("TEST: difference between pv_annuity_check and pv_annuity within half a cent",
    abs(pv_annuity_check - pv_annuity) < 5e-3, "\n")

