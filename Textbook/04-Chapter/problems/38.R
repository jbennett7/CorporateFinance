# 38. Calculating Loan Payments
# You need a 30-year, fixed-rate mortgage to buy a new home for $250,000. Your
# mortgage bank will lend you the money at a 5.3 percent APR for this 360-month
# loan. However, you can only afford monthly payments of $950, so you offer to
# pay off any remaining loan balance at the end of the loan in the form of a
# single balloon payment. How large will this balloon payment have to be for
# you to keep your monthly payments at $950.
library(scales)
library(tibble)

# present value annuity factor
pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

principal <- 250e3
rate <- .053/12
term <- 360
monthly_payment <- 950

# shortfall today (PV of the balloon)
pv_balloon_payment <- principal - monthly_payment * pvia(rate, term)

# The payment is made at the end of the loan. Future value is
label_dollar(accuracy=0.01)(fv_balloon_payment <- pv_balloon_payment *
                            (1 + rate)^term)

# This is a rather large balloon payment because the $950 monthly does not
# cover the first month's interest. This can be seen in the amortization table
# below where the principal payment is negative.

# amortization method
amortize_principal <- function(balance, interest_rate, payment, term, stop=term) {

    # preallocate one slot per month
    begin     <- numeric(stop)
    interest  <- numeric(stop)
    principal <- numeric(stop)
    end       <- numeric(stop)

    for (i in 1:stop) {
        begin[i]     <- if (i == 1) balance else end[i-1]
        interest[i]  <- begin[i] * interest_rate
        principal[i] <- payment - interest[i]
        end[i]       <- begin[i] - principal[i]
    }

    tibble(Month = 1:stop, Begin = begin, Interest = interest,
           Principal = principal, End = end)
}
amortization_table <- amortize_principal(principal, rate, monthly_payment, term)
head(amortization_table, n=5)
tail(amortization_table, n=5)

# check between both calculations
abs(fv_balloon_payment - tail(amortization_table$End,n=1)) < 5e-3
