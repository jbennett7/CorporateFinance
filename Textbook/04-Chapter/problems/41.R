# 41. EAR versus APR
# You have just purchased a new warehouse. To finance the purchase, you've
# arranged for a 30-year mortgage for 80 percent of the $4,500,000 purchase
# price. The monthly payment on this loan will be $27,500. What is the APR on
# this loan? The EAR?
library(scales)
library(tibble)

cat("80 percent of the purchase price is",
    label_dollar(accuracy=0.01)(principal <- 4.5e6 * .80),
    "\n")

cat("The term of the mortgage is", term <- 30 * 12, "months\n")

cat("The monthly payment on this loan will be",
    label_dollar(accuracy=0.01)(monthly_payment <- 27.5e3),
    "\n")

payment_gap <- function(apr, balance, payment, term)
    balance * (apr/12) / (1 - (1 + apr/12)^-term) - payment

cat("The APR is",
    label_percent(accuracy=0.01)(apr <-
        uniroot(payment_gap,
                balance = principal,
                payment = monthly_payment,
                term = term,
                interval = c(1e-4, 1),
                tol = 1e-14)$root),
    "\n")

cat("The EAR can be calculated from the APR as",
    label_percent(accuracy=0.01)(ear <- (1 + apr/12)^12 - 1),
    "\n")

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate
principal_check <- monthly_payment * pvia(apr/12, term)
cat("TEST: Confirm that principal_check within half a cent of the principal:",
    abs(principal_check - principal) < 5e-3,
    "\n")

amortize_principal <- function(balance, payment, interest_rate, periods) {

    begin     <- numeric(periods)
    interest  <- numeric(periods)
    principal <- numeric(periods)
    end       <- numeric(periods)

    for (i in 1:periods){
        begin[i]     <- if (i == 1) balance else end[i-1]
        interest[i]  <- begin[i] * interest_rate
        principal[i] <- payment - interest[i]
        end[i]       <- begin[i] - principal[i]
    }

    tibble(Begin = begin, Interest = interest, Principal = principal, End = end)
}

amortization_table <- amortize_principal(principal, monthly_payment, apr/12, term)
cat("TEST: Confirm that the final balance is within half a cent of $0:",
    abs(tail(amortization_table, n=1)$End) < 5e-3,
    "\n")
