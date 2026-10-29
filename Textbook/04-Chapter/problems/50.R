# 50. Calculating Annuitites Due
# You want to buy a new sports car from Muscle Motors for $73,000. The contract
# is in the form of a 60-month annuity due at a 6.45 percent APR. What will
# your monthly payment be?
library(scales)

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

cat("Cost:", label_dollar(accuracy=0.01)(principal <- 73e3),
    "\nAPR:", label_percent(accuracy=0.01)(apr <- 0.0645),
    "\nTerm:", term <- 60, "months",
    "\nMonthly payment:", label_dollar(accuracy=0.01)
        (monthly_payment <- principal / (1 + pvia(apr/12, term-1))),
    "\n")

# Check
check_monthly_payment <- principal / sum((1 + apr/12)^(-(0:(term-1))))
cat("\nDifference monthly_payment and check_monthly_payment is within half a cent:",
    abs(check_monthly_payment - monthly_payment) < 5e-3, "\n")
