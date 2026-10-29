# 59. Calculating Annuity Values
# An All-Pro defensive lineman is in contract negotiations. The team has
# offered the following salary structure
# Time: 0, 1, 2, 3, 4, 5, 6
# Salary $8,500,000; $3,900,000; $4,600,000; $5,300,000; $5,800,000; $6,400,000; $7,300,000
# All salaries are to be paid in a lump sum. The player has asked you as his
# agent to renegotiate the terms. He wants a $10 million signing bonus payable
# today and a contract value increase of $1,500,000. He also wants an equal
# salary paid every three months, with the first paycheck three months from
# now. If the interest rate is 5 percent compounded daily, what is the amount
# of his quarterly check? Assume 365 days in a year.
library(scales)

time   <- c(0, 1, 2, 3, 4, 5, 6)
number_of_quarters <- 6*4
salary <- c(85e5, 39e5, 46e5, 53e5, 58e5, 64e5, 73e5)
pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

apr <- .05
ear <- (1 + apr/365)^365 - 1
quarterly_rate  <- (1 + apr/365)^(365/4) - 1

pv_first_offer <- sum(salary * (1 + ear)^(-time))
cat("All values are displayed to the dollar, but are stored more accurately.\n")

cat("The present value of the first offer",
    label_dollar()(pv_first_offer), "\n")

cat("Signing bonus, paid upfront",
    label_dollar()(signing_bonus <- 10e6), "\n")

cat("Negotiated salary increase as a present value amount",
    label_dollar()(salary_increase <- 15e5), "\n")

cat("Value of the renegotiated contract (first offer + increase)",
    label_dollar()(pv_renegotiated <- pv_first_offer + salary_increase), "\n")

cat("The present value of the renegotiated contract minus the signing bonus",
    label_dollar()(pv_annuity <- pv_renegotiated - signing_bonus), "\n\n")

cat("The quarterly payment",
    label_dollar()(quarterly_payment <- pv_annuity /
                   pvia(quarterly_rate, number_of_quarters)),
    "\n\n")

pv_annuity_check <- sum(quarterly_payment /
                        (1 + apr/365)^((1:number_of_quarters)*365/4))

cat("TEST: pv_annuity is within half a cent of the check",
    abs(pv_annuity - pv_annuity_check) < 5e-3, "\n")
