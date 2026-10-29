# 37. Calculating Annuity Present Values
# You want to borrow $65,000 from your local bank to buy a new sailboat. You
# can afford to make monthly payments of $1,320, but no more. Assuming monthly
# compounding, what is the highest APR you can afford on a 60-month loan?
library(scales)

# present value annuity factor
pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

principal <- 65e3
monthly_payment <- 1320
term <- 60

npv_at <- function(rate, principal, monthly_payment, term)
    monthly_payment * pvia(rate, term) - principal

# calculate the monthly rate
monthly_rate <- uniroot(npv_at,
    principal = principal, monthly_payment = monthly_payment, term = term,
    interval = c(1e-6, 1), tol=1e-8)$root
# multiply the monthly rate by 12 to get the APR
label_percent(accuracy=0.01)(apr <- monthly_rate * 12)

# plug-back check must be within half a cent
abs(principal / pvia(monthly_rate, term) - monthly_payment) <= 5e-3
# ensure that a higher APR rate will exceed the monthly payments
principal / pvia(monthly_rate+.0001, term) > monthly_payment
