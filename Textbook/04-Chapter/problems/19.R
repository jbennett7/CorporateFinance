# 19. Calculating Number of Periods.
# One of your customers is delinquent on his accounts payable balance. You've
# mutually agreed to a repayment schedule of $700 per month. You will charge
# 1.3 percent per month interest on the overdue balance. If the current balance
# is $21,500, how long will it take for the account to be paid off?

f <- function(n) 700 * (1 - (1 + .013)^(-n)) / .013 - 21500
uniroot(f, c(0, 50))$root
