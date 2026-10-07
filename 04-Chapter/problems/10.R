# 10. Continuous Compounding
# Compute the future value of $1,900 continuously compounded for
#  a. 7 years at a stated annual interest rate of 12 percent.
#  b. 5 years at a stated annual interest rate of 10 percent.
#  c. 12 years at a stated annual interest rate of 5 percent.
#  d. 10 years at a stated annual interest rate of 7 percent.

years <- c(7, 5, 12, 10)
interest <- c(.12, .10, .05, .07)

1900*exp(interest * years)
