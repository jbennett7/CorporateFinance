# 21 Future Value
# What is the future value in six years of $1,000 invested in an account with a
# stated annual interest rate of 9 percent.
#  a. Compounded annually?
#  b. Compounded semiannually?
#  c. Compounded monthly?
#  d. Compounded continuously?
#  e. Why does the future value increase as the compounding period shortens.

compound <- c(1, 2, 12)
1000 * (1 + .09/compound)^(6*compound)
1000 * exp(.09*6)

# It increases because the interst is added back to the balance sooner and more
# often as the compounding period increases.
