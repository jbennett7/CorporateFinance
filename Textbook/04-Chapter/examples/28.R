# Example 4.28 Partial Amortization, or "Bite the Bullet"
library(scales)

# A common arrangement in real estate lending might call for a 5-year loan
# with, say, a 15-year amortization. What this means is that the borrower makes
# a payment every month of a fixed amount based on a 15-year amortization.
# However, after 60 months, the borrower makes a single, much larger payment
# called a "balloon" or "bullet" to pay off the loan. Because the monthly
# payments don't fully pay off the loan, the loan is said to be partially
# amortized.

# Suppose we have a $100,000 commercial mortgage with a 12 percent APR and a
# 20-year (240-month) amortization. Further suppose the mortgage has a
# five-year balloon. What will the monthly payment be? How big will the balloon
# payment be?

#The monthly payment can be calculated based on an ordinary annuity with a
#present value of $100,000. There are 240 payments, and the interest rate is 1
#percent per month. The payment is

balance <- 100e3
rate <- .12 / 12
full_term <- 240
pvia <- function(rate, term_months) (1 - (1 + rate)^(-term_months)) / rate

label_dollar()(monthly_payment <- balance / pvia(rate, full_term))

# Now, there is an easy way and a hard way to determine the balloon payment.
# The hard way is to actually amortize the loan for 60 months to see what the
# balance is at that time. The easy way is to recognize that after 60 months,
# we have a 240 - 60 = 180-month loan. The payment is still $1,101.09 per
# month, and the interest rate is still 1 percent per month. The loan balance
# is thus the present value of the remaining payments:

partial_term <- full_term - 60
label_dollar()(balloon_payment <- monthly_payment * pvia(rate, partial_term))

# The balloon payment is a substantial $91,744. Why is it so large? To get an
# idea, consider the first payment on the mortgage. The interest in the first
# month is $100,000 x .01 = $1,000. Your payment is $1,101.09, so the loan
# balance declines by only $101.09. Because the loan balance declines so
# slowly, the cumulative "pay down" over five years is not great.
