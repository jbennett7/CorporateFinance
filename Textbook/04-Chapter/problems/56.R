# 56. Balloon Payments
# On September 1, 2009, Susan Chao bought a motorcycle for $30,000. She paid
# $1,000 down and financed the balance with a five-year loan at a stated annual
# interest rate of 7.2 percent, compounded monthly. She started the monthly
# payments exactly one month after the purchase (i.e., October 1, 2009). Two
# years later, at the end of October 2011, Susan got a new job and decided to
# pay off the loan. If the bank charges her a 1 percent prepayment penalty
# based on the loan balance, how much must she pay the bank on November 1,
# 2011?
library(scales)

pct_balance <- function(interest_rate, age, term)
    1 - ((1 + interest_rate/12)^age - 1) / ((1 + interest_rate/12)^term - 1)

# The principal
principal <- 29e3
interest_rate <- .072
term <- 12*5

# Monthly Payment
label_dollar()(payment <- principal * (interest_rate/12) /
               (1 - (1 + interest_rate/12)^-term))

# The balance on November 1, 2011
label_dollar()(nov_2011_balance <- principal *
               pct_balance(interest_rate, 26, term))

# Prepayment penalty
label_dollar()(prepayment_penalty <- nov_2011_balance * .01)

# The November 1, 2011 payment (November balance + payment + prepayment penalty)
label_dollar()(balloon_payment <- nov_2011_balance + payment + prepayment_penalty)
