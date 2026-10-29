# 31. Calculating Interest Expense
# You receive a credit card application from Shady Bank Savings and Loan
# offering an introductory rate of 2.40 percent per year, compounded monthly
# for the first six months, increasing thereafter to 18 percent compounded
# monthly. Assuming you transfer the $7,500 balance from your existing credit
# card and make no subsequent payments, how much interest will you owe at the
# end of the first year?
library(scales)

balance <- 7500

# balance at t=6 months
label_dollar(accuracy=0.01)(balance_6months <- balance * (1 + .024/12)^6)

# balance at t=12 months using the balance at t=6 as the principal
label_dollar(accuracy=0.01)(balance_12months <- balance_6months * (1 + 0.18/12)^6)

# interest = balance_12months - balance
label_dollar(accuracy=0.01)(interest_12months <- balance_12months - balance)
