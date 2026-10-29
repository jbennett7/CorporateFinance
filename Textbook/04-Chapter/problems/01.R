# 1. Simple Interest versus Compound Interest
# First City Bank pays 8 percent simple interest on its savings account
# balances, whereas Second City Bank pays 8 percent interest compounded
# annually. If you made a $5,000 deposit in each bank, how much more money
# would you earn from your Second City Bank account at the end of 10 years?
library(scales)

# With 8% simple interest, first city bank's interest would be
cat("Simple interest at 8%:",
    label_dollar(accuracy=0.01)(first_city <- 5000 * (1 + 10 * .08)),
    "\n")

# With 8% interest compounded annually, second city ban's interest would be
cat("8% interest compunded annually:",
    label_dollar(accuracy=0.01)(second_city <- 5000 * (1 + .08)^10),
    "\n")

# The dollar difference between the two
cat("Dollar difference between simple and compounded interest:",
    label_dollar(accuracy=0.01)(second_city - first_city),
    "\n")
