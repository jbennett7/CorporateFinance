# 54. Calculating Annuities
# You have recently won the super jackpot in the Washington State Lottery. On
# reading the fine print, you discover that you have the following two options:
#
#a. You will receive 31 annual payments of $250,000, with the first payment
#   being delivered today. The income will be taxed at a rate of 28 percent. Taxes
#   will be withheld when the checks are issued.
#b. You will receive $530,000 now, and you will not have to pay taxes on this
#   amount. In addition, beginning one year from today, you will receive $200,000
#   each year for 30 years. The cash flows from this annuity will be taxed at 28
#   percent.
# Using a discount rate of 7 percent, which option should you select?
library(scales)

pvia <- function(r, t) sum((1 + r)^(-(1:t)))

label_dollar(accuracy=1)(a <- 250e3 * (1-.28) + 250e3 * pvia(.07, 30) * (1 - .28))
label_dollar(accuracy=1)(b <- 530e3 + 200e3 * pvia(.07, 30) * (1 - .28))

