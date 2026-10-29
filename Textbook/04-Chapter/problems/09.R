# 9. Perpetuities
# An investor purchasing a British consol is entitled to receive annual
# payments from the British government forever. What is the price of a consol
# that pays $150 annually if the next payment occurs one year from today? The
# market interest rate is 4.6 percent.
library(scales)

cat("Britich consol annual cash flow:",
    label_dollar()(cash_flow <- 150e3),
    "\nMarket interest rate:",
    label_percent(accuracy=0.1)(interest_rate <- .046),
    "\nPrice for Britich consol:",
    label_dollar(accuracy=0.01)(consol <- cash_flow / interest_rate), "\n")
