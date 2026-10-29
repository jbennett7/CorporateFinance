# 8. Calculating Rates of Return
# Although appealing to more refined tastes, art as a collectible has not always
# performed so profitably. During 2010, Deustcher-Menzies sold Arkies under the
# Shower, a painting by renowned Australian painter Brett Whiteley, at auction
# for a price of $1,100,000. Unfortunately for the previous owner he had
# purchased it three years earlier at a price of $1,680,000. What was his
# annual rate of return on this painting?
library(scales)

cat("Current auction selling price for painting:",
    label_dollar()(current <- 1100e3),
    "\nPrevious price three years earlier:",
    label_dollar()(previous <- 1680e3),
    "\nAnnual rate of return:",
    label_percent(accuracy=0.01)(ror <- (current/previous)^(1/3) - 1),
    "\n")
