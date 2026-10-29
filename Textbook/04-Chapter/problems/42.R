# 42. Present Value and Break-Even Interest
# Consider a firm with a contract to sell an asset for $115,000 three years
# from now. The asset costs $76,000 to produce today. Given a relevant discount
# rate on this asset of 13 percent per year, will the firm make a profit on
# this asset? At what rate does the firm just break even?
library(scales)

cat("Value of asset to sell three years from now",
    label_dollar(accuracy=0.01)(price <- 115e3),
    "\n")

cat("Production cost today",
    label_dollar(accuracy=0.01)(cost <- 76e3),
    "\n")

cat("Discount rate for the asset",
    label_percent(accuracy=0.01)(discount_rate <- .13),
    "\n")

cat("The item will be sold in", term <- 3, "years","\n\n")

pv_factor <- function(rate, term) (1 + rate)^-term

cat("Present value of Profit/Loss on sale",
    label_dollar(accuracy=0.01)(npv <- price *
                                pv_factor(discount_rate, term) - cost),
    "\n")

cat("Does the firm make a profit?", npv > 0, "\n\n")

# r = (price / cost)^(1/term) - 1
cat("The break even rate is",
    label_percent(accuracy=0.01)(break_even_rate <- (price / cost)^(1/term) - 1),
    "\n\n"
)

# Check
check_break_even <- abs(cost - price * pv_factor(break_even_rate, term)) < 5e-3
cat("Confirm the break_even_rate", check_break_even, "\n")
