# 7. Calculating Present Values
# Imprudential, Inc. has an unfunded pension liablity of $630 million that must
# be paid in 20 years. To assess the value of the firm's stock, financial
# analysts want to discount this liability back to the present. If the relevant
# discount rate is 7.1 percent, what is the present value of this liability?
library(scales)


cat("Unfunded pension liability:",
    label_dollar()(liability <- 630e6),
    "\nTo be paid in", term <- 20, "years.",
    "\nRelevant discount rate:",
    label_percent(accuracy=0.1)(discount_rate <- .071),
    "\nPresent value of liability:",
    label_dollar()(pv_liability <- liability * (1 + discount_rate)^(-term)),
    "\n")
