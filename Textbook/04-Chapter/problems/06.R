# 6. Calculating the Number of Periods
# At 8 percent interest, how long does it take to double your money? To quadruple it?
library(scales)

cat("At", label_percent()(interest <- .08), "interest, it takes approximately",
    round(double <- log(2) / log(1.08)), "years to double, and",
    round(quadruple <- log(4) / log(1.08)), "years to quadruple.\n")
