# 2. Calculating Future Values
# Compute the future value of $1,000 compounded annually for
#  a. 10 years at 5 percent.
#  b. 10 years at 10 percent.
#  c. 20 years at 5 percent.
#  d. Why is the interest earned in part (c) not twice the amount earned in part (a)
library(scales)

principal <- 1e3
term <- c(10, 10, 20)
rate  <- c(.05, .1, .05)
future_value <- c()

cat("Future value calculations:\n")
for(i in 1:length(term)){
    cat(term[i], "years at", label_percent()(rate[i]), "interest:",
        label_dollar(accuracy=0.01)
            ((future_value <- c(future_value, principal *
                                   (1 + rate[i])^term[i]))[i]),
        "\n")
}

cat("\nThe reason why (c) is not twice the amount earned in (a) is\nbecause",
    "compound interest grows exponentially, not linearly.")
