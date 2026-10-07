# 12. Present Value and Multiple Cash Flows
# Investment X offers to pay you $4,500 per year for nine years, whereas
# Investment Y offers you $7,000 per year for five years. Which of these cash
# flow streams has the higher present value if the discount rate is 5 percent?
# If the discount rate is 22 percent?

pvia <- function(r, y) sum((1 + r)^(-(1:y)))

cf = c(4500, 7000)
y = c(9, 5)
r <- c(.05, .22)

for (i in seq(r)){
    items <- unlist(lapply(1:2, function(x) sum(cf[x] * pvia(r[i], y[x]))))
    imax <- which(items == max(items))
    cat("\n")
    cat("For a rate of", r[i]*100, "% the", cf[imax], "investment is more attractive")
    cat(" with a present value of $", format(items[imax], big.mark=","), "\n")
    cat("present values:")
    print(paste0("$ ", format(items, big.mark=",")))
}
