# 51. Calculating Annuities Due
# You want to lease a set of golf clubs from Pings Ltd. The lease contract is
# in the form of 24 equal monthly payments at a 10.4 percent stated annual
# interest rate, compounded monthly. Because the clubs cost $2,300 retail,
# Pings wants the PV of the lease payments to equal $2,300. Suppose that your
# first payment is due immediately. What willyour monthly lease payments be?

problem_51 <- function() {
    term <- 24
    rate_annual <- 10.4
    PV <- 2300
    
    annuity_factor <- (1 + sum((1 + rate_annual/1200)^(-(1:(term-1)))))
    PV/annuity_factor
}
