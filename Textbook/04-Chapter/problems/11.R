# 11. Present Value and Multiple Cash Flow
# Conoly Co. has identified an investment project with the following cash
# flows. If the discount rate is 10 percent, what is the present value of these
# cash flows? What is the present value at 18 percent? At 24 percent?

year <- c(1, 2, 3, 4)
cash_flow <- c(960, 840, 935, 1350)
rates <- c(.10, .18, .24)

lapply(rates, function(r) sum(cash_flow * (1 + r)^(-year)))
