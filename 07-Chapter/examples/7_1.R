# Sensitivity Analysis, Scenario Analysis, and Break-Even Analysis

# Solar Electronics Corporation (SEC) has recently developed a solar-powered
# jet engine and wants to go ahead with full-scale production.

# Since the NPV is positive classic financial theory states we should accept
# the project. However, let's look into the projected numbers and see how they
# were developed.

# Let's assume that the following variables make up our revenue assumptions:
# 1. Market Share (we projected it to be .30).
# 2. Size of market, total number of jet units that will be sold by all market
#    participants (we projected it to be 10,000).
# 3. Selling price per engine (we want to sell each engine at $2 million).

# Based on our projections we assume we will sell 3,000 (10,000 * .3) engines
# at a price per engine of $2 million, for a total revenue of $6,000 million.

# Let's assume that the following variables make up our cost assumptions:
# 1. variable cost per unit ($1 million * 3,000 units = $3,000 million)
# 2. Fixed cost per year ($1,791 million)

# Based on our projections we assume we will have a total cost before taxes of
# $4,791 million ($3,000 million + $1,791 million) per year.

# Different Estimates for Solar Electronics' Solar Plant Engine Variables
SEC.vars <- list(
market_size      = c(
    pessimistic = 5e3,
    base         = 10e3,
    optimistic   = 20e3),
market_share     = c(
    pessimistic = .20,
    base         = .30,
    optimistic   = .50),
price            = c(
    pessimistic = 1.9e6,
    base         = 2e6,
    optimistic   = 2.2e6),
variable_cost    = c(
    pessimistic = 1.2e6,
    base         = 1e6,
    optimistic   = .8e6),
fixed_cost       = c(
    pessimistic = 1891e6,
    base         = 1791e6,
    optimistic   = 1741e6),
investment       = c(
    pessimistic = 1900e6,
    base         = 1500e6,
    optimistic   = 1000e6)
)

npv_sec <- function(market_size, market_share, price, variable_cost,
                    fixed_cost, investment, tax_rate=.34,
                    discount_rate = .15, time_span=5) {

    units        <- market_size   * market_share
    revenues     <- price         * units
    var_costs    <- variable_cost * units
    depreciation <- investment/time_span
    pretax       <- revenues - var_costs - fixed_cost - depreciation
    tax          <- pretax * tax_rate
    net_profit   <- pretax - tax
    cash_flow    <- net_profit + depreciation

    -investment + cash_flow * sum((1 + discount_rate)^(-(1:time_span)))
}

table_7_3 <- t(sapply(names(SEC.vars), function(v) {
    base <- lapply(SEC.vars, `[[`, "base")
    sapply(SEC.vars[[v]], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
round(table_7_3)
