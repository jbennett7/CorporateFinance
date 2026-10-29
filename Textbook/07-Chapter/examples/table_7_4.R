# Sensitivity analysis changes one input at a time and holds the rest at base.
# Scenario analysis tells a single story and changes several inputs together
# because they're linked. That linkage is what makes it more realistic than
# sensitivity analysis.

# Step 1: Read Table 7.4 and note these four things:
# 1. Note which inputs change:
#     Market size, Market share
# 2. Which inputs stay at their base values?
#     price, variable cost, fixed cost and investment.
# 3. What is the new NPV?
#     -$2,023 = -$1,500 - $156 x PVIA(.15, 5)
# 4. Why do these particular inputs move together? What story connects them
#     Plane Crash Scenario - what if one of the planes of an airline crashes.
#     In this scenario there will be a decrease in the number of passengers.
#     This will affect the number of planes that will be ordered and thus the
#     number of planes purchased by the airlines. Since solar-powered airplanes
#     are new to the market, the public may blame the event on new technology.
#     This means that the market_share variable will decrease as well.
options(warn = 2)
expected <- -2023
tol <- 1
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
# These are global variables
table_7_2 <- read.csv("07-Chapter/data/table_7_2.csv")
scenarios <- colnames(table_7_2)[-1]
vars <- table_7_2$variable


transpose_7_2 <- sapply(scenarios,
                        function(s) setNames(table_7_2[, s], vars),
                        simplify = FALSE)

base <- as.list(transpose_7_2$base)
plane_crash <- modifyList(base, list(market_size = 7e3, market_share = .2))

npv_plane_crash <- do.call(npv_sec, plane_crash)/1e6
stopifnot(abs(npv_plane_crash - expected) < tol)
print(npv_plane_crash)
