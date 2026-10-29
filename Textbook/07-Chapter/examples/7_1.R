suppressMessages({
    library(dplyr)
    library(scales)
})

pretty <- function(x, n=0) format(round(as.numeric(x), n), big.mark='{,}')

format_table_7_2 <- function(df) {
    df[1,2:4] <- format(df[1,2:4], big.mark=',')
    df[2,2:4] <- sprintf("%d%%", as.numeric(df[2,2:4])*100)
    df[3,2:4] <- paste0("$", format(as.numeric(df[3,2:4]), big.mark=','))
    df[4,2:4] <- paste0("$", format(as.numeric(df[4,2:4]), big.mark=','))
    df[5,2:4] <- paste0(format(as.numeric(df[5,2:4])/1e6, big.mark=','), " million")
    df[6,2:4] <- paste0(format(as.numeric(df[6,2:4])/1e6, big.mark=','), " million")
    knitr::kable(df)
}

pvia <- function(rate, time_span) sum((1 + rate)^(-(1:time_span)))

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

calc_cash_flows <- function(market_size, market_share, price, variable_cost,
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
    npv          <- -investment + cash_flow * pvia(discount_rate, time_span)

    list(
        units        = units,
        revenues     = revenues,
        var_costs    = var_costs,
        fixed_costs  = fixed_cost,
        depreciation = depreciation,
        pretax       = pretax,
        tax          = tax,
        net_profit   = net_profit,
        cash_flow    = cash_flow,
        npv          = npv
    )
}

