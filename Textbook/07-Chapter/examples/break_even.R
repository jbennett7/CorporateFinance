suppressMessages({
    library(ggplot2)
    library(tidyr)
})
tax_rate <- .34
discount_rate <- .15
time_span <- 5

pvia <- function(discount_rate, time_span)
    sum((1 + discount_rate)^(-(1:time_span)))

net_present_value <- function(investment, cash_flow, discount_rate, time_span)
    -investment + cash_flow * pvia(discount_rate, time_span)

equivalent_annual_cost <- function(investment, discount_rate, time_span) {
    investment / pvia(discount_rate, time_span)
}

outcome <- function(market_size, market_share, price, variable_cost,
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
    npv          <- net_present_value(investment, cash_flow,
                                      discount_rate, time_span)
    list(
        units         = units,
        revenues      = revenues,
        var_costs     = var_costs,
        depreciation  = depreciation,
        pretax        = pretax,
        tax           = -tax,
        net_profit    = net_profit,
        cash_flow     = cash_flow,
        npv           = npv,
        tax_rate      = tax_rate,
        discount_rate = discount_rate,
        time_span     = time_span
    )
}

table_7_2 <- read.csv("07-Chapter/data/table_7_2.csv")
scenarios <- colnames(table_7_2)[-1]
vars <- table_7_2$variable
transpose_7_2 <- sapply(scenarios,
                        function(s) setNames(table_7_2[, s], vars),
                        simplify = FALSE)

base <- as.list(transpose_7_2$base)
market_size <- base$market_size
market_share <- c(0, 1e3, 3e3, 10e3) / market_size
outcomes <- sapply(market_share, function(ms) {
    someList <- modifyList(base, list(
        market_share  = ms,
        tax_rate      = tax_rate,
        discount_rate = discount_rate,
        time_span     = time_span
    ))
    unlist(do.call(outcome, someList))
})

units        <- outcomes["units",]
revenues     <- outcomes["revenues",]/1e6
var_costs    <- outcomes["var_costs",]/1e6
depreciation <- outcomes["depreciation",][1]/1e6
pretax       <- outcomes["pretax",]/1e6
tax          <- outcomes["tax",]/1e6
npv          <- outcomes["npv",]/1e6
price        <- base$price/1e6
var_cost     <- base$variable_cost/1e6
fixed_costs  <- base$fixed_cost/1e6
investment   <- base$investment/1e6
fixed_dep    <- fixed_costs + depreciation
tot_costs    <- var_costs + fixed_dep

profit_at_units <- function(u) {
    market_share <- u / market_size
    someList <- modifyList(base, list(
        market_share  = market_share,
        tax_rate      = tax_rate,
        discount_rate = discount_rate,
        time_span     = time_span
    ))
    as.numeric(unlist(do.call(outcome, someList))['net_profit'])/1e6
}

# break-even units 
expected_beu <- 2091
calculated_beu <- (fixed_dep)/(price - var_cost)
beu <- ceiling(round(uniroot(profit_at_units, interval = c(1e3, 3e3), tol=1e-9)$root, 6))
stopifnot(beu == expected_beu)
stopifnot(beu == calculated_beu)
# break-even dollars
bed <- price * beu

account_plot_data <- data.frame(units, revenues, var_costs, fixed_dep, tot_costs) |>
    pivot_longer(
        cols      = c(revenues, var_costs, fixed_dep, tot_costs),
        names_to  = "graph",
        values_to = "dollars"
    )
accounting_be_plot <- ggplot(account_plot_data,
            aes(x = units, y = dollars, color = graph, group = graph)) +
    geom_line() +
    annotate("segment", x=0, xend=beu, y=bed,
             color="gray", linewidth=.2, linetype="dashed") +
    annotate("segment", x=beu, y=0, yend=bed,
             color="gray", linewidth=.2, linetype="dashed") +
    annotate("point", x=beu, y=bed, color="black", size=.5) +
    annotate("text", x=beu, y=-500,
             label = format(beu, big.mark=','),
             color="black", size=2) +
    annotate("text", x=-250, y=bed,
             label=paste0("$", format(bed, big.mark=',')),
             color="black", size=2) +
    labs(
        title="Accounting Break-Even",
        x="Annual sales in units",
        y="$ millions"
    )
ggsave("07-Chapter/images/account_break_even.png", accounting_be_plot, width = 8,
       height = 4, dpi = 150)

npv_at_units <- function(u) {
    market_share <- u / market_size
    someList <- modifyList(base, list(
        market_share = market_share,
        tax_rate      = tax_rate,
        discount_rate = discount_rate,
        time_span     = time_span
    ))
    as.numeric(unlist(do.call(outcome, someList))['npv'])/1e6
}
npv_var_costs <- var_costs * (1 - tax_rate) * pvia(discount_rate, time_span)
npv_revenues  <- revenues * (1 - tax_rate) * pvia(discount_rate, time_span)
# The npv_beu is higher than the accounting `beu` because we need to take into
#    account the opportunity costs of the initial investment when calculating
#    NPV. Here we use the equivalent_annual_cost function which uses the annuity
#    factor to calculate the opportunity costs at a 15% return on investment for
#    the investment over the time period, instead of just straight depreciation
#    costs which only recovers the $1,500M investment with no accounting for
#    opportunity costs.
# Sell the next unit up from fraction.
npv_beu            <- ceiling(round(uniroot(npv_at_units,
                                            interval = c(1e3, 3e3),
                                            tol=1e-9)$root, 6))
expected_npv_beu   <- 2315
calculated_npv_beu <- ceiling(round((
                equivalent_annual_cost(investment, discount_rate, time_span) +
                fixed_costs * (1 - tax_rate) - depreciation * tax_rate) /
                ((price - var_cost) * (1 - tax_rate)), 6))
stopifnot(npv_beu == expected_npv_beu)
stopifnot(npv_beu == calculated_npv_beu)
npv_bed <- price * (1 - tax_rate) * pvia(discount_rate, time_span) * npv_beu
npv_fixed_costs <- -rep(npv[1], 4)
npv_total_costs <- npv_fixed_costs + npv_var_costs
npv_plot_data <- data.frame(units, npv_revenues, npv_var_costs,
                            npv_fixed_costs, npv_total_costs) |>
    pivot_longer(
        cols      = c(npv_revenues, npv_var_costs, npv_fixed_costs, npv_total_costs),
        names_to  = "graph",
        values_to = "dollars"
    )
npv_be_plot <- ggplot(npv_plot_data,
            aes(x = units, y = dollars, color = graph, group = graph)) +
    geom_line() +
    annotate("segment", x=0, xend=npv_beu, y=npv_bed,
             color="gray", linewidth=.2, linetype="dashed") +
    annotate("segment", x=npv_beu, y=0, yend=npv_bed,
             color="gray", linewidth=.2, linetype="dashed") +
    annotate("point", x=npv_beu, y=npv_bed, color="black", size=.5) +
    annotate("text", x=npv_beu, y=-500,
             label=format(npv_beu, big.mark=','),
             color="black", size=2) +
    annotate("text", x=-250, y=npv_bed,
             label=paste0("$", format(round(npv_bed), big.mark=',')),
             color="black", size=2) +
    labs(
         title="Net Present Value Break-Even",
         x="Annual sales in units",
         y="$ millions"
    )

ggsave("07-Chapter/images/npv_break_even.png", npv_be_plot, width = 8, height = 4, dpi = 150)
