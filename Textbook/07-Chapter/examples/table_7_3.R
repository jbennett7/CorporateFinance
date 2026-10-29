options(warn = 2)
expected <- rbind(
    market_size   = c(-1802,  1517, 8154),
    market_share  = c(-696,   1517, 5942),
    price         = c( 853,   1517, 2844),
    variable_cost = c( 189,   1517, 2844),
    # book shows fixed_cost = 1628; it rounds intermediate cash flows.
    fixed_cost    = c( 1295,  1517, 1627),
    investment    = c( 1208,  1517, 1903))
colnames(expected) <- c("pessimistic", "base", "optimistic")

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

# List
# Pros: Each variable's scenarios sit on one line, so it reads like a row from
#       Table 7.3 and `SEC[[v]]` hands the sensitivity loop exactly what it
#       needs. Elements are independent, so variables could have different
#       numbers of scenarios or even different types.
# Cons: Scenario names are repeated in every vector, typos go unnoticed and
#       `sapply` takes the labels from the first vector only. The base case has
#       to be extracted with `lapply(SEC, `[[`, "base")`
# Use a list when variables differ in shape or type, or when you think of the
# data one variable at a time.
SEC <- list(
market_size      = c(pessimistic = 5e3,    base = 10e3,   optimistic = 20e3),
market_share     = c(pessimistic = .20,    base = .30,    optimistic = .50),
price            = c(pessimistic = 1.9e6,  base = 2e6,    optimistic = 2.2e6),
variable_cost    = c(pessimistic = 1.2e6,  base = 1e6,    optimistic = .8e6),
fixed_cost       = c(pessimistic = 1891e6, base = 1791e6, optimistic = 1741e6),
investment       = c(pessimistic = 1900e6, base = 1500e6, optimistic = 1000e6)
)
base <- lapply(SEC, `[[`, "base")
table_7_3 <- t(sapply(names(SEC), function(v) {
    sapply(SEC[[v]], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)

# Matrix
# Pros: The layout is Table 7.3, so the input and output have the same shape.
#       The lookup syntax is clean, and the matrix can be built with
#       `outer()`-style loops over `rownames x colnames`.
# Cons: All columns must be the same type, which is fine here since everything
#       is numeric.
# Use Matrix when you want to vary one input at a time for sensitivity analysis.
SEC <- rbind(
    market_size   = c(5e3,    10e3,   20e3),
    market_share  = c(.20,    .30,    .50),
    price         = c(1.9e6,  2e6,    2.2e6),
    variable_cost = c(1.2e6,  1e6,    .8e6),
    fixed_cost    = c(1891e6, 1791e6, 1741e6),
    investment    = c(1900e6, 1500e6, 1000e6))
colnames(SEC) <- c("pessimistic", "base", "optimistic")
SEC
base <- as.list(SEC[, "base"])
table_7_3 <- t(sapply(rownames(SEC), function(v) {
    sapply(SEC[v,], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)

# Wide data frame
# Pros: It reads and writes to CSV with `read.csv()`. You could also add a
#       `units` or `description` column of text.
# Cons: Getting one cell is clumsier: `SEC$optimistic[SEC$variable == "price"]`.
# Use the wide data frame if you want to easily read and write to csv files.
SEC <- data.frame(
    variable    = c("market_size", "market_share", "price", "variable_cost",
                    "fixed_cost", "investment"),
    pessimistic = c(5e3,  .20, 1.9e6, 1.2e6, 1891e6, 1900e6),
    base        = c(10e3, .30, 2e6,   1e6,   1791e6, 1500e6),
    optimistic  = c(20e3, .50, 2.2e6, .8e6,  1741e6, 1000e6))

base <- as.list(SEC$base)
names(base) <- SEC$variable
table_7_3 <- t(sapply(SEC$variable, function(v) {
    sapply(SEC[SEC$variable == v,
           c("pessimistic", "base", "optimistic")], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)

# Long ("tidy") data frame
# Pros: It's the standard format for the tidyverse and `ggplot2`. A tornado
#       chart of the sensitivity results is natural in this form. It also
#       handles uneven data easily: if one variable had a fourth scenario,
#       you'd just add a row.
# Cons: It's the most verbose to type by hand, and you have to reshape it to
#       get the book's layout back.
# Use the long data frame if you want to plot or summarize the results.
SEC <- tibble::tribble(
    ~variable,     ~scenario,   ~value,
    "market_size",   "pessimistic", 5e3,
    "market_size",   "base",        10e3,
    "market_size",   "optimistic",  20e3,
    "market_share",  "pessimistic", .20,
    "market_share",  "base",        .30,
    "market_share",  "optimistic",  .50,
    "price",         "pessimistic", 1.9e6,
    "price",         "base",        2e6,
    "price",         "optimistic",  2.2e6,
    "variable_cost", "pessimistic", 1.2e6,
    "variable_cost", "base",        1e6,
    "variable_cost", "optimistic",  .8e6,
    "fixed_cost",    "pessimistic", 1891e6,
    "fixed_cost",    "base",        1791e6,
    "fixed_cost",    "optimistic",  1741e6,
    "investment",    "pessimistic", 1900e6,
    "investment",    "base",        1500e6,
    "investment",    "optimistic",  1000e6)

base_rows <- dplyr::filter(SEC, scenario == "base")
base      <- as.list(setNames(base_rows$value, base_rows$variable))
table_7_3 <- SEC |>
    dplyr::mutate(npv = purrr::map2_dbl(variable, value, \(v, x) {
        do.call(npv_sec, modifyList(base, setNames(list(x), v))) / 1e6
    })) |>
    dplyr::select(-value) |>
    tidyr::pivot_wider(names_from = scenario, values_from = npv)
stopifnot(all(round(as.matrix(table_7_3[, -1])) == expected))
knitr::kable(table_7_3, digits = 0)

# Transposed list: one entry per scenario
# Pros: Each element is already a complete set of inputs for `npv_sec`. That
#       fits scenario analysis (Table 7.4) better than sensitivity analysis,
#       because there you evaluate whole scenarios. Adding a "plane crash"
#       scenario is one more list entry.
# Cons: Sensitivity analysis needs a "swap one variable" step again.
# Use a transposed list if you want to evaluate a whole scenario.
SEC <- list(
    pessimistic = c(market_size = 5e3, market_share = .20, price = 1.9e6,
                    variable_cost = 1.2e6, fixed_cost = 1891e6,
                    investment = 1900e6),
    base        = c(market_size = 10e3, market_share = .30, price = 2e6,
                    variable_cost = 1e6, fixed_cost = 1791e6,
                    investment = 1500e6),
    optimistic  = c(market_size = 20e3, market_share = .5, price = 2.2e6,
                    variable_cost = .8e6, fixed_cost = 1741e6,
                    investment = 1000e6))
base <- as.list(SEC$base)
table_7_3 <- sapply(names(SEC), function(x) {
    sapply(names(base), function(v) {
        z <- SEC[[x]][[v]]
        some_list <- modifyList(base, setNames(list(z), v))
        do.call(npv_sec, some_list)/1e6
    })
})
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)
