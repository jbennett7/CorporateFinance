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

# These are global variables
table_7_2 <- read.csv("Data/table_7_2.csv")
scenarios <- colnames(table_7_2)[-1]
vars <- table_7_2$variable


# Build List
list_7_2 <- sapply(vars,
                   function(v) unlist(table_7_2[vars == v, scenarios]),
                   simplify = FALSE)

# Test List
base <- lapply(list_7_2, `[[`, "base")
table_7_3 <- t(sapply(names(list_7_2), function(v) {
    sapply(list_7_2[[v]], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)


# Build Matrix
matrix_7_2 <- as.matrix(table_7_2[, scenarios])
rownames(matrix_7_2) <- vars

# Test Matrix
base <- as.list(matrix_7_2[, "base"])
table_7_3 <- t(sapply(rownames(matrix_7_2), function(v) {
    sapply(matrix_7_2[v,], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)


# Build Wide data frame
wide_7_2 <- table_7_2

# Test Wide data frame
base <- as.list(wide_7_2$base)
names(base) <- wide_7_2$variable
table_7_3 <- t(sapply(wide_7_2$variable, function(v) {
    sapply(wide_7_2[wide_7_2$variable == v, names(wide_7_2)[-1]], function(x) {
        some_list <- modifyList(base, setNames(list(x), v))
        do.call(npv_sec, some_list)/1e6
    })
}))
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)


# Build Long data frame
long_7_2 <- tidyr::pivot_longer(table_7_2, cols = -variable,
                    names_to = "scenario", values_to = "value")

# Test Long data frame
base_rows <- dplyr::filter(long_7_2, scenario == "base")
base      <- as.list(setNames(base_rows$value, base_rows$variable))
table_7_3 <- long_7_2 |>
    dplyr::mutate(npv = purrr::map2_dbl(variable, value, \(v, x) {
        do.call(npv_sec, modifyList(base, setNames(list(x), v))) / 1e6
    })) |>
    dplyr::select(-value) |>
    tidyr::pivot_wider(names_from = scenario, values_from = npv)
stopifnot(all(round(as.matrix(table_7_3[, -1])) == expected))
knitr::kable(table_7_3, digits = 0)


# Build Transposed list
transpose_7_2 <- sapply(scenarios,
                        function(s) setNames(table_7_2[, s], vars),
                        simplify = FALSE)

# Test Transposed list
base <- as.list(transpose_7_2$base)
table_7_3 <- sapply(names(transpose_7_2), function(x) {
    sapply(names(base), function(v) {
        z <- transpose_7_2[[x]][[v]]
        some_list <- modifyList(base, setNames(list(z), v))
        do.call(npv_sec, some_list)/1e6
    })
})
stopifnot(all(round(table_7_3) == expected))
knitr::kable(table_7_3, digits = 0)
