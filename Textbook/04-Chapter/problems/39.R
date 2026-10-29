# 39. Present and Future Values
# The present value of the following cash flow stream is $7,300 when discounted
# at 8 percent annually. What is the value of the missing cash flow?
library(scales)
library(dplyr, warn.conflicts = FALSE)

cf_stream <- tibble(
    Year = c(1, 2, 3, 4),
    `Cash Flow` = c(1500, NA, 2700, 2900)
)

view_stream <- function(stream) {
    mutate(stream, `Cash Flow` = label_dollar(accuracy=0.01)(`Cash Flow`)) |>
    print()
}

# 7,300 = 1,500 * (1.08)^-1 + X * (1.08)^-2 + 2,700 * (1.08)^-3 + 2,900 * (1.08)^-4
calc_missing_cf_value <- function(stream, discount_rate, present_value) {
    is_known     <- !is.na(stream$`Cash Flow`)
    years        <- stream$Year[is_known]
    missing_year <- stream$Year[!is_known]
    known_cf     <- stream$`Cash Flow`[is_known]

    stream$`Cash Flow`[!is_known] <- (present_value -
        sum(known_cf*(1 + discount_rate)^-years)) *
        (1 + discount_rate)^missing_year
    stream
}

cf_stream <- calc_missing_cf_value(cf_stream, .08, 7300)

cf_stream |> view_stream()

# Final check
check_cf_stream <- function(stream, discount_rate, present_value) {
    calculated_present_value <- sum(stream$`Cash Flow` *
                                    (1 + discount_rate)^-stream$Year)
    abs(calculated_present_value - present_value) < 5e-3
}

check_cf_stream(cf_stream, .08, 7300)
