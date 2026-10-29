# 3. Calculating Present Values
# For each of the following, compute the present value
library(tibble)
library(dplyr)
library(scales)

# Calculating the present value from the future value
p3 <- tibble(
    Years           = c(6, 9, 18, 23),
    `Interest Rate` = c(7, 15, 11, 18)/100,
    `Future Value`  = c(13827, 43852, 725380, 590710),
    `Present Value` = `Future Value` * (1 + `Interest Rate`)^(-Years)
)

p3_view <- p3 |>
    mutate(
        `Interest Rate` = label_percent()(`Interest Rate`),
        `Future Value`  = label_dollar(accuracy=0.01)(`Future Value`),
        `Present Value` = label_dollar(accuracy=0.01)(`Present Value`)
    )

print(p3_view)
