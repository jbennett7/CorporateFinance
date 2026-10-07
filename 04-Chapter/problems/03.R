# 3. Calculating Present Values
# For each of the following, compute the present value
p3 <- tibble::tibble(
    Years           = c(6, 9, 18, 23),
    `Interest Rate` = c(7, 15, 11, 18),
    `Future Value`  = c(13827, 43852, 725380, 590710),
    `Present Value` = `Future Value` * (1 + `Interest Rate`/100)^(-Years)
)
