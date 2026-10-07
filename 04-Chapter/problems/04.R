# 4. Calculating Interest Rates
# Solve for the unknown interst rate in each of the following

tibble::tibble(
    `Present Value` = c(242, 410, 51700, 18750),
    Years           = c(4, 8, 16, 27),
    `Future Value`  = c(307, 896, 162181, 483500),
    `Interest Rate` = ((`Future Value` / `Present Value`)^(1/Years) - 1)*100
)
