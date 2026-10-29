# 5. Calculating the Number of Periods
# Solve for the unknown number of years in each of the following

tibble::tibble(
    `Present Value` = c(625, 810, 18400, 21500),
    `Interest Rate` = c(9, 11, 17, 8),
    `Future Value` = c(1284, 4341, 402662, 173439),
    Years = log(`Future Value` / `Present Value`) / log(1 + `Interest Rate`/100),
)
