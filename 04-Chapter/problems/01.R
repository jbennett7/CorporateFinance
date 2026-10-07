# 1. Simple Interest versus Compound Interest
# First City Bank pays 8 percent simple interest on its savings account
# balances, whereas Second City Bank pays 8 percent interest compounded
# annually. If you made a $5,000 deposit in each bank, how much more money
# would you earn from your Second City Bank account at the end of 10 years?

problem_1 <- function() {
    first_city <- 5000 * (1 + 10 * .08)
    second_city <- 5000 * (1 + .08)^10
    c(first_city, second_city, second_city - first_city)
}
