# 2. Calculating Future Values
# Compute the future value of $1,000 compounded annually for
#  a. 10 years at 5 percent.
#  b. 10 years at 10 percent.
#  c. 20 years at 5 percent.
#  d. Why is the interest earned in part (c) not twice the amount earned in part (a)

problem_2 <- function() {
    A <- 1000 * (1.05)^10
    B <- 1000 * (1.1)^10
    C <- 1000 * (1.05)^20
    c(A, B, C)
}

