# 16. Calculating APR
# Find the APR, or stated rate, in each of the following cases:

m <- c(2, 12, 52)
EAR <- c(.098, .196, .083)

r2 <- function(r1, m1, m2) {
    m2 * ((1 + r1 / m1)^(m1/m2) - 1)
}

r2(EAR, 1, m)

exp(.143) - 1
