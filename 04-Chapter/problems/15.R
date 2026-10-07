# 15. Calculating EAR
# Find the EAR in each of the following cases:

APR <- c(.07, .16, .11)
m <- c(4, 12, 365)

round(((1 + APR/m)^(m) - 1)*100, 2)

# Infinite
round((exp(.12) - 1)*100, 2)


# For an annual rate of 0.0582 compounded quarterly what is the quarterly rate
# and the continuously compounded annual rate equivalent.
apr <- .0582
# Quarterly rate
(q <- apr/4)
# Continuously compounded annual rate equivalent
4 * log(1 + q)
