# 20. Calculating EAR
# Friendly's Quick Loans, Inc., offers you "three for four or I knock on your
# door." This means you get $3 today and repay $4 when you get your paycheck in
# one week (or else). What's the effective annual return Friendly's earns on
# this lending business? If you were brave enough to ask, what APR would
# Friendly's say you were paying?

(r <- 4/3 - 1)
(EAR <- (1 + r)^52 - 1)
(APR <- r * 52)
