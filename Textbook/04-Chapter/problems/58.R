# 58. Calculating Annuity Values
# After deciding to buy a new car, you can either lease the car or purchase it
# with a three-year loan. The car you wish to buy costs $31,000. The dealer has
# a special leasing arrangement where you pay $1,500 today and $405 per month
# for the next three years. If you purchase the car, you will pay it off in
# monthly payments over the next three years at an APR of 6 percent. You
# believe that you will be able to sell the car for $20,000 in three years.
# Should you buy or lease the car? What break-even resale price in three years
# would make you indifferent between buying and leasing?
library(scales)

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

car_cost <- 31e3
lease_initial <- 1500
lease_monthly <- 405
lease_term <- 3*12
resale_price <- 20e3
apr <- .06

# Leasing the car
pv_lease <- lease_initial + lease_monthly * pvia(apr/12, lease_term)

# Buying the car
pv_resale <- resale_price * (1 + apr/12)^(-lease_term)
# The PV of the payments is the car cost, since we are buying it is an up front
# cost (financed, but paid off monthly)
pv_buy <- car_cost - pv_resale

cat("Leasing", label_dollar(accuracy=0.01)(pv_lease),
    "\nBuying", label_dollar(accuracy=0.01)(pv_buy), "\n\n")

if (pv_lease < pv_buy) {
    cat("Leasing is cheaper\n")
} else if (abs(pv_lease - pv_buy) < 5e-3) {
    cat("Leasing and buying are the same\n")
} else {
    cat("Buying is cheaper\n")
}

break_even <- (car_cost - pv_lease) * (1 + apr/12)^(lease_term)
cat("\nThe break-even resale price of",
    label_dollar(accuracy=0.01)(break_even), "\n")

# Check
car_cost - break_even


