# 13. Calculating Annuity Present Value
# An investment offers $4,900 per year for 15 years, with the first payment
# occurring one year from now. If the required return is 8 percent, what is the
# value of the investment? What would the value be if the payments occurred for
# 40 years? For 75 years? Forever?

years <- c(15, 75)

# 15 and 75 years
format(lapply(years, function(y) sum(4900 * (1 + .08)^(-(1:y)))), big.mark=',')

# Forever
format(4900/.08, big.mark=',')

