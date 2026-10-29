# 14. Calculating Perpetuity Values
# The Perpetual Life Insurance Co. is trying to sell you an investment  policy
# that will pay you and your heirs $15,000 per year forever. If the required
# return on this investment is 5.2 percent, how much will you pay for the
# policy? Suppose the Perpetual Life Insurance Co. told you the policy costs
# $320,000. At what interest rate would this be a fair deal?

format(15000/.052, big.mark=',')

format(round(uniroot(function(r) 15000/r - 320000, c(0, 1))$root*100, 2))
