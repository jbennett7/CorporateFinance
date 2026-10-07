# 18. Interest Rates
# Well-known financial writer Andrew Tobias argues that he can earn 177 percent
# per year buying wine by the case. Specifically, he assumes that he will
# consume one $10 bottle of fine Bordeauz per week for the next 12 weeks. He
# can either pay $10 per week or buy a case of 12 bottles today. If he buys the
# case, he receives a 10 percent discount and, by doing so, earns the 177
# percent. Assume he buys the wine and comsumes the first bottle today. Do you
# agree with his analysis? Do you see a problem with his numbers?

per_week <- function(r) 10 + sum(10 * (1 + r)^(-(1:11)))

upfront <- 10 * 12 * .9

irr <- uniroot(function(r) per_week(r) - upfront, c(0, 2))$root

(1 + irr)^52 - 1

# The APR is much lower. States as a simple annual rate, its 52 x 1.98% = 103%.
#   Quoting 177% uses the compouned figure, which makes it sounds more
#   impressive.
# It isn't scalable or repeatable. The return applies to $98 for 11 weeks. He
#   can't put $10,000 into it, and he can't reinvest the "earnings" at 177%,
#   which is what an EAR implicitly assumes.
# There is a difference between an investment and a savings on a planned purchase.
