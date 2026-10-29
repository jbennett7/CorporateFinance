# 33. Growing Annuity
# Southern California Publishing Company is trying to decide whether to revise
# its popular textbook, Financial Psychoanalysis Made Simple. The company has
# estimated that the revision will cost $75,000. Cash flows from increased
# sales will be $21,000 the first year. These cash flows will increase 4
# percent per year. The book will go out of print five years from now. Assume
# that the initial cost is paid now and revenues are received at the end of
# each year. If the company requires a return of 10 percent for such an
# investment, should it undertake the revision?
library(scales)

# growing annuity factor
pvia <- function(return_rate, term, growth_rate=0) {
    if (abs(growth_rate - return_rate) < 1e-10) {
        term / (1 + return_rate)
    } else {
        (1 - ((1 + growth_rate) / (1 + return_rate))^(term)) /
            (return_rate - growth_rate)
    }
}

investment <- 75e3
cash_flow_y1 <- 21e3
growth_rate <- .04
term <- 5
return_rate <- .10

# Calculate the net present value
label_dollar(accuracy=0.01)(npv <- cash_flow_y1 *
                            pvia(return_rate, term, growth_rate) - investment)

# The net present value is positive, SCPC should go ahead with the revision.

# The company would be indifferent if the npv equals zero.
npv_at <- function(rate) cash_flow_y1 * pvia(rate, term, growth_rate) - investment
label_percent(accuracy=0.01)(irr <- uniroot(npv_at,
                    interval = c(growth_rate + 1e-6, 1), tol=1e-8)$root)
