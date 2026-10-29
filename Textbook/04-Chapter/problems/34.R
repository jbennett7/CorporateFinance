# 34. Growing Annuity
# Your job pays you only once a year for all the work you did over the previous
# 12 months. Today, December 31, you just received your salary of $65,000, and
# you plan to spend all of it. However, you want to start saving for retirement
# beginning next year. You have decided that one year from today you will begin
# depositing 5 percent of your annual salary in an account that will earn 10
# percent per year. Your salary will increase at 4 percent per year throughout
# your career. How much money will you have on the date of your retirement 40
# years from today?
library(scales)

# growing annuity factor
pvia <- function(rate, term, growth=0) {
    if (abs(growth - rate) < 1e-10) {
        term / (1 + rate)
    } else {
        (1 - ((1 + growth) / (1 + rate))^(term)) / (rate - growth)
    }
}

# first deposit at t=1: 5% of next year's salary (65,000 x 1.04)
label_dollar(accuracy=0.01)(savings <- 65e3 * 1.04 * .05)

# The present value of savings growing 4 percent a year
label_dollar(accuracy=0.01)(pv_savings <- savings * pvia(.1, 40, .04))

# The future value of the account after 40 years
label_dollar(accuracy=0.01)(fv_account <- pv_savings * (1 + .1)^40)
