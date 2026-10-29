# 61. Calculating Annuity Values
# You are serving on a jury. A plaintiff is suing the city for injuries
# sustained after a freak street sweeper accident. In the trial, doctors
# testified that it will be five years before the plaintiff is able to return
# to work. The jury already decided in favor of the plaintiff. You are the
# foreperson of the jury and propose that the jury give the plaintiff an award
# to cover the following: (1) The present value of two years' back pay. The
# plaintiff's annual salary for the last two years would have been $37,000 and
# $39,000, respectively. (2) The present value of five years' future salary.
# You assume the salary will be $43,000 per year. (3) $150,000 for pain and
# suffering. (4) $25,000 for court costs. Assume that the salary payments are
# equal amounts paid at the end of each month. If the interest rate you choose
# is a 9 percent EAR, what is the size of the settlement? If you were the
# plaintiff, would you like to see a higher or lower interest rate?
library(scales)

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate
fvia <- function(rate, term) ((1 + rate)^term - 1) / rate

ear <- c(.09, .1, .08)
monthly_rate <- (1 + ear)^(1/12) - 1

# backpay two years ago, valued today
backpay_2yrs_ago <- 37e3
backpay_2yrs_ago_today <- (backpay_2yrs_ago/12) * fvia(monthly_rate, 12) * (1 + ear)^1
# backpay one year ago, valued today
backpay_1yr_ago <- 39e3
backpay_1yr_ago_today <- (backpay_1yr_ago/12) * fvia(monthly_rate, 12)

# Total back pay is the sum of the two years
backpay_today <- backpay_2yrs_ago_today + backpay_1yr_ago_today

# Five year pay valued today
futurepay <- 43e3
futurepay_today <- (futurepay/12) * pvia(monthly_rate, 5*12)

# Pain and suffering, and court costs are valued today
pain_and_suffering <- 150e3
court_costs <- 25e3

settlement_size <- pain_and_suffering + court_costs + futurepay_today + backpay_today
cat("Value of the settlement",
    label_dollar(accuracy=0.01)(settlement_size[1]), "\n")

cat("If the rate was 1 percentage point higher",
    label_dollar(accuracy=0.01)(settlement_size[2]), "\n")

cat("If the rate was 1 percentage point lower",
    label_dollar(accuracy=0.01)(settlement_size[3]), "\n")

cat("The plaintiff would want a lower rate.\n")
cat("A lower rate hurts the back pay, which is compounded forward, but helps
    the future pay, which is discounted back. Future pay is bigger and spans
    more years, so its effect wins.\n\n")

# Check, only checking the first monthly rate
monthly_rate_9pct <- monthly_rate[1]
backpay_2yrs_ago_today_check <- (backpay_2yrs_ago/12) * sum((1 + monthly_rate_9pct)^(12:23))
b2c <- abs(backpay_2yrs_ago_today_check - backpay_2yrs_ago_today[1]) < 5e-3
backpay_1yr_ago_today_check <- (backpay_1yr_ago/12) * sum((1 + monthly_rate_9pct)^(0:11))
b1c <- abs(backpay_1yr_ago_today_check - backpay_1yr_ago_today[1]) < 5e-3
futurepay_today_check <- sum((futurepay/12) / (1 + monthly_rate_9pct)^(1:(5*12)))
fc <- abs(futurepay_today_check - futurepay_today[1]) < 5e-3
cat("TEST: testing only the backpay and futurepay results within half a cent",
    all(c(b2c, b1c, fc)), "\n")
