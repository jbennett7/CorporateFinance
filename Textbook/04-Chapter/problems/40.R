# 40. Calculating Present Values
# You just won the TVM Lottery. You will receive $1 million today plus another
# 10 annual payments that increase by $275,000 per year. Thus, in one year you
# receive $1.275 million. In two years, you get $1.55 million, and so on. If
# the appropriate interest rate is 9 percent, what is the present value of your
# winnings?
library(scales)

# first cash flow
cat("The cash flow today is:",
    label_dollar()(beginning <- 1e6),
    "\n")

# first payment
cat("The first payment one year from today:",
    label_dollar()(first <- 1.275e6),
    "\n")

# dollar increase
cat("Each year the payments increase:",
    label_dollar()(dollar_increase <- 275e3),
    "\n")

# The interest rate is
cat("Appropriate interest rate:",
    label_percent()(interest_rate <- .09),
    "\n")

# The term of all payments
cat("There are", term <- 10, "payments increasing", term-1, "times\n\n")

# The increase is in dollars not in percent so I cannot use the usual growth
# annuity formula. I must calculate by hand. This calculates the present value
# of a stream of payments starting at t=0 with cash_flow_0 = `y0_payment`, and
# using `increase` as the dollar increase up to and including `term`.
calculate_present_value <- function(y0_payment, increase, interest_rate, term)
    sum(seq(y0_payment, y0_payment + increase * term, by=increase) /
        (1 + interest_rate)^(0:term))

cat("The present value of the winnings is:",
    label_dollar(accuracy=0.01)(pv_winnings <-
        calculate_present_value(beginning, dollar_increase,
                                interest_rate, term)),
    "\n")

alt_pv_winnings <- sum(seq(from=first,
                           by=dollar_increase,
                           length.out=term) /
                       (1 + interest_rate)^(1:term)) + beginning

# Check that `pv_winnings` is within a half cent of `alt_pv_winnings`
cat("pv_winnings conforms to check:",
    abs(alt_pv_winnings - pv_winnings) < 5e-3,
    "\n")
