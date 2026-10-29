# 49. Calculating Annuities Due
# Suppose you are going to receive $20,000 per year for five years. The
# appropriate interest rate is 7 percent.
#  a. What is the present value of the payments if they are in the form of an
#     ordinary annuity? What is the present value if the payments are an
#     annuity due?
#  b. Suppose you plan to invest the payments for five years. What is the
#     future value if the payments are an ordinary annuity? What if the
#     payments are an annuity due?
#  c. Which has the highest present value, the ordinary annuity or annuity due?
#     Which has the highest future value? Will this always be true?
library(scales)

annual_payments <- 20e3
interest_rate <- .07
term <- 5

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate
fvia <- function(rate, term) ((1 + rate)^term - 1) / rate

cat("The present value for the ordinary annuity payments",
    label_dollar(accuracy=0.01)
        (ordinary_annuity_a <- annual_payments * pvia(interest_rate, term)),
    "\n")

cat("The present value for the annuity due payments",
    label_dollar(accuracy=0.01)
        (annuity_due_a <- annual_payments * (1 + pvia(interest_rate, term-1))),
    "\n")

cat("The future value for the ordinary annuity investments",
    label_dollar(accuracy=0.01)
        (ordinary_annuity_b <- annual_payments * fvia(interest_rate, term)),
    "\n")

cat("The future value for the annuity due investments",
    label_dollar(accuracy=0.01)
        (annuity_due_b <- annual_payments * (fvia(interest_rate, term+1) - 1)),
    "\n\n")

cat("For both the future value investments and the present value payments,\n",
    "the annuity due is higher. This is because the payment or investment\n",
    "occurs at t=0 instead of t=1 which affects compounding. If its a\n",
    "present value payment, then it is received immediately and no compounding\n",
    "affects the first value. If its a future value investment, then the\n",
    "first investment immediately compounds, instead of having to wait a\n",
    "period.\n",
    "     This occurs as long as the interest rate is greater than 0%. If the\n",
    "interest rate is 0%, then no compounding occurs and the value is just\n",
    "the sum of the payments or investments. If the interest rate is negative\n",
    "then the compounding works in the other direction where the value of the\n",
    "gets larger further out and the value of the investments diminish further\n",
    "out. This has the reverse effect on the ordinary annuity and annuity due.\n\n")

# Check
ordinary_annuity_a_check <- sum(annual_payments * (1 + interest_rate)^-(1:term))
annuity_due_a_check      <- sum(annual_payments * (1 + interest_rate)^-(0:(term-1)))
ordinary_annuity_b_check <- sum(annual_payments * (1 + interest_rate)^(0:(term-1)))
annuity_due_b_check      <- sum(annual_payments * (1 + interest_rate)^(1:term))

cat("TEST: all checks are within half a cent of values",
    tests <- all(abs(c(ordinary_annuity_a_check - ordinary_annuity_a,
              annuity_due_a_check - annuity_due_a,
              ordinary_annuity_b_check - ordinary_annuity_b,
              annuity_due_b_check - annuity_due_b)) < 5e-3), "\n")
