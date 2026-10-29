# 60. Discount Interest Loans
# This question illustrates what is known as discount interest. Imagine you are
# discussing a loan with a somewhat unscrupulous lender. You want to borrow
# $20,000 for one year. The interest rate is 15 percent. You and the lender
# agree that the interest on the loan will be .15 x $20,000 = $3,000. So, the
# lender deducts this interest amount from the loan up front and gives you
# $17,000. In this case, we say that the discount is $3,000. What's wrong here?
library(scales)

# The issue here is, because you have a smaller portion of the money, while
# paying interest on the full amount, your effective interest rate is higher
# than the nominal interest rate.

cat("The face value of the loan",
    label_dollar()(loan_face_value <- 20e3), "\n")

cat("The discount/interest rate",
    label_percent()(interest_rate <- .15), "\n")

cat("Interest paid",
    label_dollar()(interest_paid <- loan_face_value * interest_rate), "\n")

cat("Amount disbursed to you",
    label_dollar()(disbursed_amount <- loan_face_value - interest_paid),
    "\n\n")

# The effective rate will simplify to interest_rate / (1 - interest_rate). The
# reason is interest_paid / disbursed_amount =>
# (loan_face_value * interest_rate) / (loan_face_value * (1 - interest_rate)).
# Because the face value of the loan cancels out from the numerator and
# denominator, the effective rate does not depend on how much you borrow.
# As the interest rate increases, the effective rate increases as well. This is
# because, as the interest rate increases, the numerator increases and the
# denominator decrease. Also the gap between them increases faster than the
# rate increase.
cat("The effective rate for paying", label_dollar()(interest_paid),
    "of interest\n    on the", label_dollar()(disbursed_amount),
    "you actually received equals",
    label_percent(accuracy=0.01)(effective_rate <- interest_paid /
                                 disbursed_amount),
    "\n\n")

# Check
loan_face_value_check <- disbursed_amount * (1 + effective_rate)
cat("TEST: The definition of an effective rate is: what you receive today,
    grown at that rate for one year, equals what you repay.",
     abs(loan_face_value_check - loan_face_value) < 5e-3, "\n")

