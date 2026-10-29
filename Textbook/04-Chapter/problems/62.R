# 62. Calculating EAR with Points
# You are looking at a one-year loan of $10,000. The interest rate is quoted as
# 8 percent plus three points. A point on a loan is simply 1 percent (one
# percentage point) of the loan amount. Quotes similar to this one are very
# common with home mortgages. The interest rate quotation in this example
# requires the borrower to pay three points to the lender up front and repay
# the loan later with 8 percent interest. What rate would you actually be
# paying here? What is the EAR for a one-year loan with a quoted interest rate
# of 11 percent plus two points? Is your answer affected by the loan amount?

loan_face_value <- 10e3
base_interest <- .08
added_points <- .03
upfront_payment <- loan_face_value * added_points

