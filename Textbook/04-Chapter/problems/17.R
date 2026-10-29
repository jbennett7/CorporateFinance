# 17. Calculating EAR
# First National Bank charges 11.2 percent compounded monthly on its business
# loans. First United Bank charges 11.4 percent compounded semiannually. As a
# potential borrower, to which bank would you go for a new loan?

national <- .112
united   <- .114

(1 + national/12)^(12) - 1
(1 + united/2)^2 - 1
