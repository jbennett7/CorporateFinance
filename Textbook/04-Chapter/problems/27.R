# 27. Perpetuities
# A prestigious investment bank designed a new security that pays a quarterly
# dividend of $4.50 in perpetuity. The first dividend occurs one quarter from
# today. What is the price of the security if the stated annual interest rate is
# 6.5 percent, compounded quarterly?
library(scales)

# The perpituity formula is cash_flow / (interest_rate/compound_rate)
label_dollar()(4.5/(.065/4))
