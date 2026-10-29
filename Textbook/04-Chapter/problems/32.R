# 32. Perpetuities
# Barrett Pharmaceuticals is considering a drug project that costs $2.5 million
# today and is expected to generate end-of-year annual cash flows of $227,000,
# forever. At what discount rate would Barrett be indifferent between accepting
# or rejecting the project?
library(scales)

# Internal rate of return: NPV = C/r - cost = 0 => r = C / cost
label_percent(accuracy=0.01)(irr <- 227e3/2.5e6)
