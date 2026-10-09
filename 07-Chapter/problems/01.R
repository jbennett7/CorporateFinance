# Sensitivity Analysis and Break-Even Point
# We are evaluating a project that costs $644,000, has an eight-year life, and
# has no salvage value. Assume that depreciation is straight-line to zero over
# the life of the project. Sales are projected at 70,000 units per year. Price
# per unit is $37, variable cost per unit is $21, and fixed costs are $725,000
# per year. The tax rate is 35 percent, and we require return on this project.
#
# a. Calculate the accounting break-even point.
# b. Calculate the base-case cash flow and NPV. What is the sensitivity of NPV
#    to changes in the sales figure? Explain what your answer tells you about a
#    500-unit decrease in projected sales.
# c. What is the sensitivity of OCF to changes in the variable cost figure?
#    Explain what your answer tells you about a $1 decrease in estimated
#    variable costs.

# Per year values
depreciation  <- 644e3/8
price         <- 37
variable_cost <- 21
profit <- price - variable_cost
fixed_cost    <- 725e3

# a. break-even
uniroot(function(x) profit * x - fixed_cost - depreciation, c(0,100000))$root

# b.
