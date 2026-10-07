# 52. Annuities
# You are saving for the college education of your two children. They are two
# years apart in age; one will begin college 15 years from today and the other
# will begin 17 years from today. You estimate your children's college expenses
# to be $45,000 per year per child, payable at the beginning of each school
# year. The annual interest rate is 7.5 percent. How much money must you
# deposit in an account each year to fund your children's education? Your
# deposits begin one year from today. You will make your last deposit when your
# oldest child enters college. Assume for years of college.

problem_52 <- function() {
    # Four years of college
    PV4yr <- 45000 * (1 + (1 - (1 + .075)^(-3)) / .075)
    
    # Child 1 starts in year 15
    PVchild1 <- PV4yr / (1 + .075)^15
    
    # Child 2 starts in year 17
    PVchild2 <- PV4yr / (1 + .075)^17
    
    # Total present value
    PVtotal <- PVchild1 + PVchild2
    
    Deposit <- PVtotal / ((1 - (1 + .075)^-15)/.075)
}
