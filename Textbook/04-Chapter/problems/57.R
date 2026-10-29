# 57. Calculating Annuity Values
# Bilbo Baggins wants to save money to meet three objectives. First, he would
# like to be able to retire 30 years from now with a retirement income of
# $23,000 per month for 20 years, with the first payment received 30 years and
# 1 month from now. Second, he would like to purchase a cabin in Rivendell in
# 10 years estimated cost of $320,000. Third, after he passes on at the end of
# the 20 years of withdrawals, he would like to leave an inheritance of
# $1,000,000 to his nephew Frodo. He can afford to save $2,100 per month for
# the next 10 years. If he can earn an 11 percent EAR before he retires and an
# 8 percent EAR after he retires, how much will he have to save each month in
# Years 11 through 30?
library(scales)
pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate
fvia <- function(rate, term) ((1 + rate)^term - 1) / rate

# Before retirement
ear_1 <- .11
# Monthly rate before retirement
r_1 <- (1 + ear_1)^(1/12) - 1
# Cabin purchase date
purchase_date <- 10*12
# Monthly savings for the first 10 years
monthly_savings_10y <- 2100
# Future value of the 10 year savings
fv_savings_10y <- monthly_savings_10y * fvia(r_1, purchase_date)
# Purchase price of the cabin bought at year 10
purchase_price <- 320e3
# After purchasing the cabin the left over of the 10 years of savings
leftover <- fv_savings_10y - purchase_price
# Retirement start date
retirement_date <- 30*12
# The number of periods between the purchase date and the retirement date
after_purchase_period <- retirement_date - purchase_date
# Value of the leftover in the account until retirement
leftover_value <- leftover * (1 + r_1)^(after_purchase_period)

# After retirement
ear_2 <- .08
# Monthly rate after retirement
r_2 <- (1 + ear_2)^(1/12) - 1
# Inheritance amount at the end of the annuity
inheritance_amount <- 1e6
# Inheritance date and the end of the annuity
inheritance_date <- 50*12
# The number of periods between the retirement date and the inheritance date
retirement_periods <- inheritance_date - retirement_date
# Annuity monthly payments beginning at start of retirement
annuity_monthly_payment <- 23e3
# Value of the annuity at the start of retirement
retirement_annuity_value <- annuity_monthly_payment *
    pvia(r_2, retirement_periods)
# The inheritance valued at the retirement date
inheritance_at_retirement <- inheritance_amount /
    (1 + r_2)^(retirement_periods)
# The amount needed for retirement at retirement date minus leftover
additional_savings_after_y10 <- inheritance_at_retirement +
    retirement_annuity_value - leftover_value
# The amount needed between years 11 to 30
savings_after_y10 <- additional_savings_after_y10 /
    fvia(r_1, after_purchase_period)

cat("The amount needed to save from years 11 to 30 is",
    label_dollar(accuracy=0.01)(savings_after_y10),
    "\n")

# Check
balance <- 0
for (month in 1:600){
    # Interest earned this month
    rate <- if (month <= retirement_date) r_1 else r_2
    balance <- balance * (1 + rate)

    # Deposits
    if (month <= purchase_date)
        balance <- balance + monthly_savings_10y
    if (month > purchase_date & month <= retirement_date)
        balance <- balance + savings_after_y10

    # Withdrawals
    if (month == purchase_date)
        balance <- balance - purchase_price
    if (month > retirement_date)
        balance <- balance - annuity_monthly_payment
}
cat("TEST: ensure that balance is within half a cent of inheritance_amount",
    abs(balance - inheritance_amount) < 5e-3, "\n")



