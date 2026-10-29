# 23. Calculating Annuities
# You are planning to save for retirement over the next 30 years. To do this,
# you will invest $800 a month in a stock account and $350 a month in a bond
# account. The return of the stock account is expected to be 11 percent, and
# the bond account will pay 6 percent. When you retire, you will combine your
# money into an account with an 8 percent return. How much can you withdraw
# each month from your account assuming a 25-year withdrawal period?

savings <- 800 * ((1 + .11/12)^(12*30) - 1) / (0.11/12) +
           350 * ((1 + .06/12)^(12*30) - 1) / (0.06/12)

format(savings * (0.08/12) / (1 - (1 + .08/12)^(-12*25)), big.mark=',')
