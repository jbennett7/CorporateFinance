# 30. Balloon Payments
# Audrey Sanborn has just arranged to purchase a $550,000 vacation home in the
# Bahamas with a 20 percent down payment. The mortgage has a 6.1 percent stated
# annual interest rate, compounded monthly, and calls for equal monthly
# payments over the next 30 years. Her first payment will be due one month from
# now. However, the mortgage has an eight-year balloon payment, meaning that
# the balance of the loan must be paid off at the end of Year 8. There were no
# other transaction costs or finance charges. How much will Audrey's balloon
# payment be in eight years?
library(scales)
library(dplyr, warn.conflicts = FALSE)

# purchase price minus the down payment (20 percent)
balance <- 550e3 * (1 - .20)
# monthly rate from 6.1% APR
rate <- .061 / 12
# 30-year monthly payments
term <- 30 * 12
# 8-year term
term_8yr <- 8 * 12
# annuity factor
pvia <- function(rate, term) (1 - (1 + rate)^(-term)) / rate

# monthly payment
cat("Monthly payment:",
    label_dollar()(monthly_payment <- balance / pvia(rate, term)),
    "\n")

# term left after 8 years
partial_term <- term - term_8yr

# balloon payment after 8 years
cat("Balloon payment after 8 years:",
    label_dollar(accuracy = 0.01)(balloon_payment <- monthly_payment *
                                  pvia(rate, partial_term)),
    "\n\n")

# amortization method - produces an amortization table
amortize_principal <- function(balance, payment, interest_rate, periods) {

    # preallocate one slot per period
    begin     <- numeric(periods)
    interest  <- numeric(periods)
    principal <- numeric(periods)
    end       <- numeric(periods)

    for (i in 1:periods) {
        begin[i]     <- if (i == 1) balance else end[i-1]
        interest[i]  <- begin[i] * interest_rate
        principal[i] <- payment - interest[i]
        end[i]       <- begin[i] - principal[i]
    }

    tibble(Month = 1:periods, Begin = begin, Interest = interest,
           Principal = principal, End = end)
}

view_amortization_table <- function(amortization_table) {
    mutate(amortization_table, across(-Month, label_dollar(accuracy=0.01))) |>
    print()
}

amortization_table <- amortize_principal(balance, monthly_payment,
                                         rate, term_8yr)

# View the table--we only show the first and last six entries
view_amortization_table(bind_rows(head(amortization_table),
                                  tail(amortization_table)))

# Check both methods--ensure the two methods are within a half a cent
amortization_table_end <- tail(amortization_table, n=1)$End
abs(amortization_table_end - balloon_payment) < 5e-3
