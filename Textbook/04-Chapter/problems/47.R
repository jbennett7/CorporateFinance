# 47. Calculating EAR
# A local finance company quotes a 16 percent interest rate on one-year loans.
# So, if you borrow $26,000, the interest for the year will be $4,160. Because
# you must repay a total of $30,160 in one year, the finance company requires
# you to pay $30,160/12, or $2,513.33, per month over the next 12 months. Is
# this a 16 percent loan? What rate would legally have to be quoted? What is
# the effective annual rate?
library(scales)
library(tibble)

principal <- 26e3
term <- 12
rate <- .16
monthly_payment <- principal * (1 + rate) / term 

amortize_principal <- function(balance, payment, interest_rate, periods){

    begin     <- numeric(periods)
    interest  <- numeric(periods)
    principal <- numeric(periods)
    end       <- numeric(periods)

    for(i in 1:periods){
        begin[i]     <- if (i == 1)  balance else end[i-1]
        interest[i]  <- begin[i] * interest_rate
        principal[i] <- payment - interest[i]
        end[i]       <- begin[i] - principal[i]
    }

    tibble(Begin = begin, Interest = interest, Principal = principal, End = end)
}

amortization_table_r16 <- amortize_principal(principal, monthly_payment, rate/12, term)

cat("The stated rate of",
    label_percent()(rate),
    "is far too low to be accurate. When used in the amortization table",
    "\nthe final principal after the last payment",
    label_dollar(accuracy=0.01)(tail(amortization_table_r16$End, n=1)),
    "is a large negative value.\nThe rate they are using is called",
    "the add-on interest rate.\n\n")

pvia <- function(rate, term) (1 - (1 + rate)^-term) / rate

payment_gap <- function(rate, principal, payment, term) {
    payment * pvia(rate, term) - principal
}

amortization_table_2x_rate <- amortize_principal(principal, monthly_payment,
                                                 rate/12*2, term)

cat("You can estimate the real annual rate by doubling the add-on annual rate.",
    label_percent()(rate), "x 2 =", label_percent()(rate*2),
    "is\nthe estimated rate we can use in the amortization table.",
    "The final principal after the\nlast payment",
    label_dollar(accuracy=0.01)(tail(amortization_table_2x_rate$End, n=1)),
    "is closer. We will use uniroot to calculate the true rate.\n\n")

monthly_rate <- uniroot(payment_gap,
                           principal = principal,
                           payment = monthly_payment,
                           term = term,
                           interval = c(0.001, 1),
                           tol=1e-8)$root

amortization_table_uniroot_rate <- amortize_principal(principal,
                                                      monthly_payment,
                                                      monthly_rate,
                                                      term)

cat("The true annual rate",
    label_percent(accuracy=0.01)(true_apr <- monthly_rate*12),
    "when used in the amortization table gives us the final principal\nafter last payment as",
    label_dollar(accuracy=0.01)
        (check_principal <- tail(amortization_table_uniroot_rate$End, n=1)),
    "which means this is the true rate.\n\n")

cat("So, this is not a", label_percent()(rate),
    "APR loan, and the legal rate would be",
    label_percent(accuracy=0.01)(true_apr),
    "and the calculated EAR is",
    label_percent(accuracy=0.01)(true_ear <- (1 + true_apr/12)^12 - 1),
    "\n\n")

# check
cat("TEST: if the final principal after the last payment is within half a cent",
    abs(check_principal) < 5e-3, "\n")
