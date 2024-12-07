# Time value of money

compound <- function(rate, nper=1, compound=1) {
  if(compound == 0){
    return(exp(rate * nper))
  } else {
    return((1 + rate/compound)^(compound * nper))
  }
}

present.value <- function(fv, rate, nper=1, cost=0, period=1) {
  fv / compound(rate, nper, period) - cost
}

future.value <- function(pv, rate, nper=1, period=1) {
  pv * compound(rate, nper, period)
}

compound.periods <- function(pv, fv, rate) {
  log(fv/pv) / log(1+rate)
}

pv.perpetuity <- function(coupon, interest, growth=0) {
  if(growth > interest)
    stop("growth rate cannot be larger than interest rate.")
  coupon / (interest - growth)
}

pvia <- function(rate, term, growth=0) {
  (1 - ((1 + growth) / (1 + rate))^(term)) / (rate - growth)
}

pv.annuity <- function(coupon, rate, term, growth=0, delay=1, payout=1) {
  rate <- (1 + rate)^payout - 1
  if(delay < 0)
    coupon + coupon * pvia(rate, term-1, growth)
  else
    present.value(coupon * pvia(rate, term / payout, growth), rate, delay-1)
}

fv.annuity <- function(coupon, rate, term) {
  coupon * ((1 + rate)^(term) - 1) / rate
}

