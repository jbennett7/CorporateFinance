##
# 
# c(sales, costs, deprecation, interest, dividends)
#
##
sales <- function(is) {
  return(as.numeric(is[1]))
}

costs <- function(is) {
  return(as.numeric(is[2]))
}

ebitda <- function(is) {
  return(sales(is) - costs(is))
}

deprecation <- function(is) {
  return(as.numeric(is[3]))
}

interest <- function(is) {
  return(as.numeric(is[4]))
}

ebit <- function(is) {
  return(ebitda(is) - deprecation(is))
}

taxable.income <- function(is) {
  return(ebit(is) - interest(is))
}

taxes <- function(is, rate) {
  return(taxable.income(is) * rate)
}

net.income <- function(is, rate) {
  return(taxable.income(is) - taxes(is, rate))
}

dividends <- function(is) {
  return(is[5])
}

addition.to.retained.earnings <- function(is, rate) {
  return(net.income(is, rate) - dividends(is))
}

display.income.statement <- function(is, rate) {
  cat("Sales:", sales(is), "\n",
      "Costs:", costs(is), "\n",
      "ebitda:", ebitda(is), "\n",
      "deprecation:", deprecation(is), "\n",
      "interest:", interest(is), "\n",
      "ebit:", ebit(is), "\n",
      "taxable income:", taxable.income(is), "\n",
      "taxes:", taxes(is, rate), "\n",
      "net income:", net.income(is, rate), "\n",
      "Addition to retained earnings:", addition.to.retained.earnings(is, rate), "\n",
      "Dividends:", dividends(is))
}