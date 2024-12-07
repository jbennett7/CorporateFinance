# Short-Term Solvency, or Liquidity Ratios

current.ratio <- fucntion(assets, liabilities) { assets / liabilities }

quick.ratio <- function(assets, inventory, liabilities) {
  return((assets - inventory) / liabilities)
}

cash.ratio <- function(cash, liabilities) {
  cash / liabilities
}


# Long-Term solvency, or financial leverage, ratios

total.debt.ratio <- function(assets, equity) {
  (assets - equity) / assets
}

debt.equity.ratio <- function(debt, equity) {
  debt / equity
}

equity.multiplier <- function(assets, equity) {
  assets / equity
}

times.interest.earned.ratio <- function(ebit, interest) {
  ebit / interest
}

cash.coverage.ratio <- function(ebitda, interest) {
  ebitda / interest
}

# Asset utilization, or Turnover ratios

inventory.turnover <- function(cost.of.goods.sold, inventory) {
  cost.of.goods.sold / inventory
}

days.sales.in.inventory <- function(cost.of.goods.sold, inventory) {
  365 / inventory.turnover(cost.of.goods.sold, inventory)
}

receivables.turnover <- function(sales, accounts.receivable) {
  sales / accounts.receivable
}

days.sales.in.receivables <- function(sales, accounts.receivable) {
  365 / receivables.turnover(sales, accounts.receivable)
}

total.asset.turnover <- function(sales, assets) {
  return(sales / assets)
}

capital.intensity <- function(assets, sales) {
  assets / sales
}

# Profitability ratios

profit.margin <- function(net.income, sales) {
  net.income / sales
}

return.on.assets <- function(net.income, assets) {
  net.income / assets
}

return.on.equity <- funciton(net.income, equity) {
  net.income / equity
}

# Market value ratios

price.earnings.ratio <- function(price.per.share, earnings.per.share) {
  price.per.share / earnings.per.share
}

market.to.book.ratio <- function(market.value.per.share, book.value.per.share) {
  market.value.per.share / book.value.per.share
}

ev.multiple <- function(enterprise.value, ebitda) {
  enterprise.value / ebitda
}