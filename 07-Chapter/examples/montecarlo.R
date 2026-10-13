set.seed(102)

market_size <- data.frame(
    size        = c(10, 11, 12),
    probability = c(.20, .60, .20)
)

market_share <- data.frame(
    share       = c(.01, .02, .03, .04, .05, .08),
    probability = c(.10, .20, .30, .25, .10, .05)
)

price_var <- data.frame(
    variable    = c(-3, 3),
    probability = c(.5, .5)
)

price <- function(units) {
    190 + units + sample(price_var$variable, size = 1, prob = price_var$probability)
}

growth <- data.frame(
    growth_rate = c(.01, .03, .05),
    probability = c(.20, .60, .20)
)

n <- 5
model <- function(m) {
    mc_size  <- sample(market_size$size, size = 1, prob = market_size$probability)
    mc_share <- sample(market_share$share, size = 1, prob = market_share$probability)
    mc_price <- price(mc_size)
    mc_revenue <- mc_size * mc_share * mc_price
    mc_revenue
}

cf_components <- c('revenue', 'costs', 'investment')
sapply(1:n, model)
