# 35. Present Value and Interest Rates
# What is the relationship between the value of an annuity and the level of
# interest rates? Suppose you just bought a 15-year annuity of $6,800 per year
# at the current interest rate of 10 percent per year. What happens to the
# value of your investment if interest rates suddenly drop to 5 percent? What
# if interest rates suddenly rise to 15 percent?

# The relationship between interest rates and the value of a fixed income
# annuity is inverse. If interest rates decrease, then the value of the fixed
# income annuity increases If interest rates increase, then the value of the
# fixed income annuity decreases.
library(scales)
library(tibble)
library(ggplot2)

# annuity factor
pvia <- function(interest_rate, term)
    (1 - (1 + interest_rate)^-term) / interest_rate

# Value of a 15-year $6,800 annuity at 10 percent.
label_dollar(accuracy=0.01)(annuity_10pct <- 6800 * pvia(.10, 15))

# If interest rates decrease then the value of the annuity increases
label_dollar(accuracy=0.01)(annuity_5pct <- 6800 * pvia(.05, 15))
label_percent(accuracy=0.01)(percent_increase <- (annuity_5pct - annuity_10pct) / annuity_10pct)
annuity_5pct > annuity_10pct

# If interest rates increase then the value of the annuity decreases
label_dollar(accuracy=0.01)(annuity_15pct <- 6800 * pvia(.15, 15))
label_percent(accuracy=0.01)(percent_decrease <- (annuity_15pct - annuity_10pct) / annuity_10pct)
annuity_15pct < annuity_10pct

plot_points <- tibble(
    interest_rate = c(.05, .10, .15),
    annuity_value = c(annuity_5pct, annuity_10pct, annuity_15pct)
)

plot_curve <- tibble(
    interest_rate = seq(.01, .20, by = .001),
    annuity_value = 6800 * pvia(interest_rate, 15)
)

g35 <- ggplot(plot_curve, aes(x = interest_rate, y = annuity_value)) +
    geom_line(color = "blue") +
    geom_point(data = plot_points) +
    annotate("segment", x = .05, xend = .15,
             y = annuity_5pct, yend = annuity_15pct) +
    scale_x_continuous(labels = label_percent()) +
    scale_y_continuous(labels = label_dollar()) +
    labs(x = "Interest Rates", y = "Annuity Value",
         title = "Interest Rates versus Annuity Value")

ggsave("04-Chapter/images/35.png", plot = g35, width = 5, height = 5, dpi = 300)
