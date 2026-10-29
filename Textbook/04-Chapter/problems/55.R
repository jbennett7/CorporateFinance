# 55. Calculating Growing Annuities
# You have 30 years left until retirement and want to retire with $2 million.
# Your salary is paid annually, and you will receive $70,000 at the end of the
# current year. Your salary will increase at 3 percent per year, and you can
# earn a return of 9 percent on the money you invest. If you save a constant
# percentage of your salary, what percentage of your salary must you save each
# year?
library(scales)

pvia <- function(r, g, t) (1 - ((1 + g) / (1 + r))^t) / (r - g)

f <- function(s) s * 70e3 * pvia(.09, .03, 30) - 2e6 / (1.09)^30
uniroot(f, interval=c(0,1))$root
