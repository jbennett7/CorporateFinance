d.1 <- function(S, E, R, s, t) (log(S/E) + (R + s^2/2)*t)/sqrt(s^2*t)
d.2 <- function(S, E, R, s, t) d.1(S, E, R, s, t) - sqrt(s^2*t)
C.blackscholes <- function(S, E, R, s, t) {
    S * pnorm(d.1(S, E, R, s, t)) - E * exp(-R*t) * pnorm(d.2(S, E, R, s, t))
}

