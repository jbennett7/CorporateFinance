current.assets <- function(bs) {
  return(bs[1,])
}

fixed.assets <- function(bs) {
  return(bs[2,])
}

total.assets <- function(bs) {
  return(current.assets(bs) + fixed.assets(bs))
}

short.term.liabilities <- function(bs) {
  return(bs[3,])
}

long.term.liabilities <- function(bs) {
  return(bs[4,])
}

