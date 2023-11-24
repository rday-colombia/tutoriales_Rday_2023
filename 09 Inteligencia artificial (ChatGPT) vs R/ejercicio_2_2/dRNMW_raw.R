dRNMW <- function(x, mu, sigma, nu, log = FALSE) {
  if (any(x < 0)) {
    stop(paste("x must be positive", "\n", ""))
  }
  if (mu <= 0 || sigma <= 0 || nu <= 0) {
    stop(paste("mu, sigma, and nu must be positive", "\n", ""))
  }
  e_term1 <- exp(nu * x)
  e_term2 <- exp(-mu * sqrt(x) - sigma * sqrt(x) * exp(nu * x))
  f <- (1 / (2 * sqrt(x))) * (mu + sigma * (1 + 2 * nu * x) * e_term1) * e_term2
  if (log == FALSE) {
    dens <- f
  } else {
    dens <- log(f)
  }
  return(dens)
}

pRNMW <- function(
    q, mu, sigma, nu, lower.tail = TRUE, log.p = FALSE) {
  if (any(q < 0)) {
    stop(paste("q must be positive", "\n", ""))
  }
  if (mu <= 0 || sigma <= 0 || nu <= 0) {
    stop(paste("mu, sigma, and nu must be positive", "\n", ""))
  }
  cdf <- 1 - exp(-mu * sqrt(q) - sigma * sqrt(q) * exp(nu * q))
  if (lower.tail == TRUE) {
    cdf <- cdf
  } else {
    cdf <- 1 - cdf
  }
  if (log.p == FALSE) {
    cdf <- cdf
  } else {
    cdf <- log(cdf)
  }
  return(cdf)
}