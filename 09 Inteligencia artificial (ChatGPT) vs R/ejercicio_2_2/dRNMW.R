#' Probability density function of the New Modified Weibull distribution.
#'
#' This function calculates the probability density function (PDF) of the New Modified Weibull distribution for given parameters.
#'
#' @param x A vector of non-negative values at which to calculate the PDF.
#' @param mu A positive parameter representing the location parameter.
#' @param sigma A positive parameter representing the scale parameter.
#' @param nu A positive parameter representing the shape parameter.
#' @param log Logical indicating whether to return the logarithm of the PDF (default is FALSE).
#'
#' @return A vector of PDF values corresponding to the input values of x.
#'
#' @examples
#' dRNMW(0.5, 2, 1, 0.5)
#'
#' @export
dRNMW <- function(x, mu, sigma, nu, log = FALSE) {
  # Function code here...
}

#' Cumulative distribution function of the New Modified Weibull distribution.
#'
#' This function calculates the cumulative distribution function (CDF) of the New Modified Weibull distribution for given parameters.
#'
#' @param q A vector of non-negative values at which to calculate the CDF.
#' @param mu A positive parameter representing the location parameter.
#' @param sigma A positive parameter representing the scale parameter.
#' @param nu A positive parameter representing the shape parameter.
#' @param lower.tail Logical indicating whether to calculate the lower tail CDF (default is TRUE).
#' @param log.p Logical indicating whether to return the logarithm of the CDF (default is FALSE).
#'
#' @return A vector of CDF values corresponding to the input values of q.
#'
#' @examples
#' pRNMW(0.5, 2, 1, 0.5)
#'
#' @export
pRNMW <- function(
    q, mu, sigma, nu, lower.tail = TRUE, log.p = FALSE) {
  # Function code here...
}

#' Quantile function of the New Modified Weibull distribution.
#'
#' This function calculates the quantile function (inverse CDF) of the New Modified Weibull distribution for given parameters.
#'
#' @param p A vector of probabilities between 0 and 1 at which to calculate the quantiles.
#' @param mu A positive parameter representing the location parameter.
#' @param sigma A positive parameter representing the scale parameter.
#' @param nu A positive parameter representing the shape parameter.
#' @param lower.tail Logical indicating whether to calculate the lower tail quantiles (default is TRUE).
#' @param log.p Logical indicating whether the input probabilities are given in logarithmic form (default is FALSE).
#'
#' @return A vector of quantile values corresponding to the input probabilities.