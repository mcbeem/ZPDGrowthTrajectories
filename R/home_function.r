#' Function to define the home curriculum function
#' Using exponential decay
#' called by home.growth() function
#' Home curriculum intensity given by (-rate*x)
#'
#' @param x level of achievement
#' @param rate exponential decay rate
#' @examples
#' x <- seq(.0001, 1, .001)
#' plot(x, y=home(x, rate=5), "l")

home <- function(x, rate) {
  ifelse(x<=0, 0, exp(-rate*x))
}

