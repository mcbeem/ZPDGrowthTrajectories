#' Function for defining the school curriculum function as a piecewise linear equation
#' called by school.growth()
#' @param x achievement value
#' @param slope1 the slope of the lower leg of the school curriculum
#' @param slope2 the slope of the upper leg of the school curriculum
#' @param start the curriculum starting location
#' @param end the curriculum ending location
#' @examples
#' x <- seq(0, 1, .001)
#' plot(x, school(x, slope1=10, slope2=30, start=.2, end=.4), "l")
#' # mark start and end points
#  abline(v=c(.2, .4), lty="dotted", col="red")
#'
school <- function(x, slope1, slope2, start, end) {
  slope2 <- -1*slope2
  # define piecewise function
  ifelse(x > start & x < end, 1,
         ifelse(x > start - (1/slope1) & x <  start, slope1*x - slope1*start + 1,
                ifelse(x > end & x < end + (1/(-1*slope2)) , slope2*x - slope2*end + 1,
                       0)))
}
