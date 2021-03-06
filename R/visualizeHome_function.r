#' Function for visualizing the home learning environment.
#'
#' \code{visualizeHome()} is a function for visualizing the home learning environment.
#'
#' \code{visualizeHome()} plots the home curriculum. It is useful for exploring
#'  the consequences of the \code{rate} argument to \code{ZPDGrowthTrajectories()}. The function returns a
#'  \code{ggplot} object that can be modified with typical \code{ggplot2} arguments.
#'
#' @param home.learning.decay.rate Scalar, the exponential decay parameter for the home learning function.
#'  Larger values indicate a more rapid drop-off. Must be greater than 1. The functions \code{visualizHome()}
#'  and \code{visualizeContext()} can be used to visualize, understand, and select appropriate values.
#'
#' @param max The maximum value to plot on the x-axis; defaults to 1
#'
#' @return An object of class \code{ggplot2}
#'
#' @family visualizations
#'
#' @seealso \code{\link{ZPDGrowthTrajectories}} for simulating growth trajectories.
#'
#' @importFrom checkmate qtest
#'
#' @examples
#' visualizeHome(home.learning.decay.rate=5)
#'
#' @export

visualizeHome <- function(home.learning.decay.rate, max=1) {

  # check arguments

  # check that home.learning.decay.rate is numeric, length 1, greater than one, not NA
  if(checkmate::qtest(home.learning.decay.rate, "N1(1,)")==FALSE) {stop("home.learning.decay.rate must be a scalar greater than 1")}

  # check that max is numeric, length 1, greater than 0, not NA
  if(checkmate::qtest(max, "N1(0,)")==FALSE) {stop("max must be a scalar greater than 0")}

  rate <- home.learning.decay.rate

  x <- seq(0, max, length.out=10000)
  y <- home(x=x, rate=rate)

  # normalize intensity (y)
  y <- y / max(y)

  data <- data.frame(cbind(x,y))

  # calculate area to shade
  shade <- rbind(c(0,0), subset(data, (x>=0 & x<= max)), c(max, 0))

  p <- ggplot2::ggplot(data=data, ggplot2::aes(x=x, y=y))+ggplot2::geom_line(alpha=.5)+
     ggplot2::geom_polygon(data=shade, ggplot2::aes(x,y), fill="blue", alpha=.1)+
     ggplot2::theme_classic()+ggplot2::ylab("achievement")+ggplot2::xlab("intensity")

  return(p)

}
