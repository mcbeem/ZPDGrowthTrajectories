#' Function to define the ZPD using an Epanechnikov kernel
#' called by home.growth() and school.growth() functions
#' Home curriculum intensity given by (-rate*x)
#'
#' @param x level of achievement
#' @param location location of the ZPD
#' @param radius radius of the ZPD
#' @examples
#' x <- seq(-1, 1, length.out=5000)
#' y <- ZPD(x, radius=.1, location=.2)
#' plot(x,y, type="l", xlim=c(0,1))

# function defining the ZPD as an Epanechnikov function

ZPD <- function(x, location, radius) {
  ifelse(x > (location-radius) & x < (location+radius),
         1-((1/radius)*(x-location))^2,
         0)
}


