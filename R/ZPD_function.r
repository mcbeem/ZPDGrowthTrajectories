#' Function to define the ZPD using an Epanechnikov kernel
#' called by home.growth() and school.growth() functions
#' Home curriculum intensity given by (-rate*x)
#'
#' @param x level of achievement
#' @param location location of the ZPD
#' @param radius radius of the ZPD
#' @examples
#' \dontrun{
#' ZPD(x=.37, radius=.05, location=.4)
#' }


# function defining the ZPD as an Epanechnikov function

ZPD <- function(x, location, radius) {
  ifelse(x > (location-radius) & x < (location+radius),
         1-((1/radius)*(x-location))^2,
         0)
}


