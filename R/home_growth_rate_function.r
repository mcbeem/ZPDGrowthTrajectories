#' Function for calculating growth resulting from home experience at every possible achievement level,
#'  given a level of achievement, ZPD parameters, and home curriculum parameters.
#'  This is used to populate the lookup table for home growth.
#' Called by build.home.lookup()
#'
#' @param achievement a vector of achievement levels
#' @param integration.points integration points for numerical integration
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param ZPD.width the radius of the student's ZPD
#' @param rate exponential decay rate for the home curriculum function
#' @param integration.points integration points for numerical integration
#' @importFrom pracma trapz
#' @examples
#' \dontrun{
#' home.growth.rate(achievement=seq(0,1, .1), integration.points=200, ZPD.offset=0,
#'                  ZPD.width=.04, rate=5)
#' }

home.growth.rate <- function(achievement, ZPD.offset, ZPD.width, rate, integration.points) {
  # this function uses the trapezoidal area function from pracma
  # since we are just using it to populate the lookup tables, we don't
  # have to prioritize speed too much -- so we can afford to use a lot of integration
  # points for good accuracy

  # also note: the interval to be integrated only lies within the discrete limits
  # of the ZPD. E.g., we don't need to populate the entire range of potential achievement
  # with integration points.

  # width of a subinterval
  subint.width <- (2*ZPD.width) / integration.points
  # points spanning ZPD, but never at or below zero
  integration.pts <- seq(from=max((achievement+ZPD.offset-ZPD.width), 1e-5),
                         to=(achievement+ZPD.offset+ZPD.width),
                         length.out=integration.points)
  # evaluate
  return(pracma::trapz(x=integration.pts,
                       home.growth(x=integration.pts, achievement=achievement,
                                   ZPD.offset=ZPD.offset, ZPD.width=ZPD.width, rate=rate)))
}

# Vectorize the function over the achievement argument
home.growth.rate <- Vectorize(home.growth.rate, vectorize.args = "achievement")
