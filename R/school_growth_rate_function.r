#' Function for calculating growth resulting from school at every possible achievement level,
#'  given a level of achievement, ZPD parameters, and school curriculum parameters.
#'  This is used to populate the lookup table for school growth.
#' Called by build.school.lookup()
#'
#' @param achievement a vector of achievement levels
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param ZPD.width the radius of the student's ZPD
#' @param slope1 the slope of the lower leg of the school curriculum
#' @param slope2 the slope of the upper leg of the school curriculum
#' @param start the curriculum starting location
#' @param end the curriculum ending location
#' @param integration.points integration points for numerical integration
#'
#' @importFrom pracma trapz
#'
#' @examples
#' \dontrun{
#' school.growth.rate(achievement=seq(0,1, .1), integration.points=200, ZPD.offset=.05,
#'   ZPD.width=.04, slope1=10, slope2=20, start=.1, end=.2)
#'   }
#'
school.growth.rate <- function(achievement, ZPD.offset, ZPD.width, slope1, slope2,
                               start, end, integration.points) {

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
  #  (the home curriculum function, being based on exponential decay, is
  #   undefined at or below zero)
  integration.pts <- seq(from=max((achievement+ZPD.offset-ZPD.width), 1e-5),
                         to=(achievement+ZPD.offset+ZPD.width),
                         length.out=integration.points)
  # numerically evaluate the integral
  return(pracma::trapz(x=integration.pts,
                       school.growth(x=integration.pts, achievement=achievement, ZPD.offset=ZPD.offset,
                                     ZPD.width=ZPD.width, start=start, end=end,
                                     slope1=slope1, slope2=slope2)))
}

# Vectorize the function over the achievement argument
school.growth.rate <- Vectorize(school.growth.rate, vectorize.args = "achievement")
