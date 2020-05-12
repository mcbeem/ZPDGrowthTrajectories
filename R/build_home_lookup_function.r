#' Function to build the home growth lookup table
#' called by ZPDGrowthTrajectories() function
#'
#' @param integration.points integration points for numerical integration
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param ZPD.width the radius of the student's ZPD
#' @param rate exponential decay rate for the home curriculum function
#' @param maxachievement the maximum achieveable level of achievement
#' @examples
#'\dontrun{
#' build.home.lookup(integration.points=20, ZPD.width=.05, ZPD.offset=.02,
#'                  rate=4, maxachievement=1.5)
#' }

build.home.lookup <- function(integration.points, ZPD.width, ZPD.offset,
                              rate, maxachievement) {

  achievement <- seq(.0001, maxachievement, length.out=integration.points)

  home.lookup <- cbind(achievement,
                       home.growth.rate(achievement=achievement,
                                       integration.points=integration.points,
                                       ZPD.offset=ZPD.offset, ZPD.width=ZPD.width, rate))

  return(home.lookup)
}
