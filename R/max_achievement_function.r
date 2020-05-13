#' Function for calculating the max value that should be included in the lookup table
#' based on the max school curriculum and the point at which the home growth rate
#' falls below some negligible threshold
#' called by ZPDGrowthTrajectories() function
#'
#' @param ZPD.width the radius of the student's ZPD
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param curriculum.start.points matrix of curriculum starting locations
#' @param curriculum.widths matrix of curriculum widths
#' @param slope2 matrix of slopes of the upper leg of the school curriculum
#' @param rate exponential decay rate for the home curriculum function
#' @param threshold the threshold growth rate for finding the effective maximum possible achievement
#' @param integration.points integration points for numerical integration
#'
#' @importFrom stats uniroot
#'
#' @examples
#' \dontrun{
#' curriculum.start.points <- matrix(c(.1, .2, .3), ncol=1)
#' curriculum.widths <- matrix(rep(.11, 3), ncol=1)
#' slope2 <- matrix(rep(10, 3), ncol=1)
#'
#' max.achievement(ZPD.width=.05, ZPD.offset=.02,
#'                 curriculum.start.points=curriculum.start.points,
#'                 curriculum.widths=curriculum.widths,
#'                 slope2=slope2, rate=6, threshold=.00001,
#'                 integration.points=2000)
#' }

max.achievement <- function(ZPD.width, ZPD.offset,
                            curriculum.start.points, curriculum.widths,
                            slope2, rate, threshold, integration.points) {

  # find the maximum achievement level than can be obtained by exposure
  #  to the school curriculum
  # this will occur when the ZPD is fully above the highest curriculum
  school.max <- max(
    unlist(curriculum.start.points) + unlist(curriculum.widths) + 1/unlist(slope2))  +
    ZPD.width - ZPD.offset

  # find the achievement level above which growth via home curriculum
  #  falls below some insignificant threshold value. This growth never
  #  becomes zero because the home curriculum function (exponential decay)
  #  has an asymptote at zero

  home.max <- stats::uniroot(function(achievement){
    home.growth.rate(achievement=achievement, rate=rate, ZPD.offset=ZPD.offset,
                     ZPD.width=ZPD.width, integration.points=integration.points)-
      threshold}, interval=c(0, 1e100))$root

  return(max(school.max, home.max))
}
