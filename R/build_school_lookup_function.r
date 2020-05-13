#' Function to build the school growth lookup table
#' called by ZPDGrowthTrajectories() function
#'
#' @param integration.points integration points for numerical integration
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param ZPD.width the radius of the student's ZPD
#' @param curriculum.start.points matrix or list of matrices of curriculum starting locations
#' @param curriculum.widths matrix or list of matrices of curriculum widths
#' @param slope1 matrix of the slopes of the lower leg of the school curriculum
#' @param slope2 matrix of the slopes of the upper leg of the school curriculum
#' @param maxachievement the maximum achieveable level of achievement
#' @examples
#' \dontrun{
#' curriculum.start.points <- matrix(c(.1, .2, .3), ncol=1)
#' curriculum.widths <- matrx(rep(.11, 3), ncol=1)
#' slope1 <- matrix(rep(20, 3), ncol=1)
#' slope2 <- matrix(rep(50, 3), ncol=1)
#' build.school.lookup(integration.points=20, ZPD.width=.05, ZPD.offset=.02,
#'                       curriculum.start.points=curriculum.start.points,
#'                       curriculum.widths=curriculum.widths,
#'                       slope1=slope1, slope2=slope2, maxachievement=1.5)
#'  }

build.school.lookup <- function(integration.points, ZPD.width, ZPD.offset,
                                curriculum.start.points,
                                curriculum.widths,
                                slope1, slope2,
                                maxachievement) {

  achievement <- seq(.0001, maxachievement, length.out=integration.points)

  school.lookup <- matrix(achievement, nrow=integration.points, ncol=1)

  for (i in 1:nrow(curriculum.start.points)) {
    school.lookup <- cbind(school.lookup,
                           school.growth.rate(integration.points=integration.points,
                                              achievement=achievement, ZPD.offset=ZPD.offset,
                                              ZPD.width=ZPD.width, slope1=slope1[i], slope2=slope2[i],
                                              start=curriculum.start.points[i],
                                              end=curriculum.start.points[i]+curriculum.widths[i]))
  }

  return(school.lookup)
}
