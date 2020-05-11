#' Function for calculating the baseline growth due to school exposure during interval t
#' called by school.growth()
#' @param x achievement value
#' @param achievement the student's current achievement
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param ZPD.width the radius of the student's ZPD
#' @param slope1 the slope of the lower leg of the school curriculum
#' @param slope2 the slope of the upper leg of the school curriculum
#' @param start the curriculum starting location
#' @param end the curriculum ending location
#' @examples
#' school.growth(x=.25, achievement=.22, ZPD.offset=0, ZPD.width=.03, slope1=30, slope2=30, start=.2, end=.25)
#'

school.growth <- function(x, achievement, ZPD.offset, ZPD.width, slope1, slope2, start, end) {
  ZPD(x=x, location=achievement+ZPD.offset, radius=ZPD.width) * school(x, slope1=slope1,
                                                                       slope2=slope2, start=start, end=end)
}
