#' Function for calculating the growth during interval t from exposure to home
#' called by home.growth.rate() function
#'
#' @param x achievement value
#' @param achievement the student's current achievement
#' @param ZPD.offset offset of the student's ZPD from the current level of achievement
#' @param ZPD.width the radius of the student's ZPD
#' @param rate exponential decay rate for the home curriculum function
#' @examples
#' home.growth(x=.2, achievement=.2, ZPD.offset=0, ZPD.width=.1, rate=5)

home.growth <- function(x, achievement, ZPD.offset, ZPD.width, rate) {
  ZPD(x=x, location=achievement+ZPD.offset, radius=ZPD.width) * home(x, rate=rate)
}

