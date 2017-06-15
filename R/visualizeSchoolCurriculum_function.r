#' Function for visualizing the school curriculum.
#' 
#' \code{visualizeSchoolCurriculum} is a function for visualizing the school curriculum.
#'
#'  \code{visualizeSchoolCurriculum} plots school curriculum. It is useful for exploring
#'    the consequences of the \code{curriculum.start.points}, \code{curriculum.widths}, 
#'    \code{curriculum.lower.slope}, \code{curriculum.upper.slope}, and \code{alpha} arguments to 
#'    \code{ZPDGrowthTrajectories()}. The function returns a 
#'    \code{ggplot} object that can be modified with typical \code{ggplot2} arguments.
#'
#' @param start.point a scalar value between 0 and 1 providing the lower bound value of achievement
#'   at which the school curriculum has maximum intensity.
#' @param width a scalar value between 0 and 1 providing the upper bound value of achievement
#'   at which the school curriculum has maximum intensity.
#' @param lower.slope scalar argument to the \code{mode1} argument of 
#'   \code{trapedoid::dtrapezoid}; controls steepness of the school curriculum cutoff at the 
#'   lower range. Conceptually controls the amount of review content. Defaults to 8.
#' @param upper.slope scalar argument to the \code{mode2} argument of 
#'   \code{trapedoid::dtrapezoid}; controls steepness of the school curriculum cutoff at the 
#'   upper range. Conceptually controls the amount of advanced content. Defaults to 300.
#' @param alpha scalar argument to the \code{mode2} argument of \code{trapedoid::dtrapezoid}. 
#'   Controls the shape of the school curriculum at maximum intensity. Defaults to 1.
#' @export

visualizeSchoolCurriculum <- function(start.point, width, lower.slope=8, upper.slope=300, alpha=1) {
  
  x <- seq(0, 1, length.out=10000)
  y <- trapezoid::dtrapezoid(x,
                             n1=lower.slope,
                             n3=upper.slope,
                             mode1=start.point,
                             mode2=start.point+width, 
                             alpha=alpha)
  
  # normalize intensity (y)
  y <- y / max(y)
  
  data <- data.frame(cbind(x,y))
  
  # calculate area to shade
  shade <- rbind(c(0,0), subset(data, (x>=0 & x<= 1)), c(1, 0))
  
  p <- ggplot2::ggplot(data=data, ggplot2::aes(x=x, y=y))+ggplot2::geom_line(alpha=.5)+ 
    ggplot2::geom_polygon(data=shade, ggplot2::aes(x,y), fill="blue", alpha=.1)+
    ggplot2::theme_classic()+ggplot2::ylab("achievement")+ggplot2::xlab("intensity")
  
  return(p)
}