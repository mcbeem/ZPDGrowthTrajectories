#' Function for visualizing the home curriculum.
#' 
#' \code{visualizeHomeCurriculum} is a function for visualizing the home curriculum function.
#'
#'  \code{visualizeHomeCurriculum} plots the home curriculum. It is useful for exploring
#'    the consequences of the \code{home.curriculum.shape1} and \code{home.curriculum.shape2}
#'    arguments to \code{ZPDGrowthTrajectories()} and related functions. The function returns a 
#'    \code{ggplot} object that can be modified with typical \code{ggplot2} arguments.
#'
#' @param shape1 The \code{shape1} argument to the \code{dbeta} function for describing the shape of the
#'   home curriculum. Defaults to 1.0.
#' @param shape2 The \code{shape2} argument to the \code{dbeta} function for describing the shape of the
#'   home curriculum. 
#' @export

visualizeHomeCurriculum <- function(shape1=1, shape2) {
  
  x <- seq(0, 1, length.out=10000)
  y <- dbeta(x, shape1=shape1, shape2=shape2)
  
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