#' Visualize the zone of proximal development
#'
#' \code{visualizeZPD} is a function for visualizing the zone of proximal development (ZPD).
#'
#'  \code{visualizeZPD} plots the ZPD against the achievement. It is useful for exploring
#'    the consequences of the \code{zpd.sd} and \code{zpd.offset} arguments to 
#'    \code{ZPDGrowthTrajectories()} and related functions. The function returns a 
#'    \code{ggplot} object that can be modified with typical \code{ggplot2} arguments.
#'
#'  @param achievement A scalar value between 0 and 1 indicating the current achievement level.
#'   Defaults to 0.3. 
#'  @param zpd.sd A positive scalar describing the standard deviation of the normal curve describing
#'    the zone of proximal development. Controls the 'width' of the ZPD.
#'  @param zpd.offset A scalar (typically positive) describing the distance between achievement and 
#'    the peak intensity of the ZPD.
#' @export

visualizeZPD <- function(achievement=.3, zpd.sd, zpd.offset) {
 
  x <- seq(0, 1, length.out=10000)
  y <- dnorm(x, mean=achievement+zpd.offset, sd=zpd.sd)
  
  # normalize intensity (y)
  y <- y / max(y)
  
  data <- data.frame(cbind(x,y))
  
  # calculate area to shade
  shade <- rbind(c(0,0), subset(data, (x>=0 & x<= 1)), c(1, 0))
  
  p <- ggplot2::ggplot(data=data, ggplot2::aes(x=x, y=y))+ggplot2::geom_line(alpha=.5)+ 
    ggplot2::geom_polygon(data=shade, ggplot2::aes(x,y), fill="blue", alpha=.1)+
    ggplot2::geom_vline(xintercept=achievement, col="darkblue", cex=.75, alpha=.8)+
    ggplot2::theme_classic()+ggplot2::xlab("achievement")+ggplot2::ylab("intensity")
    
  return(p)
}