#' Visualize the zone of proximal development
#'
#' \code{visualizeZPD()} is a function for visualizing the zone of proximal development (ZPD).
#'
#'  \code{visualizeZPD()} plots the ZPD against the achievement. It is useful for exploring
#'    the consequences of the \code{zpd.width} and \code{zpd.offset} arguments to
#'    \code{ZPDGrowthTrajectories()}. The function returns a \code{ggplot} object that
#'    can be modified with typical \code{ggplot2} arguments.
#'
#' @param achievement the current level of achievement, defaults to 0.3
#'
#' @param zpd.width the radius of the ZPD.
#'
#' @param zpd.offset scalar value, measured on the same scale as achievement, describing where the ZPD peaks
#'   relative to the current achievement.
#'
#' @return An object of class \code{ggplot2}
#'
#' @family visualizations
#'
#' @examples
#' visualizeZPD(achievement=.3, zpd.offset=.02, zpd.width=.06)
#' @export

visualizeZPD <- function(achievement, zpd.offset, zpd.width) {

  x <- seq(min(0, achievement+zpd.offset-zpd.width),
           max(1, achievement+zpd.offset+zpd.width),
           length.out=10000)
  y <- ZPD(x=x, location=achievement+zpd.offset, radius=zpd.width)

  # normalize intensity (y)
  y <- y / max(y)

  data <- data.frame(cbind(x,y))

  # calculate area to shade
  shade <- rbind(c(0,0), subset(data, (x>=min(x) & x<= max(x))), c(1, 0))

  p <- ggplot2::ggplot(data=data, ggplot2::aes(x=x, y=y))+ggplot2::geom_line(alpha=.5)+
    ggplot2::geom_polygon(data=shade, ggplot2::aes(x,y), fill="blue", alpha=.1)+
    ggplot2::geom_vline(xintercept=achievement, col="darkblue", cex=.75, alpha=.8)+
    ggplot2::theme_classic()+ggplot2::xlab("achievement")+ggplot2::ylab("intensity")

  return(p)
}
