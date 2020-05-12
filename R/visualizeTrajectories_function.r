#' Visualize growth trajectories
#'
#' Function for visualizing synthethic growth trajectories.
#'
#' \code{visualizeTrajectories} plots synthetic growth trajectories using \code{ggplot2}.
#'
#' @param trajectories A data frame produced by the \code{ZPDGrowthTrajectories()} function.
#' If needed, this object be converted internally to "long" format suitable for \code{ggplot}.
#' The function returns a \code{ggplot} object that can be modified with typical \code{ggplot2} arguments.
#' @export

visualizeTrajectories <- function(trajectories) {

  # check to see if the trajectories are in long or wide format
  # if long, it will have 9 columns
  # if wide format, flip to long

  if (ncol(trajectories) != 9) {
    nstudents <- nrow(trajectories)
    days <- ncol(trajectories)-6

    trajectories <- reshape2::melt(trajectories, id.vars=1:6)
    trajectories[,7] <- rep(seq(1:days), each=nstudents)
    names(trajectories) <- c("id", "learn.rate", "home.env", "decay.rate", "initial.ach",
                            "curriculum", "day", "achievement")
    trajectories <- trajectories[order(trajectories$id),]
  }

  p <- ggplot2::ggplot(data=trajectories, ggplot2::aes(x=trajectories$day, y=trajectories$achievement, color=factor(trajectories$id)))+
    ggplot2::geom_line(show.legend=FALSE, size=.5, alpha=.5) +
    ggplot2::geom_hline(yintercept=0, col="gray")+ggplot2::geom_vline(xintercept=0, col="gray")+
    ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank())

  return(p)
}
