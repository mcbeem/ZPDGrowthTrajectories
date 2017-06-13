#' Visualize growth trajectories
#'
#' Function for visualizing synthethic growth trajectories.
#'#' 
#' \code{visualize} plots synthetic growth trajectories using \code{ggplot2}. 
#'
#' @param trajectories A data frame produced by the \code{ZPDGrowthTrajectories}, 
#' \code{ZPDGrowthTrajectories_studentmatrix}, or \code{ZPDGrowthTrajectories_multicore} functions.
#' Will be converted internally to "long" format suitable for \code{ggplot}. The function returns a 
#' \code{ggplot} object that can be modified with typical \code{ggplot2} arguments.
#' @export

visualize <- function(trajectories) {
  
  # check to see if the trajectories are in long or wide format
  # if long, it will have 3 columns
  # if wide format, flip to long
  
  if (ncol(trajectories) != 3) {
    nstudents <- nrow(trajectories)
    days <- ncol(trajectories)-1
    
    trajectories <- reshape2::melt(trajectories, measure.vars=c(1:days+1))
    trajectories[,2] <- rep(seq(1:days), each=nstudents)
    names(trajectories) <- c("student", "day", "achievement")
    trajectories <- trajectories[order(trajectories$student),]
  }
  
  p <- ggplot2::ggplot(data=trajectories, ggplot2::aes(x=day, y=achievement, color=factor(student)))+
    ggplot2::geom_line(show.legend=FALSE, size=.5, alpha=.5) + #ylim(0,1)+
    ggplot2::geom_hline(yintercept=0, col="gray")+ggplot2::geom_vline(xintercept=0, col="gray")+
    ggplot2::theme(text=element_text(family="Times New Roman", size=12),
          panel.background=element_blank(), panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank())
  
  return(p)
}
