#' Create summary statistics for ZPDGrowthTrajectories
#'
#' \code{describeTrajectories} calculates summary statistics by time interval and curriculum
#' for \code{ZPDGrowthTrajectories} output.
#'
#' @param trajectories An object of class \code{ZPD} produced by the \code{ZPDGrowthTrajectories()}
#'  function. If needed, this object be converted internally to "long" format.
#' @param assignment a vector indicating which school curriculum, if any, was provided during
#'  each time interval. If provided, summary statistics are calculated at each transition. Defaults
#'  to NULL.
#' @param byCurriculum Logical. Should descriptives be broken down by curriculum? Defaults
#'  to TRUE.
#'
#' @importFrom utils tail
#' @importFrom dplyr summarise group_by n filter select everything
#' @importFrom stats median sd
#'
#' @export

describeTrajectories <- function(trajectories, assignment=NULL, byCurriculum=TRUE) {

  # check if trajectories is class ZPD, if not stop
  if(!("ZPD" %in% class(trajectories))) {stop("Object supplied to trajectories argument is not ZPDGrowthTrajectories() output")}

  # check to see if the trajectories are in long or wide format
  # if long, it will have 9 columns
  # if wide format, flip to long

  if (ncol(trajectories) != 9) {
    nstudents <- nrow(trajectories)
    times <- ncol(trajectories)-6

    trajectories <- reshape2::melt(trajectories, id.vars=1:6)
    trajectories[,7] <- rep(seq(1:times), each=nstudents)
    names(trajectories) <- c("id", "learn.rate", "home.env", "decay.rate", "initial.ach",
                             "curriculum", "time", "achievement")
    trajectories <- trajectories[order(trajectories$id),]
  }

 if (is.null(assignment)) {
    warning("No value provided to assignment argument; will calculate
        summary statistics for each time interval")

    assignment <- unique(trajectories$time)
    index <- assignment
    noassignment.flag <- TRUE

 } else {

    noassignment.flag <- FALSE

    # find transitions in the assignment object
    lag.assignment <- c(utils::tail(assignment,-1),NA)

    # index of those transitions, appending first and last time point / time
    index <- c(1, which(assignment != lag.assignment), length(assignment))
 }

 if (byCurriculum == TRUE) {
 summarytable <- dplyr::summarise(dplyr::group_by(dplyr::filter(trajectories, time %in% index),
                                  time, curriculum), n=dplyr::n(),
                  mean=mean(achievement, na.rm=TRUE),
                  median=stats::median(achievement, na.rm=TRUE),
                  stddev=stats::sd(achievement, na.rm=TRUE),
                  min=min(achievement, na.rm=TRUE),
                  max=max(achievement, na.rm=TRUE))
 } else if (byCurriculum == FALSE) {
   summarytable <- dplyr::summarise(dplyr::group_by(dplyr::filter(trajectories, time %in% index),
                                                    time), n=dplyr::n(),
                                    mean=mean(achievement, na.rm=TRUE),
                                    median=stats::median(achievement, na.rm=TRUE),
                                    stddev=stats::sd(achievement, na.rm=TRUE),
                                    min=min(achievement, na.rm=TRUE),
                                    max=max(achievement, na.rm=TRUE))
 }

 if (noassignment.flag == FALSE) {

      summarytable$assignment <- assignment[summarytable$time]

      summarytable <- dplyr::select(summarytable, time, assignment, dplyr::everything())
 }

 return(summarytable)

}





