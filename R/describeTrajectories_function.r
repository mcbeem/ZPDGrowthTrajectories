#' Create summary statistics for ZPDGrowthTrajectories
#'
#' \code{describeTrajectories} calculates summary statistics by time interval and version of the
#'   school curriculum for for \code{ZPDGrowthTrajectories()} output.
#'
#' @param trajectories An object of class \code{ZPD} produced by the \code{ZPDGrowthTrajectories()}
#'  function. If needed, this object will be converted internally to "long" format.
#'
#' @param byTransition Logical. Should descriptives be provided only at curricular transitions (as
#'  well as the first and last time point)? Defaults to TRUE.
#'
#' @param byVersion Logical. Should descriptives be broken down by version of the curriculum (
#'   e.g., typical, remedial)? Defaults to TRUE.
#'
#' @param times Optional. A vector of specific times at which descriptives should be computed.
#'   Defaults to NULL.
#'
#' @return An object of class \code{tibble}
#'
#' @seealso \code{\link{visualizeTrajectories}} for plotting the trajectories.
#'
#' @seealso \code{\link{ZPDGrowthTrajectories}} for simulating growth trajectories.
#'
#' @importFrom utils tail
#' @importFrom dplyr summarise group_by n filter select everything
#' @importFrom stats median sd
#' @importFrom checkmate qtest
#'
#' @export

describeTrajectories <- function(trajectories, byTransition=TRUE, byVersion=TRUE, times=NULL) {

  # check if trajectories is class ZPD, if not stop
  if(!("ZPD" %in% class(trajectories))) {stop("Object supplied to trajectories argument is not ZPDGrowthTrajectories() output")}

  if (checkmate::qtest(byVersion, "b1") == FALSE) {stop("byVersion must be TRUE or FALSE")}
  if (checkmate::qtest(byTransition, "b1") == FALSE) {stop("byTransition must be TRUE or FALSE")}

  if (!is.null(times)) {
    if (checkmate::qtest(times, "i+[1,)") == FALSE) {stop("times must either be NULL or an integer vector containing positive values")}
    if (max(times) > max(trajectories$assignment)) {stop("a value in times exceeds the range of time points included in trajectories")}
  }

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

  # get assignment object from ZPDGrowthTrajectories output
  assignment <- trajectories$assignment[trajectories$version==1 & trajectories$id==1]

  if (byTransition==FALSE & is.null(times)) {

    warning("byTransition is FALSE and no values were supplied for times; computing descriptives for each time point.")
    assignment <- unique(trajectories$time)
    index <- assignment
    noassignment.flag <- TRUE

 } else if (byTransition==FALSE & !is.null(times)) {

   index <- times
   noassignment.flag <- FALSE #?

 } else {

    noassignment.flag <- FALSE

    # find transitions in the assignment object
    lag.assignment <- c(utils::tail(assignment,-1),NA)

    # index of those transitions, appending first and last time point / time
    index <- c(1, which(assignment != lag.assignment), length(assignment))

    if (!is.null(times)) {
      # append the values specified for times
      index <- c(index, times)
      # sort them
      index <- index[order(index)]
    }
 }

 if (byVersion == TRUE) {
 summarytable <- dplyr::summarise(dplyr::group_by(dplyr::filter(trajectories, time %in% index),
                                  time, version), n=dplyr::n(),
                  mean=mean(achievement, na.rm=TRUE),
                  median=stats::median(achievement, na.rm=TRUE),
                  stddev=stats::sd(achievement, na.rm=TRUE),
                  min=min(achievement, na.rm=TRUE),
                  max=max(achievement, na.rm=TRUE))

 } else if (byVersion == FALSE) {
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





