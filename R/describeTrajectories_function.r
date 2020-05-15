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
#' @examples
#'
#' # learning rate
#' learn.rate <- c(.08, .10, .12, .18)
#'
#' # decay rate
#' decay.rate <- c(.04, .03, .02, .01)
#'
#' # initial achievement
#' initial.ach <- rep(0, times=4)
#'
#' # quality of home environment
#' home.env <- c(.06, .12, .15, .20)
#'
#' # assignment object simulating starting kindergarten on time 201
#' #  Kindergarten for 200 days, followed by 100 days of summer
#' #  then 200 days of first grade
#' assignment <- c(rep(0, times=200), rep(1, times=200),
#'                 rep(0, times=100), rep(2, times=200))
#'
#' # define school curriculum
#' curriculum.start.points <- list(
#'     # "typical curriculum" start points for K and first grade
#'   matrix(c(.2, .26), nrow=2, ncol=1),
#'     # "advanced curriculum" start points for K and first grade
#'   matrix(c(.22, .29), nrow=2, ncol=1)
#' )
#'
#' curriculum.widths <- list(
#'   # "typical curriculum" widths for K and first grade
#'   matrix(c(.03, .03), nrow=2, ncol=1),
#'   # "advanced curriculum" widths for K and first grade
#'   matrix(c(.04, .04), nrow=2, ncol=1)
#' )
#'
#' curriculum.review.slopes <- list(
#'   # "typical curriculum" review slopes for K and first grade
#'   matrix(c(15, 15), nrow=2, ncol=1),
#'   # "advanced curriculum" review slopes for K and first grade
#'   matrix(c(30, 30), nrow=2, ncol=1)
#'   )
#'
#' curriculum.advanced.slopes <- list(
#'   # "typical curriculum" advanced slopes for K and first grade
#'   matrix(c(50, 50), nrow=2, ncol=1),
#'   # "advanced curriculum" advanced slopes for K and first grade
#'   matrix(c(25, 25), nrow=2, ncol=1)
#'   )
#'
#'  # students 1 and 2 get typical curriculum, 3 and 4 get advanced
#'  which.curriculum <- c(1,1,2,2)
#'
#'  y <- ZPDGrowthTrajectories(learn.rate=learn.rate, home.env=home.env,
#'                         decay.rate=decay.rate, initial.ach=initial.ach,
#'                         ZPD.width=.05, ZPD.offset=.02,
#'                         home.learning.decay.rate=6,
#'                         curriculum.start.points=curriculum.start.points,
#'                         curriculum.widths=curriculum.widths,
#'                         curriculum.review.slopes=curriculum.review.slopes,
#'                         curriculum.advanced.slopes=curriculum.advanced.slopes,
#'                         assignment=assignment, dosage=.8,
#'                         adaptive.curriculum=FALSE,
#'                         which.curriculum=which.curriculum,
#'                         school.weight=.5, home.weight=1, decay.weight=.05,
#'                         verbose=TRUE)
#
#' describeTrajectories(y, byVersion=FALSE)
#'
#' describeTrajectories(y, byTransition=FALSE, times=c(100, 300, 650))
#'
#' @export

describeTrajectories <- function(trajectories, byTransition=TRUE, byVersion=TRUE, times=NULL) {

  `%notin%` <- Negate(`%in%`)

  # check if trajectories is class ZPD, if not stop
  if(!("ZPD" %in% class(trajectories))) {stop("Object supplied to trajectories argument is not ZPDGrowthTrajectories() output")}

  # check arguments

  # check if byVersion is logical, length one, not NA
  if (checkmate::qtest(byVersion, "B1") == FALSE) {stop("byVersion must be TRUE or FALSE")}

  # check if byTransition is logical, length one, not NA
  if (checkmate::qtest(byTransition, "B1") == FALSE) {stop("byTransition must be TRUE or FALSE")}

  # if times is not NULL, check that it is an integer-like object of length 1+ containing no NAs
  if (!is.null(times)) {
    if (checkmate::qtest(times, "X+[1,)") == FALSE) {stop("times must either be NULL or an integer vector containing positive values")}
    if (any(times %notin% trajectories$time)) {stop("a value in times exceeds the range of time points included in trajectories")}
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
