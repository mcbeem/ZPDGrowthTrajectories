#' Function for rescaling achievement
#'
#' \code{rescaleTrajectories} performs linear rescaling of the synthetic achievement values
#'   produced by \code{ZPDGrowthTrajectories}
#'
#' \code{rescaleTrajectories} works in two modes. In the first, mode, the user supplies the intercept
#'   and slope values which are used to linearly rescale the synthetic achievement values computed
#'   by \code{ZPDGrowthTrajectories}. In the second mode, the user supplies a matrix of time points
#'   and desired values for the mean achievement at those time points. The function will calculate
#'   and apply the optimal rescaling coefficients and report the r-squared value. The user may optionally
#'   designate that this transformation should be based on a subset of the versions of the curriculum.
#'
#' @param trajectories An object of class \code{ZPD} produced by the \code{ZPDGrowthTrajectories()}
#'  function. If needed, this object will be converted internally to "long" format.
#'
#' @param intercept Optional scalar value providing the intercept for rescaling. Either \code{intercept}
#'   and \code{slope} or \code{benchmarks} must be specified. Defaults to NULL.
#'
#' @param slope Optional scalar value providing the slope for rescaling. Either \code{intercept}
#'   and \code{slope} or \code{benchmarks} must be specified.Defaults to NULL.
#'
#' @param benchmarks Optional numeric matrix with two columns. The first column contains time points.
#'   The second column contains target values for the mean achievement at those time points. The matrix
#'   must have at least two rows. Either \code{intercept} and \code{slope} or \code{benchmarks} must
#'   be specified. Defaults to NULL.
#'
#' @param version Optional numeric vector indicating that the mean achievement at the target time points
#'   should be computed only for students receiving specific version(s) of the curriculum. For example,
#'   \code{c(1,2)} indicates that the rescaling should be based only on those students receiving versions
#'   1 or 2 of the curriculum. If provided, \code{benchmarks} must also be specified. Defaults to NULL.
#'
#' @return An object of class \code{"ZPD", "data.frame"} in "long" format
#'
#' @importFrom checkmate qtest
#' @importFrom stats lm coef
#'
#' @examples
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
#' # assignment object simulating starting kindergarten on time 801
#' #  Kindergarten for 200 days, followed by 100 days of summer
#' #  then 200 days of first grade
#' assignment <- c(rep(0, times=800), rep(1, times=200),
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
#' )
#'
#' curriculum.advanced.slopes <- list(
#'   # "typical curriculum" advanced slopes for K and first grade
#'   matrix(c(50, 50), nrow=2, ncol=1),
#'   # "advanced curriculum" advanced slopes for K and first grade
#'   matrix(c(25, 25), nrow=2, ncol=1)
#' )
#'
#' # students 1 and 2 get typical curriculum, 3 and 4 get advanced
#' which.curriculum <- c(1,1,2,2)
#'
#' y <- ZPDGrowthTrajectories(learn.rate=learn.rate, home.env=home.env,
#'                            decay.rate=decay.rate, initial.ach=initial.ach,
#'                            ZPD.width=.05, ZPD.offset=.02,
#'                            home.learning.decay.rate=6,
#'                            curriculum.start.points=curriculum.start.points,
#'                            curriculum.widths=curriculum.widths,
#'                            curriculum.review.slopes=curriculum.review.slopes,
#'                            curriculum.advanced.slopes=curriculum.advanced.slopes,
#'                            assignment=assignment, dosage=.8,
#'                            adaptive.curriculum=FALSE,
#'                            which.curriculum=which.curriculum,
#'                            school.weight=.5, home.weight=1, decay.weight=.05,
#'                            verbose=TRUE)
#'
#' describeTrajectories(y, byVersion=FALSE)
#'
#' y2 <- rescaleTrajectories(y, intercept=100, slope=50)
#' describeTrajectories(y2, byVersion=FALSE)
#'
#' benchmarks <- matrix(c(
#'   300, 150.0,
#'   600, 187.2,
#'   800, 195.2), ncol=2, byrow=TRUE)
#'
#' y3 <- rescaleTrajectories(y, benchmarks=benchmarks)
#' describeTrajectories(y3, byVersion=FALSE)
#'
#' @export

rescaleTrajectories <- function(trajectories, intercept=NULL, slope=NULL, benchmarks=NULL, version=NULL) {

  # thank you r-bloggers
  # https://www.r-bloggers.com/the-notin-operator/
  `%notin%` <- Negate(`%in%`)

  # check arguments
  # check if trajectories is class ZPD, if not stop
  if(!("ZPD" %in% class(trajectories))) {stop("Object supplied to trajectories argument is not ZPDGrowthTrajectories() output")}

  if ((is.null(intercept) | is.null(slope)) & is.null(benchmarks)) {
    stop("both intercept and slope must be specified if no benchmarks are provided")
  }

  # intercept and slope must be numeric, non-missing, of length 1
  if (!is.null(intercept)) {
    if (qtest(intercept, "N1") == FALSE) {stop("intercept must be a scalar")}
  }

  if (!is.null(slope)) {
    if (qtest(slope, "N1") == FALSE) {stop("slope must be a scalar")}
  }

  # if benchmarks is provided, it must be a matrix with no NAs, proper dimensions, and values in range,
  #   and intercept and slope arguments should not be given

  if (!is.null(benchmarks)) {
    if (qtest(benchmarks, "M") == FALSE) {stop("benchmarks must be a matrix")}
    if (any(benchmarks[,1] %notin% trajectories$time)) {stop("at least one value in the first column of benchmarks is outside the range of time values in trajectories")}
    if (nrow(benchmarks) < 2) {stop("benchmarks must have at least two rows")}
    if (ncol(benchmarks) != 2) {stop("benchmarks must have two columns")}
    if (!is.null(intercept) | !is.null(slope)) {stop("intercept and slope should not be specified if benchmarks are provided")}
   }

  # if version is provided, it must be integer-line with no NA and length 1, and values in range
  if (!is.null(version)) {
    if (qtest(version, "X>=1[1,)") == FALSE) {stop("version must be a postive scalar or vector containing integer values")}
    if (any(!version %in% trajectories$version)) {stop("the specified value(s) of version are not found in trajectories object")}
  }

  if (!is.null(version) & is.null(benchmarks)) {stop("benchmarks must be specified if a value is given for version")}


  if (!is.null(benchmarks)) {

    if (is.null(version)) {

      # get the mean achievement at the benchmark times for all versions
      x <- describeTrajectories(trajectories, byTransition=FALSE, byVersion=F,
                                times=benchmarks[,1])$mean
    }

    if (!is.null(version)) {

      # get the mean achievement at the benchmark times for the specified versions
      x <- describeTrajectories(trajectories[trajectories$version %in% version,], byTransition=FALSE, byVersion=F,
                                times=benchmarks[,1])$mean
    }

    # fit regression, calculate intercept, slope, and R2
    y <- benchmarks[,2]
    fit <- lm(y~x)

    intercept <- coef(lm(y~x))[1]
    slope <- coef(lm(y~x))[2]

    rsquare <- summary(fit)$r.square

    print(paste0("The computed scaling factors are intercept = ", round(intercept, 4), ", slope = ",
                 round(slope, 4), ". The r-squared value is ", round(rsquare, 3)))

  }

  # rescale achievement
  trajectories$achievement <- (trajectories$achievement*slope) + intercept

  return(trajectories)
}
