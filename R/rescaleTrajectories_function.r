#'
#'
#' @param trajectories
#'
#' @param intercept
#'
#' @param slope
#'
#' @param benchmarks
#'
#' @param version
#'
#' @importFrom checkmate qtest
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

  if (!is.null(intercept)) {
    if (qtest(intercept, "N1") == FALSE) {stop("intercept must be a scalar")}
  }

  if (!is.null(slope)) {
    if (qtest(slope, "N1") == FALSE) {stop("slope must be a scalar")}
  }


  if (!is.null(benchmarks)) {
    if (qtest(benchmarks, "M") == FALSE) {stop("benchmarks must be a matrix")}
    if (any(benchmarks[,1] %notin% trajectories$time)) {stop("at least one value in the first column of benchmarks is outside the range of time values in trajectories")}
    if (nrow(benchmarks) < 2) {stop("benchmarks must have at least two rows")}
    if (ncol(benchmarks) != 2) {stop("benchmarks must have two columns")}
   }

  if (!is.null(version)) {
    if (qtest(version, "X>=1[1,)") == FALSE) {stop("version must be a postive scalar or vector containing integer values")}
  }

  if (!is.null(version) & is.null(benchmarks)) {stop("benchmarks must be specified if a value is given for version")}

  if (!is.null(version)) {
    if (any(!version %in% trajectories$version)) {stop("the specified value(s) of version are not found in trajectories object")}
    }

  if (!is.null(benchmarks)) {

    if (!is.null(intercept) | !is.null(slope)) {stop("intercept and slope should not be specified if benchmarks are provided")}

    if (is.null(version)) {

      x <- describeTrajectories(trajectories, byTransition=FALSE, byVersion=F,
                                times=benchmarks[,1])$mean
    }

    if (!is.null(version)) {

      x <- describeTrajectories(trajectories[trajectories$version %in% version,], byTransition=FALSE, byVersion=F,
                                times=benchmarks[,1])$mean
    }

    y <- benchmarks[,2]
    fit <- lm(y~x)

    intercept <- coef(lm(y~x))[1]
    slope <- coef(lm(y~x))[2]

    rsquare <- summary(fit)$r.square

    print(paste0("The computed scaling factors are intercept = ", round(intercept, 4), ", slope = ",
                 round(slope, 4), ". The r-squared value is ", round(rsquare, 3)))

  }

  trajectories$achievement <- (trajectories$achievement*slope) + intercept

  return(trajectories)
}

  describeTrajectories(
    rescaleTrajectories(trajectories, benchmarks=benchmarks, version=c(1,2))
    )
