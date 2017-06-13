#' Function for creating simulated growth trajectories from the theoretical model.
#'
#' description.
#'
#'
#' @param output.format Format of the results, "long" for long format, "wide" for wide. Defaults to "wide".
#' @param days the number of days to simulate.
#' @param assignment a vector. The length is the number of days to simulate. Each entry contains a number representing which
#'   grade-level curriculum to present. Zero denotes summers. The numbers correspond to the row index of the
#'    \code{curriculum.start.points} and \code{width} objects.
#' @param curriculum.start.points a matrix providing the start points of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param curriculum.widths a matrix providing the spans of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param dosage scalar dose parameter, controls mixing of school curriculum and home curriculum during school years.
#'   Must be [0,1].
#' @param learning.rates vector of learning rates, one per child.
#' @param decay.rates vector of decay rates, one per child. Controls forgetting.
#' @param initial.achievements vector of initial achievements, one per child.
#' @param home.environments vector of home environments, one per child.
#' @param integration.points number of integration points. Controls tradeoff between accuracy and execution speed.
#'   Defaults to 200.
#' @param curriculum.lower.slope scalar argument to the \code{mode1} argument of \code{trapedoid::dtrapezoid}; controls
#'   steepness of the school curriculum cutoff at the lower range. Conceptually controls the amount of review content.
#' @param curriculum.upper.slope scalar argument to the \code{mode2} argument of \code{trapedoid::dtrapezoid}; controls
#'   steepness of the school curriculum cutoff at the upper range. Conceptually controls the amount of advanced content.
#' @param alpha scalar argument to the \code{mode2} argument of \code{trapedoid::dtrapezoid}. Defaults to 1.
#' @param home.curriculum.shape1 The \code{shape1} argument to the \code{dbeta} function for describing the shape of the
#'   home curriculum. Defaults to 1.
#' @param home.curriculum.shape2 The \code{shape2} argument to the \code{dbeta} function for describing the shape of the
#'   home curriculum. Defaults to 4.6.
#' @param zpd.offset scalar value, measured on the same scale as achievement, describing where the ZPD peaks
#'   relative to the current achievement. Defaults to 0.022.
#' @param zpd.sd the standard deviation of the ZPD function, controls its width. Defaults to 0.042.
#' @param zpd.scale scaling factor for the ZPD function, controls overall learning rate. Default is 0.055.
#' @param decay.weight scalar parameter, global control of decay rates.
#' @param useGPU should the code be executed on the GPU? May offer performance boost on large simulations. The \code{gpuR}
#' package must be installed. Defaults to FALSE.
#' @export


ZPDGrowthTrajectories <- function(output.format="wide", days, assignment, curriculum.start.points, curriculum.widths, dosage, learning.rates, decay.rates,
                                  initial.achievements, home.environments, integration.points=200,
                                  curriculum.lower.slope=8, curriculum.upper.slope=300, alpha=1, home.curriculum.shape1=1,
                                  home.curriculum.shape2=4.6, zpd.offset=.022, zpd.sd=.042,
                                  zpd.scale=.055, decay.weight=.005, useGPU=FALSE) {


  start.time <- Sys.time()
  message(paste("\nExecution began at ", start.time, sep=''))

  # first call the two functions needed to create the school curriculum

  # school.curr.parms.by.index <- buildSchoolCurriculum(start_point=curriculum.start.points, width=curriculum.widths,
  #                                                     assignment=assignment,
  #                                                     n1=curriculum.lower.slope, n3=curriculum.upper.slope, alpha=alpha)
  #
  # school.curr.fcn.values.by.day <- calculateSchCurricula(assignment=assignment, points=integration.points,
  #                                                        school.curr.parms.by.index=school.curr.parms.by.index)
  #


  school.curr.fcn.values.by.day <- buildSchoolCurriculum(assignment=assignment, start_point=curriculum.start.points,
                                                          width=curriculum.widths, points=integration.points,
                                                          n1=curriculum.lower.slope, n3=curriculum.upper.slope, alpha=alpha)

  # then call the function to create the home curriculum

  home.curr.fcn.values <- buildHomeCurriculum(points=integration.points, shape1=home.curriculum.shape1,
                                              shape2=home.curriculum.shape2)

  message("\nSetup complete.")
  message("\nBeginning growth trajectory simulation...\n")
  # then grow the trajectories

  trajectories <- growTrajectories(days=days, points=integration.points, learn.rate=learning.rates, decay.rate=decay.rates,
                                   initial.ach=initial.achievements, home.env=home.environments, dose=dosage, decay.weight=decay.weight,
                                   school.curr.fcn.values.by.day=school.curr.fcn.values.by.day,
                                   home.curr.fcn.values=home.curr.fcn.values,
                                   zpd.offset=zpd.offset, zpd.sd=zpd.sd, zpd.scale=zpd.scale,
                                   useGPU=useGPU)

  n <- length(learning.rates)
  trajectories <- data.frame(trajectories)
  trajectories <- cbind(seq(1:n), trajectories)
  names(trajectories) <- c("student", paste("day", seq(1:days), sep=""))

  if (output.format=="long") {
    message("\nRestructuring output from wide to long.")

    trajectories <- reshape2::melt(trajectories, measure.vars=c(1:days+1))
    trajectories[,2] <- rep(seq(1:days), each=length(learning.rates))
    names(trajectories) <- c("student", "day", "achievement")
    trajectories <- trajectories[order(trajectories$student),]
  }

  end.time <- Sys.time()

  message(paste("\nExecution finished at ", end.time, sep=''))
  elapsed.time <- end.time-start.time
  time.unit <- units(elapsed.time)

  message(paste("\nThe simulation required ", round((end.time-start.time)[[1]],2), " ", time.unit, ".", sep=''))

  return(trajectories)

}
