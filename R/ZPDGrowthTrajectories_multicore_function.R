#' Multicore function for creating simulated growth trajectories from the theoretical model.
#'
#' This function runs the \code{ZPDGrowthTrajectories} function on multiple CPUs simultaneously
#' to speed up execution. 
#'
#' @param n.cores The number of CPUs to use. The number available can be seen by running \code{detectCores()}.
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
#' @param verbose shoud status updates be printed to the console? Defaults to TRUE.
#' @export

ZPDGrowthTrajectories_multicore <- function(n.cores, output.format="wide", days, assignment, curriculum.start.points, curriculum.widths, dosage, learning.rates, decay.rates,
                                             initial.achievements, home.environments, integration.points=200,
                                             curriculum.lower.slope=8, curriculum.upper.slope=300, alpha=1, home.curriculum.shape1=1,
                                             home.curriculum.shape2=4.6, zpd.offset=.022, zpd.sd=.042,
                                             zpd.scale=.055, decay.weight=.005, useGPU=FALSE, verbose=TRUE) {

  students <- cbind(learning.rates, decay.rates, initial.achievements, home.environments)
  
  if (verbose==TRUE) {
    start.time <- Sys.time()
    message(paste("\nExecution began at ", start.time, sep=''))
  }
  
  # divide sample into n.cores list elements. give the last list element the remainder if 
  #  the number of students does not divide evenly into the number of cores
  student.list <- list()
  ns <- floor(length(learning.rates)/n.cores)
  
  if (n.cores > 1) {
    for (i in 1:(n.cores-1)) {
      student.list[[i]] <- students[(ns*(i-1)+1):(ns*i),]
    }
  }
  
  student.list[[n.cores]] <- students[(ns*(n.cores-1)+1):nrow(students),]
  
  ####  Begin multiprocessing ###
  
  # Initiate clusters
  cl <- parallel::makeCluster(n.cores)
  
  trajectories.list <- parallel::parLapply(cl=cl, X=student.list, 
                                          fun=ZPDGrowthTrajectories::ZPDGrowthTrajectories_studentmatrix, 
                                          output.format="long", days=days, assignment=assignment, 
                                          curriculum.start.points=curriculum.start.points,
                                          curriculum.widths=curriculum.widths,
                                          dosage=dosage, integration.points=integration.points,
                                          curriculum.lower.slope=curriculum.lower.slope, 
                                          curriculum.upper.slope=curriculum.upper.slope, alpha=alpha, 
                                          home.curriculum.shape1=home.curriculum.shape1,
                                          home.curriculum.shape2=home.curriculum.shape2, 
                                          zpd.offset=zpd.offset, zpd.sd=zpd.sd,
                                          zpd.scale=zpd.scale, decay.weight=decay.weight, 
                                          useGPU=useGPU, verbose=FALSE)
  
  
  parallel::stopCluster(cl)
  
  
  # recombine list results into a matrix
  trajectories <- do.call(rbind, trajectories.list)
  
  if (verbose==TRUE) {
    end.time <- Sys.time()
    message(paste("\nExecution finished at ", end.time, sep=''))
    elapsed.time <- end.time-start.time
    time.unit <- units(elapsed.time)
    
    message(paste("\nThe simulation required ", round((end.time-start.time)[[1]],2), " ", time.unit, ".", sep=''))
  }
  
  return(trajectories)
}
