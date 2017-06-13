#' Function for creating simulated growth trajectories from the theoretical model.
#'
#' An alternative version of the \code{ZPDGrowthTrajectories} function that accepts a matrix of student
#' characteristics rather than four separate vectors for learning rate, decay rate, initial achievement,
#' and home environment. Calls \code{ZPDGrowthTrajectories}.
#'
#' @param output.format Format of the results, "long" for long format, "wide" for wide. Defaults to "wide".
#' @param days the number of days to simulate.
#' @param assignment a vector. The length is the number of days to simulate. Each entry contains a number representing which
#'   grade-level curriculum to present. Zero denotes summers. The numbers correspond to the row index of the
#'    \code{curriculum.start.points} and \code{width} objects.
#' @param students a matrix with four columns describing the student characteristics. The first column is a vector
#'   learning rates, the second column is a vector of decay rates, the third column is a vector of initial achievement 
#'   values, and the fourth column is a vector of home environments. The number of rows of this object controls the 
#'   number of students to simulate.     
#' @param curriculum.start.points a matrix providing the start points of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param curriculum.widths a matrix providing the spans of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param dosage scalar dose parameter, controls mixing of school curriculum and home curriculum during school years.
#'   Must be [0,1].
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

ZPDGrowthTrajectories_studentmatrix <- function(output.format="wide", days, assignment, students, 
                  curriculum.start.points, curriculum.widths, dosage, integration.points=200,
                  curriculum.lower.slope=8, curriculum.upper.slope=300, alpha=1, home.curriculum.shape1=1,
                  home.curriculum.shape2=4.6, zpd.offset=.022, zpd.sd=.042,
                  zpd.scale=.055, decay.weight=.005, useGPU=FALSE, verbose=FALSE) {
  
  return(ZPDGrowthTrajectories::ZPDGrowthTrajectories(output.format=output.format, 
   days=days, assignment=assignment,
   curriculum.start.points=curriculum.start.points,
   curriculum.widths=curriculum.widths,
   dosage=dosage, learning.rates=students[,1], decay.rates=students[,2], 
   initial.achievements=students[,3],
   home.environments=students[,4], integration.points=integration.points,
   curriculum.lower.slope=curriculum.lower.slope, 
   curriculum.upper.slope=curriculum.upper.slope, alpha=alpha, 
   home.curriculum.shape1=home.curriculum.shape1,
   home.curriculum.shape2=home.curriculum.shape2, 
   zpd.offset=zpd.offset, zpd.sd=zpd.sd,
   zpd.scale=zpd.scale, decay.weight=decay.weight, 
   useGPU=useGPU, verbose=verbose))
}

