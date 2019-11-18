#' Function for restructuring matrices from wide to long.
#'
#' \code{matrix.melt()} restructures matrices from wide to long. 
#' 
#' This function restructures matrices. It works similarly to reshape2::melt but does not coerce
#'   matrices to data frames. Used interally by \code{ZPDGrowthTrajectories()} and not suited for general
#'   use. 
#'   
#'   This code assumes that the first column contains student IDs. These are dropped.
#'
#' @param matrix Matrix to be transposed.
#' @export

matrix.melt <- function(matrix) {
 
 # matrix <- matrix[,2:ncol(matrix)]
  values <- as.vector(apply(matrix, 1, t))
  students <- rep((1:nrow(matrix)), each=ncol(matrix))
  days <- rep(1:ncol(matrix), times=nrow(matrix))
  
  long.matrix <- matrix(c(students, days, values), ncol=3)
  return(long.matrix)
}
