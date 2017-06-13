#'  A helper function for describing the school curriculum.
#'
#'  \code{buildSchoolCurriculum} creates the school curriculum object expected by \code{ZPDGrowthTrajectories}.
#'
#'  This function takes the matrix of start points, widths, and assignment,
#'  as well as the control parameters min, max, n1, n3, and alpha, and
#'  creates a list mapping the parameters of the trapezoidal probability density function
#'  to each grade level ("index"). Index=0 refers to periods of no school (e.g., summers).
#'  It is an internal function and not intended for stand-alone use.
#'
#' @param start_point a matrix providing the start points of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param width a matrix providing the spans of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param assignment a vector. The length is the number of days to simulate. Each entry contains a number representing which
#'   grade-level curriculum to present. Zero denotes summers. The numbers correspond to the row index of the
#'    \code{start_point} and \code{width} objects.
#' @param points number of integration points. Controls tradeoff between accuracy and execution speed.
#'   Defaults to 200.
#' @param n1 scalar argument to the \code{n1} argument of \code{trapedoid::dtrapezoid}; controls
#'   steepness of the school curriculum cutoff at the lower range. Conceptually controls the amount of review content.
#' @param n3 scalar argument to the \code{n3} argument of \code{trapedoid::dtrapezoid}; controls
#'   steepness of the school curriculum cutoff at the upper range. Conceptually controls the amount of advanced content.
#' @param alpha scalar argument to the \code{alpha} argument of \code{trapedoid::dtrapezoid}. Defaults to 1.
#' @export


buildSchoolCurriculum <- function(start_point, width, assignment, points, n1, n3, alpha) {

  # initialize list objects
  curr.parms <- list()
  school.curr.parms.by.day <- list()

  # the number of school years is the highest value assignment vector
  school.years <- max(assignment)
  num.curricula <- ncol(start_point)
  
  # convert the assignment object to a data frame
  days <- length(assignment)
  assignment <- data.frame(matrix(assignment, nrow=length(assignment)))
  names(assignment) <- "index"
  assignment$day <- 1:days

  # begin loop to populate the parameters of dtrapezoid to describe the curriculum
  # (the number of columns in start_point tells how many curricula there are (e.g., typical, remedial, advanced))
  
  if (school.years > 0) {  # only do this if there's at least one school year
  
    for (i in 1:num.curricula) {
  
      curr.parms[[i]] <- matrix(c(
        #order: index min mode1 mode2 max n1 n3 alpha
  
        seq(1:school.years),                 # index parameter
        rep(0, times=school.years),          # min parameter
        start_point[,i],                    # mode1 parameter; where curriculum starts; comes from start_point
        start_point[,i]+width[,i],         # mode2 parameter; where curriculum ends; comes from start_point plus width
        rep(1, times=school.years),          # max parameter
        rep(n1, times=school.years),          # n1 parameter
        rep(n3, times=school.years),        # n3 parameter
        rep(alpha, times=school.years)),          # alpha parameter
        nrow=school.years, ncol=8, byrow=FALSE)
  
      # append row for summer with index=0
  
      curr.parms[[i]] <- rbind(curr.parms[[i]], c(0, -2, min(start_point[,i])-2, min(start_point[,i])-1,
                                                  -1, n1, n3, alpha))
    
      # convert to to data frames and give names
      curr.parms[[i]] <- data.frame(curr.parms[[i]])
      names(curr.parms[[i]]) <- c("index", "min", "mode1", "mode2", "max", "n1", "n3", "alpha")

    }
  } else {
    curr.parms[[1]] <- matrix(c(0, -2, min(start_point[,1])-2, min(start_point[,1])-1,
                                            -1, n1, n3, alpha), nrow=1)
    # convert to to data frames and give names
    curr.parms[[1]] <- data.frame(curr.parms[[1]])
    names(curr.parms[[1]]) <- c("index", "min", "mode1", "mode2", "max", "n1", "n3", "alpha")
  }

  x <- seq(0, 1, length.out=points)

  values <- list()
  school.curr.fcn.values.by.day <- list()

  # this code is a kludge. there *has* to be a better way to do this. alas.

  # i indexes the curricula, each stored as a separate matrix
  for (i in 1:num.curricula) {

    # initialize the values matrix for each list member (which is a curriculum, i.e. typical, remedial, advanced)
    values[[i]] <- matrix(nrow=nrow(curr.parms[[1]]), ncol=points)

    # for each curriculum and grade level, calculate the value of the school curriculum function at each integration point
    for (j in 1:nrow(curr.parms[[i]])) {
      y <-  mapply(trapezoid::dtrapezoid, x, MoreArgs=curr.parms[[i]][j, 2:8])
      ifelse(curr.parms[[i]]$index[j] == 0,
             values[[i]][j,] <- rep(0, times=points),  # e.g., do this if the index is zero (for summers)
             values[[i]][j,] <- y / max(y))  # do this if the index is nonzero (for academic years)
      # this line calculates the normalized value of the school curriculum function (with max=1)
    }
    # adds index (e.g., grade) as the first column
    values[[i]] <- cbind(curr.parms[[i]]$index, values[[i]])
    # convent to data frame and make variable names
    values[[i]] <- data.frame(values[[i]])
    names(values[[i]]) <- c("index", paste("x_is_", round(x,4), sep=''))

    # merge the school curriculum function values for each location of x with the 'assignment' object, which
    #   is the crosswalk from day to what grade level curriculum (if any) is presented.
    school.curr.fcn.values.by.day[[i]] <- merge(x=assignment, y=values[[i]], by="index")

    # sort the resulting object by day
    school.curr.fcn.values.by.day[[i]] <- school.curr.fcn.values.by.day[[i]][order(school.curr.fcn.values.by.day[[i]]$day),]
    # transform back to a matrix
    school.curr.fcn.values.by.day[[i]] <- as.matrix(school.curr.fcn.values.by.day[[i]])
  }

  return(school.curr.fcn.values.by.day)
}
