#' Function for creating simulated growth trajectories from the theoretical model.
#'
#' \code{ZPDGrowthTrajectories} creates synthetic academic achievement growth trajectories.
#'
#' This function creates synthetic achievement growth trajectories from a quantitative intepretation
#' of Vygotsky theory based on an upcoming publication. The user describes
#' the student characteristics (learning rate, decay rate, initial achievement, and home environment) as
#' well as the home learning environment and the school curriculum. The function will then create a simulated
#' growth trajectory for each student.
#'
#' @param learn.rate Vector of learning rates, one per child. These values specify the global learning rate
#'  for each student. Values should be positive. The lengths of \code{learn.rate}, \code{home.env}, \code{decay.rate},
#'  and \code{initial.ach} indicate the number of students to simulate and must match.
#'
#' @param home.env Vector of home environments, one per child. These values describe how conducive each
#'  student's home environment is for learning. Values should be positive.
#'
#' @param decay.rate Vector of decay rates, one per child. These values describe each student's rate of learning
#'   loss or decay. Values should be positive.
#'
#' @param initial.ach Vector of values describing the initial achievement level for each child. They should be zero
#'   or higher.
#'
#' @param ZPD.width Scalar value describing the radius of the ZPD. Affects the global growth rate. The function
#'   \code{visualizeZPD()} can be used to visualize different choices.
#'
#' @param ZPD.offset Scalar, measured on the same scale as achievement, describing where the ZPD peaks
#'   relative to the current achievement. A positive value means that the ZPD peaks at a higher achievement level that the
#'   student has currently attained. The function \code{visualizeZPD} can be used to select visualize, select, and understand
#'   this value. Positive values are generally appropriate, though the magnitude depends on \code{ZPD.width}.
#'
#' @param home.learning.decay.rate Scalar, the exponential decay parameter for the home learning function. Larger values indicate
#'   more rapid drop-off. Must be greater than 1. The functions \code{visualizHome()} and \code{visualizeContext()} can be used to
#'   visualize, understand, and select appropriate values.
#'
#' @param curriculum.start.points a matrix or list of matrices providing the start points of the school curriculum to
#'   be offered during each grade (or other time division). The matrices should have one column and one row per grade. Each
#'   list entry corresponds to a version of the curriculum, such as 'typical' or 'remedial.' The functions
#'   \code{visualizeSchool()} and \code{visualizeContext()} can be used to visualize, understand, and select appropriate
#'   values.
#'
#' @param curriculum.widths a matrix or list of matrices providing the widths or spans of the school curriculum to
#'   be offered during each grade (or other time division). The matrices should have one column and one row per grade. Each
#'   list entry corresponds to a version of the curriculum, such as 'typical' or 'remedial.' The functions
#'   \code{visualizeSchool()} and \code{visualizeContext()} can be used to visualize, understand, and select appropriate
#'   values.
#'
#' @param curriculum.review.slopes A matrix or list of matrices describing the steepness of the school curriculum cutoff
#'   at the lower range. Conceptually controls the amount of review content. Small numbers indicate a shallower slope and
#'   therefore more review content. As general guidance, values in the range of 10-20 describe heavy review, while 50-100
#'   decribes little review, though this depends on the scale of the curriculum. The functions \code{visualizeSchool()} and
#'   \code{visualizeContext()} can be used to visualize, understand, and select appropriate values.
#'
#' @param curriculum.advanced.slopes a matrix or list of matrices describing the steepness of the school curriculum
#'   cutoff at the upper range. Conceptually controls the amount of advanced content. Small numbers indicate a shallower slope and
#'   therefore more advanced content. As general guidance, values in the range of 10-20 describe heavy review, while 50-100
#'   decribes little review, though this depends on the scale of the curriculum. The functions \code{visualizeSchool()} and
#'   \code{visualizeContext()} can be used to visualize, understand, and select appropriate values.
#'
#' @param assignment A vector of integer values assigning school curricula to time intervals. The length is the number of
#'   time intervals to simulate. Each entry contains a number representing which grade-level curriculum to present. Zero
#'   denotes periods of no school instruction, such as summer breaks. The numbers correspond to the row index of the
#'    \code{curriculum.start.points}, \code{curriculum.widths}, \code{curriculum.review.slopes}, and
#'    \code{curriculum.advanced.slopes} objects.
#'
#' @param dosage Scalar dose parameter, controls mixing of school curriculum and home curriculum during school years,
#'  where 1 denotes all school and 0 denotes all home. Must be [0,1].
#'
#' @param adaptive.curriculum Logical; if there are multiple versions of the school curriculum for each time period,
#'   should they be assigned adaptively? TRUE means that the version that would produce the optimal achievement in
#'   each time period is presented. FALSE means that the curriculum assignment is based on \code{which.curriculum}.
#'   Defaults to FALSE
#'
#' @param which.curriculum Vector of integer values, one for each student, indicating which version of the curriculum is presented
#'   to each student. The values correspond to the list index for the \code{curriculum.start.points}, \code{curriculum.widths},
#'   \code{curriculum.review.slopes}, and \code{curriculum.advanced.slopes} objects. If NULL, all students are assigned
#'   to the first curriculum. Defaults to NULL.
#'
#' @param school.weight A scalar for adjusting the overall growth from school exposure; prevents having to adjust
#'  all \code{learn.rate} values. Must be greater than or equal to 0. Defaults to 1.
#'
#' @param home.weight A scalar for adjusting the overall growth from home; prevents having to adjust all \code{home.env}
#'  values. Must be greater than or equal to 0. Defaults to 1.
#'
#' @param decay.weight A scalar for adjusting the overall rate of decay or forgetting; prevents having to adjust all
#'   \code{decay.rate} values. Defaults to 0.1.
#'
#' @param integration.points Integer number of integration points. Controls tradeoff between accuracy and execution speed.
#'   Defaults to 250.
#'
#' @param threshold A scalar value Used for determining the maximum achievement to be populated in the lookup tables, specified
#'   as a growth rate. The max achievement to be populated is the value at which the growth rate calls below the this number.
#'   Defaults to .00001.
#'
#' @param verbose logical, should status updates be printed to the console? Defaults to TRUE.
#'
#' @param output.format Format of the results, "long" for long format, "wide" for wide. Defaults to "long".
#'
#' @return An object of class \code{ZPD, data.frame}
#'
#' @seealso \code{\link{visualizeContext}}, \code{\link{visualizeZPD}}, \code{\link{visualizeHome}}, and
#'   \code{\link{visualizeSchool}} for plotting the ZPD, home, and school learning context.
#'
#' @seealso \code{\link{visualizeTrajectories}} for plotting the trajectories
#'
#' @seealso \code{\link{describeTrajectories}} for calculating summary statistics
#'
#' @importFrom reshape2 melt
#' @importFrom stats approx
#'
#' @export

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
#'                            verbose=TRUE, output.format="long")
#'
#' describeTrajectories(y, assignment=assignment, byVersion=FALSE)
#' visualizeTrajectories(y)

ZPDGrowthTrajectories <- function(learn.rate, home.env, decay.rate, initial.ach,
                                  ZPD.width, ZPD.offset,
                                  home.learning.decay.rate,
                                  curriculum.start.points, curriculum.widths,
                                  curriculum.review.slopes, curriculum.advanced.slopes,
                                  assignment, dosage,
                                  adaptive.curriculum=FALSE, which.curriculum=NULL,
                                  school.weight, home.weight, decay.weight,
                                  integration.points=250, threshold=.00001,
                                  verbose=TRUE, output.format="long") {

  # rename objects
  slope1 <- curriculum.review.slopes
  slope2 <- curriculum.advanced.slopes
  rate <- home.learning.decay.rate

  start.time <- Sys.time()
  if (verbose==TRUE) {message(paste0("Execution began at ", start.time, "\n"))}

  ## Check for valid inputs ##

  # learn.rate home.env decay.rate  initial.ach: vector or scalar, [0,inf), length in compliance

  # ZPD.width scalar > 0
  # ZPD.offset scalar

  # home.learning.decay.rate scalar > 1. Check for implied max.achievement

  #curriculum.start.points curriculum.widths curriculum.review.slopes curriculum.advanced.slopes
  #  all need to be matrices or lists, all with same dimensions
  #  widths > 0, start.points [0,], slopes > 0, check vs width

  # assignment integer vector, highest value needs to match highest row in above. warning if some
  #  values in above are not used

  #dosage scalar numeric range 0-1, warning at 0,1

  #adaptive.curriculum=FALSE logical. if TRUE there must be >1 version of curriculum

  # which.curriculum=NULL must be vector, same length as assignment, and should match dimensionality
  #  of curriculum objects. warning if not all versions are assigned

  #school.weight home.weight decay.weight must be scalars [0,)

  # integration.points=250 integer, check against max.achievement for coverage, warning if insufficient

  # threshold=.00001 warning if result is below max school curriculum + width

  # verbose=TRUE check logical

  # output.format="long" check "long" or "wide"

  # check for correspondence between adaptive.curriculum and versions of curriculum
  #  (if one version, curriculum.start.points and curriculum.widths are matrices,
  #  if more than one, they are lists with length > 1)

  if ((adaptive.curriculum==TRUE & !is.list(curriculum.start.points)) |
       (adaptive.curriculum==TRUE & !is.list(curriculum.start.points) &
        length(curriculum.start.points)==1)) {
    warning("Only one version of each curriculum was provided, setting adaptive.curriculum to FALSE")
    adaptive.curriculum <- FALSE
  }

  # check for correct type of the student descriptive objects

  if (!is.numeric(learn.rate)) {stop("learn.rate should be a numeric vector")}
  if (!is.numeric(home.env)) {stop("home.env should be a numeric vector")}
  if (!is.numeric(decay.rate)) {stop("decay.rate should be a numeric vector")}
  if (!is.numeric(initial.ach)) {stop("initial.ach should be a numeric vector")}

  #if (!is.numeric(which.curriculum)) {stop("which.curriculum should be a numeric vector")}

  nstudents <- length(learn.rate)

  # if a single value is specified for which.curriculum, recycle it into a vector
  if (!is.null(which.curriculum) & length(which.curriculum)==1) {
    which.curriculum <- rep(which.curriculum, times=nstudents)
  }

  # if adaptive.curriculum is TRUE, then the school lookup table will include, for each level of
  #  achievement, the maximum growth that could occur over all the curricula. So there will
  #  be only one school lookup table (in slot [[1]]) containing this consolidation of
  #  values

  if (adaptive.curriculum==TRUE) {which.curriculum <- rep(1, times=nstudents)}


  # check for consistent size in the student descriptive objects
  if (min(c(length(learn.rate), length(home.env), length(decay.rate), length(initial.ach))) !=
      max(c(length(learn.rate), length(home.env), length(decay.rate), length(initial.ach)))) {
    stop("The length of learn.rate, home.env, decay.rate, and initial.ach must be identical")}

  # the number of time intervals is the length of the assignent vector
  times <- length(assignment)


  students <- students.to.list(learn.rate=learn.rate, home.env=home.env, decay.rate=decay.rate,
                               initial.ach=initial.ach, which.curriculum=which.curriculum)


  # the number of students is the length of the learn.rate vector
  nstudents <- length(learn.rate)  ### check this now that students is

  # AY is an indicator of whether it is summer or not (AY = "academic year")
  #   this is used to replace a slow if() with fast multiplication
  AY <- as.numeric(assignment > 0)


  # if curriculum.start.points, curriculum.widths, slope1, or slope2 are matrices, make them lists
  if (is.matrix(curriculum.start.points)) {
    warning("curriculum.start.points was specified as a matrix; it will coerced to a list")
    old.curriculum.start.points <- curriculum.start.points
    curriculum.start.points <- list()
    curriculum.start.points[[1]] <- old.curriculum.start.points
  }

  if (is.matrix(curriculum.widths)) {
    warning("curriculum.widths was specified as a matrix; it will coerced to a list")
    old.curriculum.widths <- curriculum.widths
    curriculum.widths <- list()
    curriculum.widths[[1]] <- old.curriculum.widths
  }

  if (is.matrix(slope1)) {
    warning("slope1 was specified as a matrix; it will coerced to a list")
    old.slope1 <- slope1
    slope1 <- list()
    slope1[[1]] <- old.slope1
  }

  if (is.matrix(slope2)) {
    warning("slope2 was specified as a matrix; it will coerced to a list")
    old.slope2 <- slope2
    slope2 <- list()
    slope2[[1]] <- old.slope2
  }


  ##############################################################
  ##### Build the lookup tables for school and home growth #####
  ##############################################################

  ### Build the school lookup table ###

  # initialize school lookup table as empty list
  school.lookup.table <- list()

  # determine the max achievement; this is how high we need to populate the lookup
  #   tables. It is the highest growth a person can reach given the specification of
  #   the school and home curriculum

  maxachievement <- max.achievement(ZPD.width=ZPD.width, ZPD.offset=ZPD.offset,
                                    curriculum.start.points=curriculum.start.points,
                                    curriculum.widths=curriculum.widths,
                                    slope2=slope2, rate=rate, threshold=threshold,
                                    integration.points=integration.points)

  for (i in 1:length(curriculum.start.points)) {
    school.lookup.table[[i]] <- build.school.lookup(integration.points=integration.points,
                                                    ZPD.width=ZPD.width,
                                                    ZPD.offset=ZPD.offset,
                                                    curriculum.start.points=curriculum.start.points[[i]],
                                                    curriculum.widths=curriculum.widths[[i]],
                                                    slope1=slope1[[i]], slope2=slope2[[i]],
                                                    maxachievement=maxachievement)
  }


  if (adaptive.curriculum==TRUE) {
    # build a lookup table based on the max growth possible for each
    #  level of achievement.

    # convert the list to an array, where the third dimension is the version of the curriculum
    #  (which was formerly indexed by list item [[j]]]
    school.lookup.table <- simplify2array(school.lookup.table)

    # change the order of the dimensions in the array so that the curriculum version
    #  becomes the first dimension, row (e.g, achievement point) is the second,
    #  and column (e.g, grade level) is the third
    school.lookup.table <- aperm(school.lookup.table, c(3, 1, 2))

    # find the maximum growth given the achievement level for all the versions of the curriculum
    #   within each grade
    # we end up with a matrix with nrow=points and ncol= # of grades + 1
    max.school.lookup <- cbind(
      # first col should be the achievement values
      school.lookup.table[1, , 1],
      # remaining cols are the max for each grade across curricula
      #apply(school.lookup.table[, , 2:3], c(1,2), max)
      t(apply(school.lookup.table[, , 2:3], c(1,2), max))
    )

    # which curriculum gives the max growth, given the achievement level?
    #  note: if all are zero, defaults to the first curriculum, which should be the "baseline"
    #  or "grade-level"
    which.school.lookup <- cbind(
      # first col should be the achievement values
      school.lookup.table[1, , 1],
      # remaining cols are the max for each grade across curricula
      #apply(school.lookup.table[, , 2:3], c(1,2), which.max)
      t(apply(school.lookup.table[, , 2:3], c(1,2), which.max))
    )

    rm(school.lookup.table)
    school.lookup.table <- list()
    school.lookup.table[[1]] <- max.school.lookup

  }

  # Build the home lookup table
  home.lookup.table <- build.home.lookup(integration.points=integration.points,
                                                                ZPD.width=ZPD.width, ZPD.offset=ZPD.offset,
                                                                rate=rate, maxachievement=maxachievement)


  if (verbose==TRUE) {
    message("Lookup table calculations complete.")
    message(paste0("Lookup table evaluated at ", integration.points, " integration points with max achievement of ", round(maxachievement, 3)))
    message("Beginning calculations.")
  }



  # call the grow.trajectories function to loop over time intervals
  achievement <- list()

  for (i in 1:max(which.curriculum)) {
    achievement[[i]] <-  cbind(students[[i]],
                               grow.trajectories(students=students[[i]], times=times, which.curriculum=i,
                                                 assignment=assignment, school.weight=school.weight, home.weight=home.weight,
                                                 decay.weight=decay.weight, dosage=dosage,
                                                 school.lookup.table=school.lookup.table, home.lookup.table=home.lookup.table)
    )
  }

  # convert output to data frame
  achievement <- data.frame(do.call(rbind, achievement))

  if (verbose==T) {message("Finished calculations.")}



  # append student id as the first column
  achievement <- cbind(seq(1:length(learn.rate)), achievement)

  # name the columns
  names(achievement) <- c("id", "learn.rate", "home.env", "decay.rate", "initial.ach",
                          "curriculum", paste("time", seq(1:times), sep=""))

  if (output.format=="long") {

    if (verbose==TRUE) {message("Restructuring output from wide to long.")}

    achievement <- reshape2::melt(achievement, id.vars=1:6)
    achievement[,7] <- rep(seq(1:times), each=length(learn.rate))
    names(achievement) <- c("id", "learn.rate", "home.env", "decay.rate", "initial.ach",
                            "curriculum", "time", "achievement")
    achievement <- achievement[order(achievement$id),]

    achievement$assignment <- rep(assignment, times=length(learn.rate))

    if (adaptive.curriculum==TRUE) {
      for (i in 1:max(assignment)) {
        achievement$curriculum[achievement$assignment==i] <- round(stats::approx(x=which.school.lookup[,1], y=which.school.lookup[,1+i],
                                                                          xout=achievement$achievement[achievement$assignment==i])$y, 0)
      }

      achievement$curriculum[achievement$assignment==0] <- NA

    }
  }


  if (verbose==TRUE) {
    end.time <- Sys.time()
    message(paste0("\nExecution finished at ", end.time))
    elapsed.time <- end.time-start.time
    time.unit <- units(elapsed.time)

    message(paste0("\nExecution required ", round((end.time-start.time)[[1]],2), " ", time.unit, "."))
  }

  class(achievement) = c("ZPD", "data.frame")
  return(achievement)

}
