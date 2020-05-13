#' Function for creating simulated growth trajectories from the theoretical model.
#'
#' \code{ZPDGrowthTrajectories} creates synthetic academic achievement growth trajectories.
#'
#' This function creates synthetic achievement growth trajectories from a quantitative intepretation
#' of Vygotsky theory based on an upcoming publication. The user describes
#' the student characteristics (learning rate, decay rate, initial achievement, and home environment) as
#' well as the home 'curriculum' and the school curriculum. The function will then create a simulated
#' growth trajectory for each student.
#'
#' @param learn.rate vector of learning rates, one per child.
#' @param decay.rate vector of decay rates, one per child. Controls forgetting.
#' @param initial.ach vector of initial achievements, one per child.
#' @param home.env vector of home environments, one per child.
#' @param school.weight a global scalar for adjusting the growth from school exposure; prevents having to adjust
#'  all learn.rate values
#' @param home.weight a global scalar for adjusting the growth from home; prevents having to adjust all home.env values
#' @param decay.weight a global scalar for adjusting the rate of forgetting; prevents having to adjust all decay.rate values
#' @param dosage scalar dose parameter, controls mixing of school curriculum and home curriculum during school years.
#'   Must be [0,1].
#' @param assignment a vector. The length is the number of time intervals to simulate. Each entry contains a number representing which
#'   grade-level curriculum to present. Zero denotes summers. The numbers correspond to the row index of the
#'    \code{curriculum.start.points} and \code{curriculum.widths} objects.
#' @param adaptive.curriculum logical; if there are multiple versions of the school curriculum for each time period,
#'   should they be assigned adaptively? TRUE means that the version that would produce the optimal achievement in
#'   each time period is presented. FALSE means that the curriculum assignment is based on which.curriculum.
#'   Defaults to FALSE
#' @param which.curriculum Defaults to NULL
#' @param integration.points Integer number of integration points. Controls tradeoff between accuracy and execution speed.
#'   Defaults to 250.
#' @param threshold Used for determining the maximum achievement to be populated in the lookup tables, specified
#'   as a growth rate. Defaults to .00001
#' @param slope1 a matrix or list of matrices describing the steepness of the school curriculum cutoff at the lower range. Conceptually controls the amount of review content.
#' @param slope2 a matrix or list of matrices describing the slope of the school curriculum at the upper range. Conceptually controls the amount of advanced content.
#' @param rate Scalar, the exponential decay parameter describing the home curriculum function.
#' @param ZPD.offset scalar value, measured on the same scale as achievement, describing where the ZPD peaks
#'   relative to the current achievement.
#' @param curriculum.start.points a matrix or list of matrices providing the start points of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param curriculum.widths a matrix or list of matrices providing the spans of each grade level (rows) and version (columns) of the
#'   school curriculum.
#' @param ZPD.width the radius of the ZPD.
#' @param verbose logical, should status updates be printed to the console? Defaults to TRUE.
#' @param output.format Format of the results, "long" for long format, "wide" for wide. Defaults to "long".
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
#' slope1 <- list(
#'   # "typical curriculum" review slopes for K and first grade
#'   matrix(c(15, 15), nrow=2, ncol=1),
#'   # "advanced curriculum" review slopes for K and first grade
#'   matrix(c(30, 30), nrow=2, ncol=1)
#' )
#'
#' slope2 <- list(
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
#'                            school.weight=.5, home.weight=1, decay.weight=.05,
#'                            dosage=.8, assignment=assignment,
#'                            which.curriculum=which.curriculum,
#'                            adaptive.curriculum=FALSE,
#'                            slope1=slope1, slope2=slope2, rate=6,
#'                            ZPD.width=.05, ZPD.offset=.02,
#'                            curriculum.start.points=curriculum.start.points,
#'                            curriculum.widths=curriculum.widths,
#'                            verbose=TRUE, output.format="long")
#'
#' describeTrajectories(y, assignment=assignment, byCurriculum=FALSE)
#' visualizeTrajectories(y)

ZPDGrowthTrajectories <- function(learn.rate, home.env, decay.rate, initial.ach,
                                  school.weight, home.weight, decay.weight, dosage,
                                  assignment,
                                  adaptive.curriculum=FALSE, which.curriculum=NULL,
                                  ZPD.width, ZPD.offset,
                                  curriculum.start.points, curriculum.widths,
                                  slope1, slope2, rate=6,
                                  integration.points=250, threshold=.00001,
                                  verbose=TRUE, output.format="long") {

  start.time <- Sys.time()
  if (verbose==TRUE) {message(paste0("Execution began at ", start.time, "\n"))}

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
