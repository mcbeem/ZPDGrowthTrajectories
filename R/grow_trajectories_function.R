#' Function for creating growth trajectories.
#'
#' \code{growTrajectories} calculates achievement at each day.
#'
#' This function is called by \code{ZPDGrowthTrajectories} to iteratively calculate achievement for each day.
#'
#' @param days the number of days to simulate.
#' @param points the number of integration points.
#' @param learn.rate vector of learning rates, one per child.
#' @param decay.rate vector of decay rates, one per child. Controls forgetting.
#' @param initial.ach vector of initial achievements, one per child.
#' @param home.env vector of home environments, one per child.
#' @param dose scalar dose parameter, controls mixing of school curriculum and home curriculum during school years. Must be [0,1].
#' @param decay.weight scalar parameter, global control of decay rates.
#' @param school.curr.fcn.values.by.day object produced by the \code{buildSchoolCurriculum} function.
#' @param home.curr.fcn.values object produced by the \code{buildHomeCurriculum} function.
#' @param zpd.offset scalar value, measured on the same scale as achievement, describing where the ZPD peaks
#' relative to the current achievement.
#' @param zpd.sd the standard deviation of the ZPD function, controls its width.
#' @param zpd.scale scaling factor for the ZPD function, controls overall learning rate.
#' @param useGPU should the code be executed on the GPU? May offer performance boost on large simulations. The \code{gpuR}
#' package must be installed.
#' @param verbose shoud status updates be printed to the console? defaults to TRUE.
#' @export


growTrajectories <- function(days, points, learn.rate, decay.rate, initial.ach, home.env, dose, decay.weight,
                             school.curr.fcn.values.by.day, home.curr.fcn.values, zpd.offset, zpd.sd, zpd.scale, #zpd.percentile, zpd.df
                             useGPU=FALSE, verbose=TRUE) {

  if ((!requireNamespace("gpuR" & useGPU==TRUE), quietly=TRUE)) {
    message("Package gpuR must be installed for useGPU=TRUE, setting useGPU to FALSE and continuing...\n")
    useGPU=FALSE
  }

  # transpose
  home.curr.fcn.values <- matrix(home.curr.fcn.values, nrow=points, ncol=1)
  school.curr.fcn.values.by.day <- lapply(school.curr.fcn.values.by.day, "t")

  # if using the GPU, load the school curr function values by day matrix into it ONE TIME! sheesh!
  # unfortunately, lists are not available. have to unlist() the object. It's now got num.curricula \times the number of cols
  if (useGPU == TRUE) {
    school.curr.fcn.values.by.day_g <- gpuR::vclMatrix(matrix(unlist(school.curr.fcn.values.by.day),nrow=points+2)[3:(points+2),], type="float")
    home.curr.fcn.values_g <-  gpuR::vclMatrix(home.curr.fcn.values, type="float")
  }

  # x is the vector of integration points spanning the range of achievement that is of interest
  x <- seq(0, 1, length.out=points)

  # calculate the number of curricula
  num.curricula <- length(school.curr.fcn.values.by.day)

  # calculate the number of students
  n <- length(learn.rate)

  # initialize the 'achievement' object to hold the result
  achievement <- matrix(nrow=n, ncol=days)
  achievement[,1] <-  initial.ach

  # zpd_offset controls how far the peak of the zpd lies from the current achievement
  #zpd.offset <- qnorm(zpd.percentile, 0, zpd.sd)

  # loop over students allowing for GPU performance boost (hopefully!)

  for (t in 2:days) {

    # compute updated ZPD

    # zpd is a matrix of the zpd function evaluated for each child (row) at each integration point (x, col)
    # it has dimensions (n x points)
    #
    #
    # zpd <- matrix(dt.scaled(rep(x, times=n), df=zpd.df, mean=rep(achievement[,t-1]+zpd.offset, each=points), sd=zpd.sd),
    #               nrow=n, ncol=points, byrow=T)

    zpd <- matrix(dnorm(rep(x, times=n), mean=rep(achievement[,t-1]+zpd.offset, each=points), sd=zpd.sd),
                  nrow=n, ncol=points, byrow=T)

    # normalize the zpd function. first calculate the max of each row. Then divide every value by the max
    max.zpd <- apply(zpd, 1, max)
    zpd <- (zpd/max.zpd)*zpd.scale

    # initialize a matrix to hold the unweighted growth during time t for each student in each curriculum
    # one row per student, one column per curriculum
    # actual growth will be proportional to the max value per row
    # i.e., each student "officially" gets the most beneficial curriculum for each unit of time
    unweighted.sch.growth.by.curr <- matrix(nrow=n, ncol=num.curricula)

    # CPU execution

    if (useGPU == FALSE) {
      home_learn <- as.vector((zpd %*% home.curr.fcn.values) * home.env * learn.rate * (1/points))

      for (i in 1:num.curricula) {

        if (school.curr.fcn.values.by.day[[i]][1,t]==0) {

          unweighted.sch.growth.by.curr[,i] <- rep(0, times=n)} else {
            unweighted.sch.growth.by.curr[,i] <- zpd %*% school.curr.fcn.values.by.day[[i]][3:(points+2), t-1]}
      }
    }

    # GPU execution
    if (useGPU == TRUE) {
      zpd_g <- gpuR::vclMatrix(zpd, type="float")
      home_learn <- as.vector(as.matrix((zpd_g %*% home.curr.fcn.values_g))) * home.env * learn.rate * (1/points)

      for (i in 1:num.curricula) {

        current.col <- as.integer(((i-1)*days)+t)

        if (school.curr.fcn.values.by.day[[i]][1,t]==0) {
          unweighted.sch.growth.by.curr[,i] <- rep(0, times=n)} else {
            unweighted.sch.growth.by.curr[,i] <-   as.vector(as.matrix(zpd_g %*% gpuR::block(school.curr.fcn.values.by.day_g,
                                    rowStart=1L, rowEnd=as.integer(points), colStart=current.col, colEnd=current.col)))}
      }
    }

    school_learn <- as.vector(apply(unweighted.sch.growth.by.curr, 1, max))* learn.rate * (1/points)

    if (school.curr.fcn.values.by.day[[1]][1,t]==0) {  # this happens during summers, the index value is equal to zero
      achievement[,t] <- (achievement[,t-1] + home_learn) * (1 - (decay.rate*decay.weight))} else {
        achievement[,t] <- (achievement[,t-1] + (dose*school_learn) + ((1-dose)*home_learn)) * (1 - (decay.rate*decay.weight))}

    # print a progress message to the console every 100 time points
  if (verbose==TRUE) {
    if (t %% 100 == 0) {message(paste("Finished computing ", t, " time points.", sep=""))}
    }
  }

  if (useGPU==TRUE) {
    rm(zpd_g)
    rm(home.curr.fcn.values_g)
    rm(school.curr.fcn.values.by.day_g)
    gc(verbose=FALSE)
  }

  return(achievement)

}
