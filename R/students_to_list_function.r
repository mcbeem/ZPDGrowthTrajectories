#' Function for converting the student characteristics into a list in which the
#'  first list element is a matrix as expected by the ZPDGrowthTrajectories function
#'  called by ZPDGrowthTrajectories() function
#'
#' @param learn.rate vector of learning rates, one for each student
#' @param home.env vector of home environments, one for each student
#' @param decay.rate vector of decay rates, one for each student
#' @param initial.ach vector of achievement values, one for each student
#' @param which.curriculum a vector of values indicating which version of the school curriculum should be presented
#'  during interval t. The numbers index list elements of the curriculum.start.points and curriculum.widths objects
#' @examples
#' \dontrun{
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
#' students.to.list(learn.rate=learn.rate, home.env=home.env, decay.rate=decay.rate,
#'                 initial.ach=initial.ach,
#'                 which.curriculum=c(rep(1, times=length(learn.rate)/2),
#'                 rep(2, times=length(learn.rate)/2)))
#' }

students.to.list <- function(learn.rate, home.env, decay.rate, initial.ach,
                             which.curriculum) {

  students <- list()

  for (i in 1:max(which.curriculum)) {
    students[[i]] <- cbind(
      learn.rate[which.curriculum==i],
      home.env[which.curriculum==i],
      decay.rate[which.curriculum==i],
      initial.ach[which.curriculum==i],
      which.curriculum[which.curriculum==i]
    )
  }

  return(students)

}
