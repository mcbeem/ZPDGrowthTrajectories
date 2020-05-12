#' Function for looping over all the time periods to calculate achievement trajectories
#' called by ZPDGrowthTrajectories()
#'
#' @param times how many time intervals to compute; the first n values of the assignment vector
#' @param assignment vector indicating which school curriculum will be presented during each time period,
#'  where the numbers refer to columns of the curriculum.start.points matrix and zeros represent times
#'  of no school instruction (e.g., summmer)
#' @param students a four-column matrix of student characteristics, where the first column contains learn.rate,
#'   the second home.env, the third decay.rate, and the fourth achievement. There is one row per student.
#' @param which.curriculum a vector of values indicating which version of the school curriculum should be presented
#'  during interval t. The numbers index list elements of the curriculum.start.points and curriculum.widths objects
#' @param decay.weight a global scalar for adjusting the rate of forgetting; prevents having to adjust all decay.rate values
#' @param school.weight a global scalar for adjusting the growth from school exposure; prevents having to adjust
#'  all learn.rate values
#' @param home.weight a global scalar for adjusting the growth from home; prevents having to adjust all home.env values
#' @param dosage scalar ranging from 0 to 1 indicating the ratio of school / home exposure during time
#'  intervals where school curriculum is presented (e.g., during academic years)
#' @param school.lookup.table the school growth lookup table produced by the build_school_lookup() function
#' @param home.lookup.table the home growth lookup table produced by the build_home_lookup() funtion

#'
#' @examples
#' \dontrun{
#' curriculum.start.points <- matrix(c(.1, .2, .3), ncol=1)
#'
#' curriculum.widths <- matrx(rep(.11, 3), ncol=1)
#'
#' school.lookup.table <- build.school.lookup(integration.points=2000, ZPD.width=.05,
#'                                            ZPD.offset=.02,
#'                                            curriculum.start.points=curriculum.start.points,
#'                                            curriculum.widths=curriculum.widths,
#'                                            slope1=10, slope2=30, maxachievement=1.5)
#'
#' home.lookup.table <- build.home.lookup(integration.points=2000, ZPD.width=.05,
#'                                        ZPD.offset=.02, rate=4, maxachievement=1.5)
#'
#' students <- matrix(c(.05, .04, .03,
#'                    .15, .2, .08,
#'                    .001, .002, .0015,
#'                    rep(.15, 3)), ncol=4)
#'
#' grow.trajectories(times=10, students=students, which.curriculum=1,
#'                   assignment=rep(1, 10), decay.weight=.25, school.weight=1, home.weight=1,
#'                   dosage=.8, school.lookup.table=list(school.lookup.table),
#'                   home.lookup.table=home.lookup.table)
#' }

grow.trajectories <- function(times, assignment, students, which.curriculum,
                              decay.weight, school.weight, home.weight,
                              dosage, school.lookup.table, home.lookup.table) {

  nstudents <- nrow(students)

  # create achievement matrix, populated initially with NAs

  achievement <- matrix(
    c(students[,4], rep(NA, nstudents*(times-1))),
    nrow=nstudents, ncol=times, byrow=F)

  for (t in 2:times) {

    achievement[,t] <-  update.achievement(learn.rate=students[,1], home.env=students[,2],
                                           decay.rate=students[,3],
                                           achievement=achievement[, t-1],
                                           which.curriculum=which.curriculum,
                                           assignment=assignment[t], decay.weight=decay.weight,
                                           school.weight=school.weight, home.weight=home.weight,
                                           dosage=dosage,
                                           school.lookup.table=school.lookup.table,
                                           home.lookup.table=home.lookup.table)

  }

  return(achievement)
}
