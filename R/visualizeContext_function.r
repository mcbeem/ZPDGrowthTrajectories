#' Function for visualizing the learning context, including the school and (optionally) home.
#'
#' \code{visualizeContext()} is a function for visualizing the learning context that is specified
#'   by the relevant arguments to \code{ZPDGrowthTrajectories()}.
#'
#' The function produces a figure in which time is represented on the x-axis, achievement on the y-axis,
#'   and the intensity of learning opportunities is represented by the coloration of the background, where
#'   darker / more opaque means more rapid learning. The home learning context is represented if a value is supplied
#'   to the \code{home.learning.decay.rate} argument, otherwise the figure only shows the school learning
#'   context. If there are multiple versions of the curriculum, they are displayed as facets.
#'   In essence, this figure describes the vector field that is produced by the home and school learning
#'   contexts, where the coloration or opacity of the background describes the magnitude of the vertical component
#'   at each location. Individual student growth depends not only on the location in the field, but also on the
#'   student's learning rate, home environment, decay rate, as well as the global parameters describing the
#'   size and offset of the ZPD as well as the global weights for school learning, home learning, and decay.
#'
#' @param home.learning.decay.rate Scalar, An optional value specifying the exponential decay parameter for the
#'   home learning function. If provided, the background will be shaded in proportion to the intensity of
#'   home learning. Larger values indicate more rapid drop-off. Must be greater than 1. If omitted, only the
#'   school curricular context is shown. The \code{visualizHome()} can also be used to visualize, understand, and
#'   select appropriate values. Defaults to NULL.
#'
#' @param curriculum.start.points a matrix or list of matrices providing the start points of the school curriculum to
#'   be offered during each grade (or other time division). The matrices should have one column and one row per grade. Each
#'   list entry corresponds to a version of the curriculum, such as 'typical' or 'remedial.' The
#'   \code{visualizeSchool()} function can also be used to visualize, understand, and select appropriate
#'   values.
#'
#' @param curriculum.widths a matrix or list of matrices providing the widths or spans of the school curriculum to
#'   be offered during each grade (or other time division). The matrices should have one column and one row per grade. Each
#'   list entry corresponds to a version of the curriculum, such as 'typical' or 'remedial.' The
#'   \code{visualizeSchool()} function can also be used to visualize, understand, and select appropriate
#'   values.
#'
#' @param curriculum.review.slopes A matrix or list of matrices describing the steepness of the school curriculum cutoff
#'   at the lower range. Conceptually controls the amount of review content. Small numbers indicate a shallower slope and
#'   therefore more review content. As general guidance, values in the range of 10-20 describe heavy review, while 50-100
#'   decribes little review, though this depends on the scale of the curriculum. The
#'   \code{visualizeSchool()} function can also be used to visualize, understand, and select appropriate
#'   values.
#'
#' @param curriculum.advanced.slopes a matrix or list of matrices describing the steepness of the school curriculum
#'   cutoff at the upper range. Conceptually controls the amount of advanced content. Small numbers indicate a shallower slope and
#'   therefore more advanced content. As general guidance, values in the range of 10-20 describe heavy review, while 50-100
#'   decribes little review, though this depends on the scale of the curriculum. The
#'   \code{visualizeSchool()} function can also be used to visualize, understand, and select appropriate
#'   values.
#'
#' @param assignment A vector of integer values assigning school curricula to time intervals. The length is the number of
#'   time intervals to simulate. Each entry contains a number representing which grade-level curriculum to present. Zero
#'   denotes periods of no school instruction, such as summer breaks. The numbers correspond to the row index of the
#'    \code{curriculum.start.points}, \code{curriculum.widths}, \code{curriculum.review.slopes}, and
#'    \code{curriculum.advanced.slopes} objects.
#'
#' @param points Integer value desribing the number of points within the span of the curriculum
#'  for which the intensity is calculated. This controls the smoothness of the gradient of intensity
#'  shading in the plot. Larger values yield better smoothness but require longer execution time.
#'  Defaults to 500.
#'
#' @param rate Scalar, the exponential decay parameter describing the home curriculum function. If a value is
#'   given, the background is shaded proportional to the home learning rate. If NULL, only the school
#'   curriculum is displayed. Defaults to NULL.
#'
#' @param schoolcolor The color used to represent the school curricular intensity. Defaults to "grey25".
#'
#' @param homecolor The color used to represent the home curricular intensity. Defaults to "grey25". Has an effect
#'   only if a non-null value is given for \code{rate}.
#'
#' @param zoomschool One of "none", "school", or "truncate". Sets the range of the y-axis of the plot when a non-null
#'  value is provided for \code{rate}. "none" does not zoom. "school" shows achievement of zero through the maximum
#'  level of the school curriculum. "truncate" shows the minimum and maximum achievement levels of the school
#'  curriculum.
#'
#' @param linecolor The color used for the horizontal reference lines representing where the
#'   curriculum has full intensity. Defaults to "indianred2".
#'
#' @param versionlabels A character vector providing labels for the facets representing the different
#'   versions of the curriculum (e.g., typical, remedial).
#'
#' @param annotate logical. should the figure be annotated with the row numbers of the school
#'   curricula? defaults to FALSE
#'
#' @return An object of class \code{ggplot2}
#'
#' @family visualizations
#'
#' @seealso \code{\link{ZPDGrowthTrajectories}} for simulating growth trajectories.
#'
#' @importFrom stats uniroot
#'
#' @examples
#' #' # assignment object simulating starting kindergarten on time 201
#' #  Kindergarten for 200 days, followed by 100 days of summer
#' #  then 200 days of first grade
#' assignment <- c(rep(0, times=200), rep(1, times=200),
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
#'   )
#'
#' curriculum.advanced.slopes <- list(
#'   # "typical curriculum" advanced slopes for K and first grade
#'   matrix(c(50, 50), nrow=2, ncol=1),
#'   # "advanced curriculum" advanced slopes for K and first grade
#'   matrix(c(25, 25), nrow=2, ncol=1)
#'   )
#'
#' visualizeContext(home.learning.decay.rate=6,
#'                   curriculum.start.points=curriculum.start.points,
#'                   curriculum.widths=curriculum.widths,
#'                   curriculum.review.slopes=curriculum.review.slopes,
#'                   curriculum.advanced.slopes=curriculum.advanced.slopes,
#'                   assignment=assignment,
#'                   annotate=TRUE,
#'                   versionlabels = c("Typical", "Advanced"),
#'                   linecolor="blue",
#'                   zoomschool="truncate")
#'
#' @export


visualizeContext <- function(home.learning.decay.rate=NULL,
                             curriculum.start.points, curriculum.widths,
                             curriculum.review.slopes, curriculum.advanced.slopes,
                             assignment,
                             points=500, annotate=FALSE, schoolcolor="gray25",
                             homecolor="gray25", linecolor="indianred2", versionlabels=NULL,
                             zoomschool="truncate") {

  # rename objects
  slope1 <- curriculum.review.slopes
  slope2 <- curriculum.advanced.slopes
  rate <- home.learning.decay.rate

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

  # calculate the number of different curricula (e.g. "grades")
  #   and the number of different versions (e.g. typical, advanced, remedial)
  n.curricula <- length(curriculum.start.points[[1]])
  n.versions <- length(curriculum.start.points)

  starts <- unlist(curriculum.start.points)
  ends <- unlist(curriculum.widths)
  slope1 <- unlist(slope1)
  slope2 <- unlist(slope2)

  dat <- data.frame(
    curriculum=rep(1:n.curricula, times=n.versions),
    version=rep(1:n.versions, each=n.curricula),
    start=starts,
    end=starts+ends
    )

  # process assignment object
  #  convert it to data frame
  #  add a time point variable
  #  remove rows where there is no school curriculum offered
  assignment.df <- data.frame(curriculum=assignment)
  assignment.df$time <- 1:nrow(assignment.df)
  assignment.df <- assignment.df[assignment.df$curriculum != 0,]

  dat <- merge(assignment.df, dat, by="curriculum")

  slope.df <- data.frame(
    curriculum=rep(1:n.curricula, times=n.versions),
    version=rep(1:n.versions, each=n.curricula),
    slope1=slope1,
    slope2=slope2
  )

  dat <- merge(dat, slope.df, by=c("curriculum", "version"))

  # populate x with the region of non-zero curricular intensity
  x <- seq(
    min(dat$start) - 1/min(slope1),
    max(dat$end) + 1/min(slope2),
    length.out=points)

  dat2 <- dat[rep(1:nrow(dat), each=length(x)),]
  dat2$x <- rep(x, times=nrow(dat))


  dat2$intensity <- mapply(school, dat2$x, dat2$slope1, dat2$slope2, dat2$start, dat2$end)

  dat2 <- dat2[dat2$intensity > 0,]

  if (!is.null(versionlabels)) {
    # check for length equality
    if (length(curriculum.start.points) != length(versionlabels)) {stop("The number of versionlabels does not match the number of versions of the curriculum")}
    dat$version <- factor(dat$version, labels=versionlabels)
    dat2$version <- factor(dat2$version, labels=versionlabels)
    }

  p <- ggplot2::ggplot()+
    ggplot2::geom_tile(data=dat2, ggplot2::aes(x=time, y=x, alpha=intensity), fill=schoolcolor) +
    ggplot2::geom_point(data=dat, ggplot2::aes(x=time, y=start), size=.1, color=linecolor)+
    ggplot2::geom_point(data=dat, ggplot2::aes(x=time, y=end), size=.1, color=linecolor)+
    ggplot2::facet_wrap(~version)+
    ggplot2::theme_bw()+
    ggplot2::coord_cartesian(xlim=c(0, length(assignment)))+
    ggplot2::labs(x="time", y="achievement", alpha="intensity")+
    ggplot2::theme(legend.position="none")


  if (!is.null(rate)) {

    # get the max practically achievable achievement
    #  for setting the scale of the y-axis

    if (zoomschool=="none") {
      max.achievement <- max(
        max(dat$end - 1/dat$slope2),
        stats::uniroot(function(x, rate){exp(-rate*x)-1e-3}, rate=rate, interval=c(1e-10, 1e100))$root
      )

        achievement <- seq(0, max.achievement, length.out=points)

    } else if (zoomschool == "school") {
      max.achievement <- max(dat$end + 1/dat$slope2)
      achievement <- seq(min(dat$start - 1/dat$slope1), max.achievement, length.out=points)

    } else if (zoomschool == "truncate") {
      max.achievement <- max(dat$end + 1/dat$slope2)
      achievement <- seq(0, max.achievement, length.out=points)
    }


    homegrid <- expand.grid(
      time = seq(0, length(assignment), length.out=points),
      achievement = achievement
    )

    homegrid$intensity <- home(x=homegrid$achievement, rate=rate)

    p <- p + ggplot2::geom_raster(data=homegrid, ggplot2::aes(x=time, y=achievement,
                                                     alpha=intensity), fill=homecolor)

  }


  if (annotate==TRUE) {

    # make a dataframe of assignment so time is represented as a discrete variable
    #   rather than a position
    assignment.df <- data.frame(time=1:length(assignment), curriculum=assignment)

    # define a function to find the midpoint of a set of target values in the
    #   assignment object
    findmids <- function(x, target) {round(mean(x$time[x$curriculum==target]),0)}

    # find the midpoints of unique nonzero curriculum value
    #  this will define the positions and values for the annotations
    index <- sapply(unique(assignment[assignment>0]), findmids, x=assignment.df)

    # create a dataframe
    df.annotations <- data.frame(x=index, y=assignment[index])

    # scale of y from the highest to lowest value plotted. used to calculate label
    #  vertical offset value
    scaley <- max(dat$end - 1/dat$slope2)-min(dat$start - 1/dat$slope1)

    # add the text annotations to the plot object
    p <- p + ggplot2::geom_text(data=df.annotations,
                                ggplot2::aes(x=x, y=min(dat$start - 1/dat$slope1), label=y),
                                nudge_y=-.05*scaley)
  }

  return(p)

}
