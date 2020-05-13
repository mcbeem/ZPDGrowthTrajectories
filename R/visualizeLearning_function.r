#' Function for visualizing the learning context, including the school and (optionally) home.
#'
#' @param curriculum.start.points a matrix or list of matrices providing the start points of each
#'   grade level (rows) and version (columns) of the school curriculum.
#' @param curriculum.widths a matrix or list of matrices providing the spans of each grade level
#'   (rows) and version (columns) of the school curriculum.
#' @param assignment a vector. The length is the number of time intervals to simulate. Each entry contains a number representing which
#'   grade-level curriculum to present. Zero denotes summers. The numbers correspond to the row index of the
#'    \code{curriculum.start.points} and \code{curriculum.widths} objects.
#' @param slope1 The steepness of the school curriculum cutoff at the lower range. Conceptually controls the amount of review content.
#' @param slope2 The slope of the school curriculum at the upper range. Conceptually controls the amount of advanced content.
#' @param points Integer value desribing the number of points within the span of the curriculum
#'  for which the intensity is calculated. This controls the smoothness of the gradient of intensity
#'  shading in the plot. Larger values yield better smoothness but require longer execution time.
#'  Defaults to 500.
#' @param rate Scalar, the exponential decay parameter describing the home curriculum function. If a value is
#'   given, the background is shaded proportional to the home learning rate. If NULL, only the school
#'   curriculum is displayed. Defaults to NULL.
#' @param schoolcolor The color used to represent the school curricular intensity. Defaults to "grey25".
#' @param homecolor The color used to represent the home curricular intensity. Defaults to "grey25". Has an effect
#'   only if a non-null value is given for \code{rate}.
#' @param zoomschool One of "none", "school", or "truncate". Sets the range of the y-axis of the plot when a non-null
#'  value is provided for \code{rate}. "none" does not zoom. "school" shows achievement of zero through the maximum
#'  level of the school curriculum. "truncate" shows the minimum and maximum achievement levels of the school
#'  curriculum.
#' @param linecolor The color used for the horizontal reference lines representing where the
#'   curriculum has full intensity. Defaults to "indianred2".
#' @param versionlabels A character vector providing labels for the facets representing the different
#'   versions of the curriculum (e.g., typical, remedial).
#' @param annotate logical. should the figure be annotated with the row numbers of the school
#'   curricula? defaults to FALSE
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
# visualizeLearning(curriculum.start.points=curriculum.start.points,
#                   curriculum.widths=curriculum.widths,
#                   assignment=assignment,
#                   slope1=50, slope2=100, points=200, annotate=T,
#                   versionlabels = c("Typical", "Advanced"),
#                   linecolor="blue",
#                   rate=6, zoomschool="truncate")
#'
#' @export


visualizeLearning <- function(curriculum.start.points, curriculum.widths, assignment,
                          slope1, slope2, points=500, annotate=FALSE, schoolcolor="gray25",
                          homecolor="gray25", linecolor="indianred2", versionlabels=NULL,
                          rate=NULL, zoomschool="truncate") {

  # if curriculum.start.points or curriculum.widths are matrices, make them lists
  if (is.matrix(curriculum.start.points)) {
    old.curriculum.start.points <- curriculum.start.points
    curriculum.start.points <- list()
    curriculum.start.points[[1]] <- old.curriculum.start.points
  }

  if (is.matrix(curriculum.widths)) {
    old.curriculum.widths <- curriculum.widths
    curriculum.widths <- list()
    curriculum.widths[[1]] <- old.curriculum.widths
  }

  # calculate the number of different curricula (e.g. "grades")
  #   and the number of different versions (e.g. typical, advanced, remedial)
  n.curricula <- length(curriculum.start.points[[1]])
  n.versions <- length(curriculum.start.points)

  starts <- unlist(curriculum.start.points)
  ends <- unlist(curriculum.widths)

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
  dat$slope1 <- slope1
  dat$slope2 <- slope2

  # populate x with the region of non-zero curricular intensity
  x <- seq(
    min(dat$start) - 1/slope1,
    max(dat$end) + 1/slope2,
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
