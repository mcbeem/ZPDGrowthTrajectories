#' A helper function for describing the home curriculum.
#'
#' \code{buildHomeCurriculum} creates the home curriculum object expected by \code{ZPDGrowthTrajectories}.
#'
#' This function creates the home curriculum in the format expected by the
#' expected by the \code{ZPDGrowthTrajectories} function.
#' It is an internal function and not intended for stand-alone use.
#' The home curriculum is defined according to a beta probability
#' density function defined on [0,1].
#'
#' @param  points an integer number of integration points. Higher means more accuracy but slower execution.
#' @param shape1 shape1 parameter for \code{dbeta}.
#' @param shape2 shape2 parameter for \code{dbeta}.
#' @export


buildHomeCurriculum <- function(points, shape1, shape2) {

  # !!!there is a bug here related to min/max!!!
  # if use specs min max as not 0/1, an incongruency occurs in the integration points

  # make x vector; a vector of integration points. Note that beta distribution is only
  # defined on [0,1]. So min / max are fixed to these values.
  # The endpoints can be rescaled via the min=, max= arguments of the function.
  x <- seq(0, 1, length.out=points)

  # note that this is normalized by dividing by the max of the beta pdf, which is the shape2 parameter
  values <- dbeta(x, shape1=shape1, shape2=shape2) / shape2

  return(matrix(values, nrow=1, ncol=points))
}

