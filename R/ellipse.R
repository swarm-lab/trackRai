.pdiff <- function(a, b) {
  a - matrix(b, nrow = length(a), ncol = length(b), byrow = TRUE)
}

.dist2ellipse <- function(x, y, cx, cy, width, height, angle) {
  relx <- -.pdiff(cx, x)
  rely <- -.pdiff(cy, y)
  cosa <- cos(-angle)
  sina <- sin(-angle)
  sqrt(((relx * cosa - rely * sina) / (width / 2))^2 +
    ((relx * sina + rely * cosa) / (height / 2))^2)
}

#' @title Fit an Ellipse
#'
#' @description Given a set of x/y positions, this function attempts to find the
#'  best fitting ellipse that goes through these points.
#'
#' @param x,y Vectors of x and x positions.
#'
#' @return A vector with 5 elements: the x and y coordinated of the center of
#'  the ellipse, the width and height of the ellipse, and the angle of the
#'  ellipse relative to the y axis in degrees.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' optim_ellipse(rnorm(5), rnorm(5))
#'
#' @export
optim_ellipse <- function(x, y) {
  md <- sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2)
  start <- c(mean(x), mean(y), md, md, 0)

  opt <- stats::optim(start, function(par) {
    sum((.dist2ellipse(x, y, par[1], par[2], par[3], par[4], par[5]) - 1)^2)
  },
  method = "L-BFGS-B",
  lower = c(min(x), min(y), -Inf, -Inf, -pi),
  upper = c(max(x), max(y), Inf, Inf, pi)
  )

  out <- opt$par
  out[5] <- 180 * out[5] / pi
  out
}
