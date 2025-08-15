# .minOBB <- function(m) {
#   com <- matrix(apply(m, 2, mean), nrow = nrow(m), ncol = 2, byrow = TRUE)
#   init <- which.max(apply((com - m)^2, 1, sum))
#   ix <- rdist::farthest_point_sampling(
#     m,
#     k = 5,
#     initial_point_index = init,
#     metric = "euclidean"
#   )

#   # ell <- trackRcv::optim_ellipse(m[ix, 1], m[ix, 2])
#   # out <- cv2$boxPoints(list(ell[1:2], ell[3:4], ell[5]))

#   out <- cv2$boxPoints(cv2$fitEllipse(reticulate::r_to_py(m[ix, ])))
#   out <- np$int_(out)
#   out
# }

# .approxOBB <- function(m) {
#   com <- colMeans(m)
#   mc <- scale(m, center = com, scale = FALSE)
#   pca_result <- prcomp(mc)
#   ev1 <- pca_result$rotation[, 1]
#   ra <- -atan2(ev1[2], ev1[1])
#   rm <- matrix(c(cos(ra), -sin(ra), sin(ra), cos(ra)), nrow = 2, byrow = TRUE)
#   mc <- t(rm %*% t(mc))
#   rx <- range(mc[, 1])
#   ry <- range(mc[, 2])
#   ext <- cbind(
#     x = c(rx[1], rx[2], rx[2], rx[1]),
#     y = c(ry[1], ry[1], ry[2], ry[2])
#   )
#   rm <- matrix(
#     c(cos(-ra), -sin(-ra), sin(-ra), cos(-ra)),
#     nrow = 2,
#     byrow = TRUE
#   )
#   ext <- t(rm %*% t(ext))
#   scale(
#     ext,
#     center = -com,
#     scale = FALSE
#   )
# }

.approxOBB <- function(m) {
  d <- as.matrix(dist(m))
  ix <- arrayInd(which.max(d), .dim = c(nrow(m), nrow(m)))
  com <- colMeans(m[ix, ])
  ra <- -atan2(m[ix[2], 2] - m[ix[1], 2], m[ix[2], 1] - m[ix[1], 1])
  rm <- matrix(c(cos(ra), -sin(ra), sin(ra), cos(ra)), nrow = 2, byrow = TRUE)
  mcr <- scale(m, center = com, scale = FALSE)
  mcr <- t(rm %*% t(mcr))

  rx <- range(mcr[, 1])
  ry <- range(mcr[, 2])
  ext <- cbind(
    x = c(rx[1], rx[2], rx[2], rx[1]),
    y = c(ry[1], ry[1], ry[2], ry[2])
  ) * 1.5
  ra <- -ra
  rm <- matrix(c(cos(ra), -sin(ra), sin(ra), cos(ra)), nrow = 2, byrow = TRUE)
  ext <- t(rm %*% t(ext))
  scale(
    ext,
    center = -com,
    scale = FALSE
  )
}
