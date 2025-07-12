.sampleMinDist <- function(roi, n, min_dist) {
  nr <- nrow(roi)
  nc <- ncol(roi)
  rnd_loc <- matrix(NA, n, 2)
  nz <- which(roi > 0, arr.ind = TRUE)
  l <- nrow(nz)
  rnd <- sample(seq_len(l))
  i <- 1
  j <- 1
  keep_going <- TRUE

  while (keep_going) {
    coords <- nz[rnd[i], ]
    i <- i + 1

    if (roi[coords[1], coords[2]] > 0) {
      rnd_loc[j, ] <- coords
      j <- j + 1
      rr <- c(max(1, coords[1] - min_dist), min(nr, coords[1] + min_dist))
      rc <- c(max(1, coords[2] - min_dist), min(nc, coords[2] + min_dist))
      roi[rr[1]:rr[2], rc[1]:rc[2]] <- 0L
    }

    if (j > n) {
      keep_going <- FALSE
    }

    if (i > l) {
      keep_going <- FALSE
    }
  }

  rnd_loc <- rnd_loc[1:(j - 1), ]
}