library(proffer)

theImage <- image("~/Desktop/termites/background.png")
theMask <- image("~/Desktop/termites/mask.png")
toDisplay <- zeros(nrow(theImage), ncol(theImage), 3)
bgr1 <- zeros(nrow(theImage), ncol(theImage), 3)
gray1 <- zeros(nrow(theImage), ncol(theImage), 1)
gray2 <- zeros(nrow(theImage), ncol(theImage), 1)
ccDump <- zeros(nrow(theImage), ncol(theImage), 1, "16U")
zero <- zeros(nrow(theImage), ncol(theImage), 3)

px <- pprof({
# system.time({
  cloneImage(zero, toDisplay)

  if (isImage(theImage)) {
    add(toDisplay, 1, toDisplay)
  }
  
  changeColorSpace(theMask, "GRAY", gray1)
  setTo(toDisplay, gray1, "green", target = "self")
  compare(theMask, 0, ">", bgr1)
  changeColorSpace(bgr1, "GRAY", gray1)
  invert(gray1, gray1)
  setTo(toDisplay, gray1, "red", target = "self")
  
  if (isImage(theImage)) {
    addWeighted(toDisplay, theImage, c(0.25, 0.75), target = toDisplay)
  } else {
    addWeighted(toDisplay, zeros(nrow(theMask), ncol(theMask), 3),
      c(0.25, 0.75),
      target = toDisplay
    )
  }
  
  sc <- max(dim(toDisplay) / 720)
  r <- 0.01 * min(nrow(toDisplay), ncol(toDisplay))

  changeColorSpace(theMask, "GRAY", gray1)
  morph(gray1, "dilate", k_shape = "cross", k_height = 1, k_width = 1, target = gray2)
  invert(gray1, gray1)
  morph(gray1, "dilate", k_shape = "cross", k_height = 1, k_width = 1, target = gray1)
  bitAnd(gray1, gray2, gray1)
  morph(gray1, "dilate",
    k_height = max(1, 0.5 * sc),
    k_width = max(1, 0.5 * sc),
    target = gray1
  )
  
  # canny(theMask, 0, 0, target = gray1)
  # morph(gray1, "dilate",
  #   k_height = max(1, 0.75 * sc),
  #   k_width = max(1, 0.75 * sc),
  #   target = gray1
  # )
  setTo(toDisplay, gray1, "white", target = "self")
  
  extractChannel(theMask, 1, gray1)
  h <- imhist(gray1, nbins = 257, range = c(0, 256))
  valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]
  
  com <- lapply(valid, function(val) {
    cc <- connectedComponents(gray1 == val, stats = FALSE, target = ccDump)
    lapply(1:cc$n, function(lab) {
      dt <- distanceTransform(border(ccDump == lab, 1))
      apply(findNonZero(dt == max(dt)[1]), 2, mean) - 1
    })
  })
  
  lab <- lapply(com, function(l) {
    lapply(l, function(loc) {
      pget(theMask, loc[1], loc[2])[1]
    })
  })
  
  mapply(function(com, lab) {
    mapply(function(com, lab) {
      drawText(toDisplay, lab,
        com[1] - (floor(log10(lab)) + 1) * 5,
        com[2] - 5 * sc,
        font_scale = 0.5 * sc,
        thickness = max(1, 1.5 * sc),
        color = "white"
      )
    }, com = com, lab = lab)
  }, com = com, lab = lab)
})


# if (collectMask() == 1) {
#   if (nrow(maskCoords) > 0) {
#     drawPolyline(toDisplay, maskCoords, closed = TRUE, color = "white", thickness = max(1, 1.5 * sc))
#   }
# }

# if (collectMask() > 0) {
#   drawCircle(toDisplay,
#     x = maskCoords[, 1], y = maskCoords[, 2],
#     radius = r * 1.5, thickness = -1, color = "white"
#   )
#   drawCircle(toDisplay,
#     x = maskCoords[, 1], y = maskCoords[, 2],
#     radius = r, thickness = -1, color = "red"
#   )
# }



sp <- image(array(sample(-25:25, prod(dim(toDisplay)), replace = TRUE), dim = dim(toDisplay)))

runif(prod(dim(toDisplay)), 0, 25)

