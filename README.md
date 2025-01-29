# trackRai <a href="https://swarm-lab.github.io/trackRai/"><img src="man/figures/logo.png" align="right" height="138" alt="trackRai website" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/trackRai)](https://CRAN.R-project.org/package=trackRai)
[![R-CMD-check](https://github.com/swarm-lab/trackRai/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/swarm-lab/trackRai/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/swarm-lab/trackRai/graph/badge.svg)](https://app.codecov.io/gh/swarm-lab/trackRai)
[![test-coverage](https://github.com/swarm-lab/trackRai/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/swarm-lab/trackRai/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

trackRai is a YOLO-based multi-object tracking software for R. It provides a 
series of Shiny apps for automating the preparation and training of YOLO11 
models, for performing the tracking of multiple objects in a video, and for 
visualizing the results of the tracking process. 

---

## Installation

You can install the development version of trackRai from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("swarm-lab/trackRai")
```

---

## User guides

- [1 - Install YOLO11](https://swarm-lab.github.io/trackRai/articles/z1_installyolo.html)
- 2 - Prepare a dataset for training a YOLO model
- 3 - Train a YOLO model
- 4 - Track objects in a video using a trained YOLO model
- 5 - Visualize the results of tracking
