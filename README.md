# trackRai <a href="https://swarm-lab.github.io/trackRai/"><img src="man/figures/logo.png" align="right" height="138" alt="trackRai website" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/trackRai)](https://CRAN.R-project.org/package=trackRai)
[![R-CMD-check](https://github.com/swarm-lab/trackRai/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/swarm-lab/trackRai/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/swarm-lab/trackRai/graph/badge.svg)](https://app.codecov.io/gh/swarm-lab/trackRai)
[![test-coverage](https://github.com/swarm-lab/trackRai/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/swarm-lab/trackRai/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

## Description 

trackRai is a YOLO-based multi-object tracking software for R. It provides
easy-to-use (or so I think) Shiny apps for automating the preparation and
training of [YOLO11](https://docs.ultralytics.com/models/yolo11/) models, and
for performing the tracking of multiple objects in a video in a range of
conditions while maintaining individual identities.

Finally, trackRai can leverage
[trackRcv](https://swarm-lab.github.io/trackRcv)'s convenience apps to manually
correct common errors that occurs during tracking, and to export
publication-ready videos showing the moving objects with their track overlaid on
top of them.

---

## Installation

You can install the development version of trackRai from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("swarm-lab/trackRai")
```

---

## User guides

- [1. Installing trackRai](https://swarm-lab.github.io/trackRai/articles/z1_install.html)
- [2. Preparing a training dataset](https://swarm-lab.github.io/trackRai/articles/z2_prepare.html)
- [3. Training a YOLO model](https://swarm-lab.github.io/trackRai/articles/z3_train.html)
- [4. Tracking objects](https://swarm-lab.github.io/trackRai/articles/z4_track.html)
- [5. Fixing tracks](https://swarm-lab.github.io/trackRcv/articles/z3_fixing_tracks.html)
- [6. Visualizing tracks](https://swarm-lab.github.io/trackRcv/articles/z4_visualizing_tracks.html)

---

## FAQ

**Will something break? Can I use trackRai in 'production' mode?** 

Something will definitely break. This is mostly a one-person operation and I 
cannot promise that I have fully tested every single scenario that could 
challenge trackRai. This being said, it will work fine in most cases and is 
certainly usable for most tracking projects. If you run into an issue, please 
report it at: https://github.com/swarm-lab/trackRai/issues.

--

**How can I help?**

trackRai is an open-source project, meaning that you can freely modify its code
and implement new functionalities. If you have coding skills, you are more than 
welcome to contribute new code or code improvement by submitting pull requests 
on the GitHub repository of the project at: https://github.com/swarm-lab/trackRai. 
I will do my best to review and integrate them quickly. 

If you do not feel like contributing code, you can also help by submitting bug 
reports and feature requests using the issue tracker on the GitHub repository of 
the project at: https://github.com/swarm-lab/trackRai/issues. These are extremely 
helpful to catch and correct errors in the code, and to guide the development of 
trackRai by integrating functionalities requested by the community. 
