# Install and Update YOLO

This function automates the installation/updating of YOLO and all its
Python dependencies in a dedicated Python virtual environment for use
with the
[trackRai](https://swarm-lab.github.io/trackRai/reference/trackRai.md)
apps.

## Usage

``` r
install_yolo(python_version = "3.12.5", cuda_win_version = "auto")
```

## Arguments

- python_version:

  A character string indicating the version of Python you would like
  YOLO to run on (default: "3.12.5"). YOLO is currently compatible with
  Python 3.8.0 to 3.12.8. Not all versions of Python will necessarily
  work on your system, but the chosen default works on most systems that
  we tested so far.

- cuda_win_version:

  Windows-only. A character string indicating the version of CUDA
  installed on the computer. Valid values are: auto (the function tries
  to determine automatically the version of CUDA, the default), NA (YOLO
  will be installed without CUDA support), 11.8, 12.4, and 12.6. All
  other values will throw an error. Ignored on Mac and Linux computers

## Value

If the installation/update completes successfully, a data frame
indicating the location of the YOLO installation and its version number.

## Note

If no Python of the requested version is found when the dedicated Python
virtual environment is created, this function installs it first. This
function does not start Python before the virtual environment exists. If
Python is already running in the current R session, restart R to use the
new virtual environment.

## See also

[`remove_yolo()`](https://swarm-lab.github.io/trackRai/reference/remove_yolo.md)

## Author

Simon Garnier, <garnier@njit.edu>
