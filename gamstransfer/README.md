
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamstransfer

<!-- badges: start -->

<!-- badges: end -->

`gamstransfer` is a data interface to efficiently transfer data between
GAMS and R.

## Quick Start Guide

To install the latest version of `gamstransfer` from CRAN, use the
following command.

``` r
install.packages("gamstransfer")
```

To use `gamstransfer`, load the package using `library(gamstransfer)`.
Following is the code snippet to read data from a GDX file named
`foo.gdx`.

``` r
library(gamstransfer)
m = Container$new("foo.gdx")
```

## Dependencies

gamstransfer depends on the following R packages:

  - R6
  - collections
  - Rcpp
  - R.utils

To build this package from source, the following libraries must be
installed:

  - zlib

## Install a particular release from GitHub

It is recommended that the users install released versions of
`gamstransfer` using

``` r
remotes::install_github("GAMS-dev/transfer-r/gamstransfer@vX.Y.Z")
```

All the package dependencies can be installed at once using the
`install.packages` command.

``` r
install.packages(c("R6", "collections", "Rcpp", "R.utils"))
```

## Install the development version from GitHub

Installation from the development version is similar to installation
from the source. To install the development version, users need to point
the installer to the `zlib` library dependency. There are a couple ways
to do it.

1.  You can add `zlib` to your system environment variable `PATH`. This
    is the easier and the recommended way. One can then install
    `gamstransfer` using the following command.

<!-- end list -->

``` r
remotes::install_github("GAMS-dev/transfer-r/gamstransfer")
```

2.  You can manually point the installer to `zlib` using
    `configure.vars` argument. An example is shown below.

<!-- end list -->

``` r
remotes::install_github("GAMS-dev/transfer-r/gamstransfer@vX.Y.Z",
configure.vars=c("INCLUDE_DIR='[path_to_include]' LIB_DIR='[path_to_bin]'"))
```

or

``` r
remotes::install_github("GAMS-dev/transfer-r/gamstransfer@vX.Y.Z",
configure.vars=c("LIB_DIR='[path_to_shared_object]'"))
```

3.  One can also set `LIB_DIR` and `INCLUDE_DIR` permanently in the
    .Renviron file.

## Build gamstransfer from source

A quick and easy way to build `gamstransfer` source package is by
running `R CMD build .` from the package directory. This will generate
the package source file. To build a binary package, run `R CMD INSTALL .
--build` from the package directory. This command builds a binary
package along with installation.

## Install gamstransfer using a pre-build package file

You can install the package from the command line or from the R console.

### binary package

To install `gamstransfer` from command line using a binary package, run
the following command.

    R CMD INSTALL [binary_file_name]

To install `gamstransfer` from R console using a binary package, run the
following command.

``` r
install.packages("[binary_file_name]", type="binary" )
```

### source package

To install `gamstransfer` from command line using a source package, run
the following command.

    R CMD INSTALL [source_file_name]

To install `gamstransfer` from R console using a source package, run the
following command.

``` r
install.packages("[source_file_name]")
```

Note that installing from source requires `zlib`. Please refer to
“installing the development version from GitHub” section in this file.

## Documentation

You can view the detailed documentation for `gamstransfer`
[here](https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html).

## Feedback

We would love to get feedback on `gamstransfer`. Please direct your
questions to <support@gams.com>
