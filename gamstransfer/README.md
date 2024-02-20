
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gamstransfer

<!-- badges: start -->

<!-- badges: end -->

GAMS Transfer is a package to maintain GAMS data outside a GAMS script
in a programming language like Python, Matlab or R. It allows the user
to add GAMS symbols (Sets, Aliases, Parameters, Variables and
Equations), to manipulate GAMS symbols, as well as read/write symbols to
different data endpoints. GAMS Transfer’s main focus is the highly
efficient transfer of data between GAMS and the target programming
language, while keeping those operations as simple as possible for the
user. In order to achieve this, symbol records - the actual and
potentially large-scale data sets - are stored in native data structures
of the corresponding programming languages. The benefits of this
approach are threefold: (1) The user is usually very familiar with these
data structures, (2) these data structures come with a large tool box
for various data operations, and (3) optimized methods for reading from
and writing to GAMS can transfer the data as a bulk - resulting in the
high performance of this package.

## Quick Start Guide

To install `gamstransfer` from GitHub, install the released version of
`remotes` package from CRAN.

``` r
install.packages("remotes")
```

You can then install a particular release of `gamstransfer` from GitHub
using the following command.

``` r
remotes::install_github("GAMS-dev/gtr/gamstransfer@vX.Y.Z")
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
remotes::install_github("GAMS-dev/gtr/gamstransfer@vX.Y.Z")
```

To use `gamstransfer`, all the R package dependencies can be installed
using the `install.packages` command. All the dependencies can be
installed at once using the following command.

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
remotes::install_github("GAMS-dev/gtr/gamstransfer")
```

2.  You can manually point the installer to `zlib` using
    `configure.vars` argument. An example is shown below.

<!-- end list -->

``` r
remotes::install_github("GAMS-dev/gtr/gamstransfer@vX.Y.Z", configure.vars=c("INCLUDE_DIR='[path_to_include]' LIB_DIR='[path_to_bin]'"))
```

or

``` r
remotes::install_github("GAMS-dev/gtr/gamstransfer@vX.Y.Z", configure.vars=c("LIB_DIR='[path_to_shared_object]'"))
```

3.  One can also set `LIB_DIR` and `INCLUDE_DIR` permanently in the
    .Renviron file

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
