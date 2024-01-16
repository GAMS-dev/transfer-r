# GAMS Transfer R

gamstransfer: data exchange between GAMS and R
# Dependencies#
gamstransfer depends on the following R packages:
- R6
- collections
- Rcpp
- R.utils

To build this package from source, following libraries must be installed:
- zlib

# How to install#

To install given a package (.zip, .tar.gz etc. file)
for binary packages:
```
install.packages("[binary_package_name]", type=binary )
```
for source:
```
install.packages("[binary_package_name]")
```
# How to build#
If you are trying to build from the clone of this repository, you first have
 to run the `configure` script that is at the root of this package. This sript
 moves the necessary source files from the GDX submodule to the src directory
 of this package. This script generates some log and cache files. To install
 and remove these files, one can do the following:
 ```
 R CMD INSTALL gamstransfer --clean
 ```
 This calls the `cleanup` script after installation and removes the unnecessary files.

 Next, you can run `R CMD INSTALL ...` or `devtools::install()`. This will
 perform a staged installation. Before building the package, `configure`
 script inside the package directory will be run. This script performs basic
 checks for compilers but it also checks for the presence of `zlib`. If this
 script fails to automatically detect `zlib` on your device, you can use
 `--configure-vars` argument when using `R CMD INSTALL` to point the
 installation to `zlib` as follows:
 ```
 R CMD INSTALL gamstransfer --configure-vars="LIB_DIR='[path_to_dll]'"
 ```
 or

 ```
 R CMD INSTALL gamstransfer --configure-vars="INCLUDE_DIR='[path_to_include]' LIB_DIR='[path_to_bin]'"
 ```
Ideally, if you have `zlib` in your environment variable `PATH`, the configure script should detect it.

If you are trying to install from source on Windows, the `configure`script will not be executed. Instead,
a warning will be displayed. Since there is only one dependency i.e., zlib, if the installation fails and
throws errors with `compress` and `uncompress`, it is suggested that `zlib` should be inspected first.
