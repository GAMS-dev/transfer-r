# GAMS Transfer R

gamstransfer: data exchange between GAMS and R

GAMS Transfer is a package to maintain GAMS data outside a GAMS script
 in a programming language like Python, Matlab or R. It allows the user
to add GAMS symbols (Sets, Aliases, Parameters, Variables and Equations),
 to manipulate GAMS symbols, as well as read/write symbols to different
data endpoints. GAMS Transfer's main focus is the highly efficient transfer
 of data between GAMS and the target programming language, while keeping
those operations as simple as possible for the user. In order to achieve this,
 symbol records - the actual and potentially large-scale data sets - are
stored in native data structures of the corresponding programming languages.
The benefits of this approach are threefold: (1) The user is usually very
familiar with these data structures, (2) these data structures come with a
large tool box for various data operations, and (3) optimized methods for
reading from and writing to GAMS can transfer the data as a bulk - resulting
 in the high performance of this package.
# Dependencies
gamstransfer depends on the following R packages:
- R6
- collections
- Rcpp
- R.utils

To build this package from source, following libraries must be installed:
- zlib

# How to install

To install given a binary package (.zip, .tar.gz etc. file):
```
install.packages("[binary_file_name]", type="binary" )
```

To install a source package:
```
install.packages("[source_file_name]")
```
# How to build

***Setup***

To build the package from the clone of this repository, first
run the `setup` script. This script moves necessary source
files from the GDX submodule to the src directory of this package.

***Build***

A quick and easy way to build `gamstransfer` is by running ```R CMD INSTALL gamstransfer``` followed by installing the resulting source package as mentioned above.

The install command above starts by running the `configure` script in the
package directory. This script checks for the presence of `zlib` and performs
basic checks for compilers. If you see an error saying `zlib` is not found,
make sure that zlib is installed and is in your environment variable `PATH`.
If the issue persists and you know that you have `zlib` on your system, you
can manually point the installer to it using `configure-vars` arguemnt
as follows:

R CMD INSTALL gamstransfer --configure-vars="LIB_DIR='[path_to_dll]'" or R CMD INSTALL gamstransfer --configure-vars="INCLUDE_DIR='[path_to_include]' LIB_DIR='[path_to_bin]'"

The `configure` script generates some log and cache files. To remove these files
automatically after installation, use the following command.
```
R CMD INSTALL gamstransfer --clean
```
This calls the `cleanup` script after installation and removes the unnecessary files.

If you are trying to install from source on Windows, the `configure`script will not be executed. Instead, a warning will be displayed. Since there is only one dependency i.e., zlib, if the installation fails and throws errors with the functions functions `compress` and `uncompress`, it is suggested that `zlib` should be inspected first.

Note:
The configure script is generated using `autoconfig` with the help of configure.ac file
 and should be modified only by modifying the `configure.ac` and running `autoconfig configure.ac`.
