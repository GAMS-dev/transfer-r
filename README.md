# GAMS Transfer R #

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

# Dependencies #
gamstransfer depends on the following R packages:
- R6
- collections
- Rcpp
- R.utils

To build this package from source, the following libraries must be installed:
- zlib

# How to build #

A quick and easy way to build `gamstransfer` source package is by running ```R CMD build gamstransfer```.This will generate the package source file. To build a binary package, run `R CMD INSTALL gamstransfer --build`. This command builds a binary package along with installation.

# How to install #

This section explains how to install `gamstransfer` using a pre-built package file.

## binary package ##

```
# from the command prompt
R CMD INSTALL [binary_file_name]

# from R console
install.packages("[binary_file_name]", type="binary" )
```

## source package ##

```
# from the command line
R CMD INSTALL [source_file_name]

# from R console
install.packages("[source_file_name]")
```

### dependency on zlib ###
Installation from the source package starts by executing the `configure` script. This script checks for the presence of `zlib` and performs
basic checks for compilers. If you see an error saying `zlib` is not found,
make sure that zlib is installed and is in your environment variable `PATH`.
If the issue persists and you know that you have `zlib` on your system, you
can manually point the installer to it using `--configure-vars` arguemnt
as follows.

```
R CMD INSTALL gamstransfer --configure-vars="LIB_DIR='[path_to_shared_object]'"
```
or
```
R CMD INSTALL gamstransfer --configure-vars="INCLUDE_DIR='[path_to_include]' LIB_DIR='[path_to_bin]'"
```

Users installing from R console can make use `configure.vars` argument to achieve the same.

Additional comments about the use of `configure` script:
* The `configure` script generates also generates log and cache files. To remove these files
    along with installation, use the following command.
    ```
    R CMD INSTALL gamstransfer --clean
    ```
    This calls the `cleanup` script after installation and removes unnecessary files.

* `configure` script is autogenerated using `autoconfig` with the help of `configure.ac` file
    and should be modified only by modifying the `configure.ac` and running `autoconfig configure.ac`.

*  For Windows users: If you are trying to install from source on Windows, the `configure`script will not be executed. Instead, a warning will be displayed. Since there is only one dependency i.e., zlib, if the installation fails and throws errors with the functions functions `compress` and `uncompress`, it is suggested that `zlib` should be inspected first.
