# GAMS Transfer R

gamstransfer: data exchange between GAMS and R

# How to install from GitHub #

The R devtools package allows packages that are hosted on GitHub to be
installed directly from there. This is a convenient way to install
GAMS Transfer R.

The devtools package is installed by running the following command in R.
```
install.packages("devtools")
install.packages("usethis")
```

devtools can then be loaded
```
library(usethis)
library(devtools)
```

Installation from GitHub will install a package and all of its
prerequisites:

```
install_github("GAMS-dev/gams-transfer-r")
```
