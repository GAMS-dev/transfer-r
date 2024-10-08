% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Alias.R
\name{Alias}
\alias{Alias}
\title{Alias Class}
\description{
A class for Alias objects.
Please visit https://transfer-r.readthedocs.io/en/latest/
for detailed documentation of this package.
}
\examples{
# create a container
m <- Container$new()
# add a set
i <- Set$new(m, "i")
# add an alias to the set "i"
ii <- Alias$new(m, "ii", i)
}
