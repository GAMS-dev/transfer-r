% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DomainViolation.R
\name{DomainViolation}
\alias{DomainViolation}
\title{DomainViolation Class}
\description{
Contains information about the domain violation
for a symbol.
Please visit https://transfer-r.readthedocs.io/en/latest/
for detailed documentation of this package.
}
\section{Fields}{

\describe{
\item{\code{symbol}}{symbol name}

\item{\code{dimension}}{dimension in which domain violation is present}

\item{\code{domain}}{domain name}

\item{\code{violations}}{vector of violations}
}}

\examples{
m <- Container$new()
i <- Set$new(m, "i", records = paste0("i", 1:5))
p <- Parameter$new(m, "p", i, records = data.frame(i = c("i1", "i3", "i6"), value = c(1, 5, 7)))
dv <- p$getDomainViolations()[[1]]
sym_dv <- dv$symbol
dim_dv <- dv$dimension
domain_dv <- dv$domain
violation_dv <- dv$violations
}
