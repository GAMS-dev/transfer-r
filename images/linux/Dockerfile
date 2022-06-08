# Build from the MATLAB base image
FROM r-base

RUN R -e "install.packages('devtools',dependencies=TRUE)"
RUN R -e "install.packages('R6',dependencies=TRUE)"
RUN R -e "install.packages('Rcpp',dependencies=TRUE)"
RUN R -e "install.packages('assertthat',dependencies=TRUE)"
RUN R -e "install.packages('R.utils',dependencies=TRUE)"
