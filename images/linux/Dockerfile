# r-base mixes the rolling testing and sid apt suites, which can drift out of sync with
# each other and break dependency resolution (seen repeatedly with curl/libcurl4t64 and
# python3's dependency chain). r-ver builds on Debian stable instead, avoiding that
# whole class of problem structurally.
FROM rocker/r-ver:latest

RUN apt-get update && apt-get -y install curl zip build-essential python3 python3-pip git && apt-get -y clean

RUN R -e "install.packages('R6',dependencies=TRUE); library(R6)"
RUN R -e "install.packages('Rcpp',dependencies=TRUE); library(Rcpp)"
RUN R -e "install.packages('assertthat',dependencies=TRUE); library(assertthat)"
RUN R -e "install.packages('R.utils',dependencies=TRUE); library(R.utils)"
RUN R -e "install.packages('collections',dependencies=TRUE); library(collections)"
RUN R -e "install.packages('testthat',dependencies=TRUE); library(testthat)"

# install gitlab glab CLI (release-cli, used previously, is deprecated in GitLab 18.0
# and removed in 20.0 - glab's `release create` replaces it, see release_job)
RUN curl -L --output /tmp/glab.deb \
    "https://gitlab.com/api/v4/projects/gitlab-org%2Fcli/packages/generic/glab/1.115.0/glab_1.115.0_linux_amd64.deb" && \
    dpkg -i /tmp/glab.deb && \
    rm /tmp/glab.deb
