# Build from the R base image
FROM r-base


RUN apt-get update && apt-get -y install curl zip build-essential python3 python3-pip && apt-get -y clean

RUN R -e "install.packages('R6',dependencies=TRUE)"
RUN R -e "install.packages('Rcpp',dependencies=TRUE)"
RUN R -e "install.packages('assertthat',dependencies=TRUE)"
RUN R -e "install.packages('R.utils',dependencies=TRUE)"
RUN R -e "install.packages('collections',dependencies=TRUE)"
RUN R -e "install.packages('testthat',dependencies=TRUE)"

# install gitlab release-cli
RUN curl -L --output /usr/local/bin/release-cli \
    "https://release-cli-downloads.s3.amazonaws.com/latest/release-cli-linux-amd64" && \
    chmod +x /usr/local/bin/release-cli
