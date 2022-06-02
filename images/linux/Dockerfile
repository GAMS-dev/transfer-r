# Build from the MATLAB base image
FROM rocker/r-ver

# install gitlab release-cli
RUN sudo curl -L --output /usr/local/bin/release-cli \
    "https://release-cli-downloads.s3.amazonaws.com/latest/release-cli-linux-amd64" && \
    sudo chmod +x /usr/local/bin/release-cli