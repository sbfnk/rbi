FROM rocker/rstudio

LABEL maintainer="Sebastian Funk <sebastian.funk@lshtm.ac.uk>"

USER root

# libbi prerequisittes
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    libblas-dev \
    liblapack-dev \
    libqrupdate-dev \
    libboost-all-dev \
    libgsl0-dev \
    libnetcdf-dev \
    libxml2-dev \
    autoconf \
    automake \
    libthrust-dev \
    cpanminus \
    libmodule-install-perl \
    curl && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# install libbi
RUN curl -fLo /tmp/LibBi.tar.gz https://github.com/lawmurray/LibBi/archive/stable.tar.gz && \
    tar xzf /tmp/LibBi.tar.gz -C ~ && \
    cd ~/LibBi-stable && \
    cpanm --quiet --installdeps --notest . && \
    cpanm --quiet . && \
    cd .. && \
    rm /tmp/LibBi.tar.gz && \
    rm -rf ~/LibBi-stable

USER $NB_UID

# install R packages
RUN install2.r -e \
    data.table \
    ggplot2 \
    lubridate \
    scales \
    coda \
    GGally \
    ggmcmc \
    remotes \
    magrittr \
    tidyverse \
    ncdf4 && \
    rm -rf /tmp/downloaded_packages

RUN installGithub.r \
    sbfnk/rbi \
    sbfnk/rbi.helpers
 
USER root
