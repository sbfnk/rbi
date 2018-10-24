## Start with the shiny docker image
FROM rocker/tidyverse:latest

## Get thrust
RUN wget https://github.com/thrust/thrust/releases/download/1.8.2/thrust-1.8.2.zip \
    && unzip thrust-1.8.2.zip\
    && mv thrust /usr/local/include

## Set env for Libbi
ENV PERL_MM_USE_DEFAULT=1

## Get LibBi (and deps)
RUN git clone https://github.com/lawmurray/LibBi.git \
  && cd LibBi \
  && sudo apt-get install -y \
    libblas-dev \
    liblapack-dev \
    libqrupdate-dev \
    libboost-all-dev \
    libgsl0-dev \
    libnetcdf-dev \
    autoconf \
    automake \
  && sudo cpan .

## Get latex libs for package install + CRAN
RUN apt-get install -y \
    texlive-latex-recommended \
    texlive-fonts-extra \
    texinfo \
    libqpdf-dev \
    && apt-get clean
    
## Add package files to repo  
ADD . /home/rstudio/RBI

##Install the dev deps
RUN Rscript -e 'devtools::install_dev_deps("home/rstudio/RBI")'

## Get pkgdown for site building
RUN Rscript -e 'devtools::install_github("r-lib/pkgdown")'

## Install the package
RUN Rscript -e 'devtools::install("home/rstudio/RBI")'