FROM ubuntu:18.04

ARG BUILD_DATE
ARG VCS_REF
LABEL org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.name="RStable" \
      org.label-schema.description="A docker image with stable versions of R and a bunch of package. The full list of packages is available in the README." \
      org.label-schema.license="MIT" \
      org.label-schema.url="e.g. https://www.inbo.be/" \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.vcs-url="https://github.com/inbo/RLaTeX" \
      org.label-schema.vendor="Research Institute for Nature and Forest" \
      maintainer="Thierry Onkelinx <thierry.onkelinx@inbo.be>"

## for apt to be noninteractive
ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN true


## Set a default user. Available via runtime flag `--user docker`
## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
## User should also have & own a home directory (for rstudio or linked volumes to work properly).
RUN useradd docker \
  && mkdir /home/docker \
  && chown docker:docker /home/docker \
  && addgroup docker staff

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN apt-get update \
  && apt-get install -y  --no-install-recommends \
    locales \
  && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen en_US.utf8 \
  && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

## Install R dependencies
RUN sed -i.bak "/^#.*deb-src.*universe$/s/^# //g" /etc/apt/sources.list \
&&  apt-get update \
&&  apt-get -y --no-install-recommends build-dep r-base

## Install R devel
RUN apt-get install -y --no-install-recommends wget \
&&  wget --no-check-certificate https://stat.ethz.ch/R/daily/R-devel.tar.gz \
&&  tar -xvf R-devel.tar.gz \
&&  cd R-devel \
&&  ./configure \
    --enable-memory-profiling \
    --enable-R-shlib \
    --with-blas \
    --with-lapack \
&&  make \
&&  make install

## Install devtools
RUN apt-get update \
&&  apt-get install -y --no-install-recommends \
      libxml2-dev \
      libssl-dev \
      libcurl4-openssl-dev \
      git \
      qpdf \
&&  Rscript -e 'install.packages(c("devtools", "dplyr", "ggplot2", "knitr", "rmarkdown", "sp", "tidyr"), repos = c(CRAN = "https://cloud.r-project.org"))'

## Install dependencies
RUN Rscript -e 'install.packages(c("dplyr", "ggplot2", "knitr", "rmarkdown", "sp", "tidyr"), repos = c(CRAN = "https://cloud.r-project.org"))' \
&&  Rscript -e 'install.packages("INLA", repos = c(CRAN = "https://cloud.r-project.org", INLA = "https://inla.r-inla-download.org/R/stable"))'

## Install lintr
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com"))' >.Rprofile \
&&  Rscript -e 'install.packages("lintr")' \
