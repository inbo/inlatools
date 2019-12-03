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

## Add apt-get repositories
RUN apt-get update \
&&  apt-get install -y --no-install-recommends \
      gnupg \
      ca-certificates \
&&  sh -c 'echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list' \
&&  gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
&&  gpg -a --export E298A3A825C0D65DFD57CBB651716619E084DAB9 | apt-key add -

## Install R base
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    r-base-dev

## Install devtools
RUN apt-get update \
&&  apt-get install -y --no-install-recommends \
      libxml2-dev \
      libssl-dev \
      libcurl4-openssl-dev \
      git \
      qpdf \
&&  echo 'options(repos = c(CRAN = "https://cran.rstudio.com"))' >.Rprofile \
&&  Rscript -e 'install.packages("devtools")'

## Install dependencies
RUN Rscript -e 'install.packages(c("dplyr", "ggplot2", "knitr", "rmarkdown", "sp", "tidyr"))' \
&&  Rscript -e 'install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"))'

## Install lintr
RUN Rscript -e 'install.packages(c("codetools", "cyclocomp", "xmlparsedata", "lintr"))'

## Install pkgdown
RUN Rscript -e 'install.packages("pkgdown")'
