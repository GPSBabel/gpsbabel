# this file is used to build the image gpsbabel_build_environment used by travis.

FROM ubuntu:bionic

LABEL maintainer="https://github.com/tsteven4"

WORKDIR /app

# update environment.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \
 && apt-get upgrade -y \
 && rm -rf /var/lib/apt/lists/*

# install packages needed for gpsbabel build
# split into multiple commands to limit layer size

# basic build and test tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    g++ \
    make \
    autoconf \
    gperf \
    git \
    valgrind \
    expat \
    libxml2-utils \
    bear \
 && rm -rf /var/lib/apt/lists/*

# alternative compiler
RUN apt-get update && apt-get install -y --no-install-recommends \
    clang \
 && rm -rf /var/lib/apt/lists/*

# pkgs needed to build document
RUN apt-get update && apt-get install -y --no-install-recommends \
    fop \
    xsltproc \
    docbook-xml \
    docbook-xsl \
 && rm -rf /var/lib/apt/lists/*

# pkgs with libraries needed by gpsbabel
RUN apt-get update && apt-get install -y --no-install-recommends \
    libusb-dev \
    pkg-config \
    libudev-dev \
 && rm -rf /var/lib/apt/lists/*

# pkgs with qt used by gpsbabel
RUN apt-get update && apt-get install -y --no-install-recommends \
    qt5-default \
    libqt5webkit5-dev \
    qttools5-dev-tools \
    qttranslations5-l10n \
 && rm -rf /var/lib/apt/lists/*

# pkgs needed to generate coverage report:
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcovr \
 && rm -rf /var/lib/apt/lists/*

# install environment for locale test
RUN apt-get update && apt-get install -y --no-install-recommends \
    locales \
 && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/^# *\(en_US ISO-8859-1\)/\1/' /etc/locale.gen \
 && locale-gen \
 && locale -a

