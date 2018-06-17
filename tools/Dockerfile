# this file is used to build the image gpsbabel_build_environment used by travis.

FROM ubuntu:bionic

WORKDIR /app

# update environment, including held back packages that were found by hand.
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends apt-utils && apt-get install -y apt dpkg libapt-pkg5.0 && apt-get upgrade -y

# install packages needed for gpsbabel build
# split into multiple commands to limit layer size
RUN apt-get install -y g++ make autoconf gperf git valgrind
RUN apt-get install -y clang
RUN apt-get install -y expat fop xsltproc libxml2-utils docbook-xml docbook-xsl
RUN apt-get install -y libusb-dev
RUN apt-get install -y qt5-default libqt5webkit5-dev qttools5-dev-tools
# pkgs needed to generate coverage report:
RUN apt-get install -y gcovr openjdk-8-jre-headless

# install environment for locale test
RUN apt-get install -y locales && sed -i 's/^# *\(en_US ISO-8859-1\)/\1/' /etc/locale.gen && locale-gen && locale -a








