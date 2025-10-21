#!/bin/bash
set -e

UBUNTU_VERSION=${UBUNTU_VERSION:-focal}
CRAN_LINUX_VERSION=${CRAN_LINUX_VERSION:-cran40}
LANG=${LANG:-en_US.UTF-8}
LC_ALL=${LC_ALL:-en_US.UTF-8}
DEBIAN_FRONTEND=noninteractive

# Set up and install R
R_HOME=${R_HOME:-/usr/lib/R}

#R_VERSION=${R_VERSION}

sudo apt-get update

sudo apt-get -y install --no-install-recommends \
      ca-certificates \
      less \
      libopenblas-base \
      locales \
      vim-tiny \
      wget \
      dirmngr \
      gpg \
      gpg-agent

echo "Add the CRAN repository for Ubuntu Noble"
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/"


# Wildcard * at end of version will grab (latest) patch of requested version
sudo apt-get update && sudo apt-get -y install --no-install-recommends r-base-dev=${R_VERSION}*

## Add PPAs: NOTE this will mean that installing binary R packages won't be version stable.
##
## These are required at least for bionic-based images since 3.4 r binaries are

echo "en_US.UTF-8 UTF-8" | sudo tee -a /etc/locale.gen
sudo locale-gen en_US.utf8
sudo /usr/sbin/update-locale LANG=${LANG}

Rscript -e "install.packages(c('littler', 'docopt'))"

## By default R_LIBS_SITE is unset, and defaults to this, so this is where `littler` will be.
## We set it here for symlinks, but don't make the env var persist (since it's already the default)
R_LIBS_SITE=/usr/local/lib/R/site-library
sudo ln -s ${R_LIBS_SITE}/littler/examples/install.r /usr/local/bin/install.r
sudo ln -s ${R_LIBS_SITE}/littler/examples/install2.r /usr/local/bin/install2.r
sudo ln -s ${R_LIBS_SITE}/littler/examples/installGithub.r /usr/local/bin/installGithub.r
sudo ln -s ${R_LIBS_SITE}/littler/bin/r /usr/local/bin/r
