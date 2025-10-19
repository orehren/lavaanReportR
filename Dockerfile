# 1. BASE IMAGE
# Use a well-maintained base image from the Rocker project.
# Single colon is the correct syntax.
FROM rocker/verse:latest

# 2. SYSTEM DEPENDENCIES
# Install Linux libraries needed by the R packages.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libv8-dev \
    make \
    && rm -rf /var/lib/apt/lists/*

# 3. R PACKAGES
# Install all R dependencies needed for development and for the package itself.
RUN R -e "install.packages(c('remotes', 'R6'), repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_cran(c('lavaan', 'igraph', 'DiagrammeR', 'devtools', 'roxygen2',
'usethis', 'testthat', 'knitr', 'rmarkdown', 'here', 'data.table', 'future', 'doFuture', 'future.apply', 'psych'))"

# 4. SET A DEFAULT WORKING DIRECTORY
# This will be the location where Jules clones the repository content into.
WORKDIR /work
