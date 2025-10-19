# 1. BASE IMAGE
# Start from a specific version of a rocker image for long-term reproducibility.
# rocker/verse includes the tidyverse and many common development tools.
FROM rocker/verse:latest

# 2. INSTALL SYSTEM DEPENDENCIES
# Install Linux libraries required by the R packages in renv.lock.
# This layer is cached and only re-runs if this section changes.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libv8-dev \
    make \
    && rm -rf /var/lib/apt/lists/*

# 3. SET UP RENV AND INSTALL PACKAGES (THE EFFICIENT WAY)
# This is the key section for caching. We copy only the renv files,
# install the packages, and then copy the rest of the source code.
# This way, the time-consuming package installation is cached as its own layer.
WORKDIR /work

# First, copy only the files renv needs to identify and install packages.
COPY renv.lock .
COPY .Rprofile .
COPY renv/activate.R renv/activate.R

# Install the renv package itself, then restore all dependencies from the lockfile.
# This creates a cached layer with all your version-locked R packages.
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# 4. COPY THE REST OF THE PROJECT SOURCE CODE
# Now that dependencies are installed, copy the rest of your package code.
# If you only change a file in R/, Docker will re-use the cached layer above
# and this step will be almost instant.
COPY . .

# 5. (OPTIONAL) SET A DEFAULT COMMAND
# For a development image, this is often left open, but you could, for example,
# run the package check as a default test.
# CMD ["Rscript", "-e", "devtools::check()"]
