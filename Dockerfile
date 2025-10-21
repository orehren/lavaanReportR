# Start from a base image that already has R installed.
# Using the same version for consistency.
FROM rocker/r-ver:4.5.1

RUN echo 'options(repos = c(P3M = "https://packagemanager.posit.co/cran/__linux__/noble/latest", CRAN = "https://cloud.r-project.org"))' >>"${R_HOME}/etc/Rprofile.site"

# These are common dependencies for packages that compile from source.
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    graphviz

# --- renv Restore Phase ---
# First, copy only the files necessary for renv to restore the environment.
# This allows Docker to cache this layer effectively. If these files don't change,
# the expensive package installation step won't be re-run.
WORKDIR /lavaanReportR
COPY renv.lock .
COPY .Rprofile .
COPY renv/activate.R renv/
COPY renv/settings.dcf renv/

# Install renv and restore the project's R dependencies from the lockfile.
# This will install the exact versions of packages specified in renv.lock.
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org/')"
RUN R -e "renv::restore()"

# --- Application Code Phase ---
# Now that dependencies are installed, copy the rest of the application code.
COPY . .

# Finally, install the local package itself into the renv library.
RUN R -e "renv::install(project = '.', rebuild = TRUE)"

# The WORKDIR is already set, so this is the final state.
