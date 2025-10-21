# Start from a base image that already has R installed.
# The 'rocker' project provides excellent, versioned R images.
FROM rocker/r-ver:4.3.1

# Install any system-level dependencies your R packages might need.
# For example, DiagrammeR sometimes needs graphviz.
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    graphviz

# Copy your package source code into the container.
COPY . /lavaanReportR
WORKDIR /lavaanReportR

# Install the R packages required by your project.
# This command uses the 'remotes' package to read the DESCRIPTION file
# and install all listed dependencies (Imports and Suggests).
RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org/')"
RUN R -e "remotes::install_deps(dependencies = TRUE)"

# Finally, install your local package itself.
RUN R CMD INSTALL .

# Set the working directory again in case R CMD INSTALL changed it.
WORKDIR /lavaanReportR
