#!/bin/bash
# This script is sourced by the main setup.sh script.
# It installs the tidyverse suite and its dependencies.

# --- 1. Install System Dependencies ---
echo "Info: Installing tidyverse system dependencies..."

tidyverse_deps=(
    libxml2-dev
    libcairo2-dev
    libgit2-dev
    default-libmysqlclient-dev
    libpq-dev
    libsasl2-dev
    libsqlite3-dev
    libssh2-1-dev
    libxtst6
    libcurl4-openssl-dev
    libharfbuzz-dev
    libfribidi-dev
    libfreetype6-dev
    libpng-dev
    libtiff5-dev
    libjpeg-dev
    unixodbc-dev
    xz-utils
)
# apt_install is defined in the main script setup_r.sh
apt_install "${tidyverse_deps[@]}"


# --- 2. Install R Packages ---
echo "Info: Installing core R packages (tidyverse, devtools, db backends)..."

# This R code tries to incorporate the following functionality:
# 1. Uses NCPUS (from the environment variable) for parallel computing setup
# 2. Skips already installed packages (setdiff)
# 3. Skips missing packages (by checking availability)
# 4. Uses the repositories from Rprofile.site (automatically)

Rscript - <<EOF
# --- Setup ---
# 1. Set parallel cores
ncpus <- as.integer(Sys.getenv("NCPUS", "-1"))
if (ncpus == -1) {
    # -1 bedeutet, alle verfügbaren Kerne zu verwenden
    ncpus <- parallel::detectCores()
}
options(Ncpus = ncpus)
cat("Using", ncpus, "cores for R package installation.\n")

# 2. Get all currently installed packages
installed <- rownames(installed.packages())

# 3. Get all available packages from the repo
avail <- rownames(available.packages())


# --- Helper Function ---
smart_install <- function(pkg_list) {
    # 1. Find packages, that are not already installed yet.
    to_install <- setdiff(pkg_list, installed)

    if (length(to_install) == 0) {
        cat("All packages in this group are already installed.\n")
        return()
    }

    # 2. Find all packages, that are available in the repositories.
    available_to_install <- intersect(to_install, avail)

    # 3. Gives a Message, if packages are omitted.
    if (length(available_to_install) < length(to_install)) {
        missing <- setdiff(to_install, available_to_install)
        cat("Warning: Skipping missing packages:", paste(missing, collapse=", "), "\n")
    }

    # 4. Install packages
    if (length(available_to_install) > 0) {
        cat("Installing:", paste(available_to_install, collapse=", "), "\n")
        install.packages(available_to_install, ask = FALSE, checkBuilt = TRUE)
    }
}

# --- Block 1: Core Packages ---
cat("\n--- Installing Core Packages ---\n")
core_pkgs <- c(
    "tidyverse", "devtools", "rmarkdown",
    "BiocManager", "vroom", "gert"
)
smart_install(core_pkgs)

# --- Block 2: Database Backends ---
cat("\n--- Installing Database Packages ---\n")
db_pkgs <- c(
    "arrow", "dbplyr", "DBI", "dtplyr", "duckdb",
    "nycflights13", "Lahman", "RMariaDB", "RPostgres",
    "RSQLite", "fst"
)
smart_install(db_pkgs)

EOF

# --- 3. Final Check ---
echo -e "\nChecking tidyverse installation..."

R -q -e "library(tidyverse)"

echo "Info: Tidyverse core installation complete."
