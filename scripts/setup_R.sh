#!/bin/bash
# This script installs and configures R, OpenBLAS, and optional tools
# (RStudio, Quarto, Tidyverse) on a Debian-based system (like Ubuntu).
# It acts as an orchestrator, sourcing modular scripts for complex installs.

set -e

# ==============================================================================
# 1. PREPARATION AND HELPER FUNCTIONS
# ==============================================================================
echo "#### 1. Script Preparation and Definitions ####"

# --- NEW: Define robust script paths ---
# Get the absolute path to the directory where this script is located
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
# Define the project root as the parent directory of the script dir
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

# --- Script Configuration & Argument Parsing ---

# 1. Set Defaults
R_VERSION_DEFAULT="4.5.1"
USE_QUARTO_DEFAULT="false"
USE_RSTUDIO_DEFAULT="false"
USE_TIDYVERSE_DEFAULT="false"
RSTUDIO_VERSION_DEFAULT="stable"
QUARTO_VERSION_DEFAULT="latest"
NCPUS_DEFAULT="-1" # -1 means use all available cores

# 2. Initialize from defaults
R_VERSION="$R_VERSION_DEFAULT"
USE_QUARTO="$USE_QUARTO_DEFAULT"
USE_RSTUDIO="$USE_RSTUDIO_DEFAULT"
USE_TIDYVERSE="$USE_TIDYVERSE_DEFAULT"
RSTUDIO_VERSION="$RSTUDIO_VERSION_DEFAULT"
QUARTO_VERSION="$QUARTO_VERSION_DEFAULT"
NCPUS="$NCPUS_DEFAULT"

# 3. Parse command-line arguments to override defaults
while [[ $# -gt 0 ]]; do
  case "$1" in
    --r-version=*)
      R_VERSION="${1#*=}"
      shift
      ;;
    --r-version)
      if [[ -z "$2" || "$2" == --* ]]; then
        echo "Error: --r-version requires an argument." >&2; exit 1
      fi
      R_VERSION="$2"
      shift 2
      ;;
    --rstudio-version=*)
      RSTUDIO_VERSION="${1#*=}"
      shift
      ;;
    --rstudio-version)
      if [[ -z "$2" || "$2" == --* ]]; then
        echo "Error: --rstudio-version requires an argument." >&2; exit 1
      fi
      RSTUDIO_VERSION="$2"
      shift 2
      ;;
    --quarto-version=*)
      QUARTO_VERSION="${1#*=}"
      shift
      ;;
    --quarto-version)
      if [[ -z "$2" || "$2" == --* ]]; then
        echo "Error: --quarto-version requires an argument." >&2; exit 1
      fi
      QUARTO_VERSION="$2"
      shift 2
      ;;
    --use-quarto)
      USE_QUARTO="true"
      shift
      ;;
    --use-rstudio)
      USE_RSTUDIO="true"
      shift
      ;;
    --use-tidyverse)
      USE_TIDYVERSE="true"
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [options]"
      echo "Options:"
      echo "  --r-version=<ver>       Set R version (default: $R_VERSION_DEFAULT)"
      echo "  --rstudio-version=<ver> Set RStudio version (default: $RSTUDIO_VERSION_DEFAULT)"
      echo "  --quarto-version=<ver>  Set Quarto version (default: $QUARTO_VERSION_DEFAULT)"
      echo "  --use-quarto            Install Quarto & dependencies (default: $USE_QUARTO_DEFAULT)"
      echo "  --use-rstudio           Install RStudio & dependencies (default: $USE_RSTUDIO_DEFAULT)"
      echo "  --use-tidyverse         Install tidyverse & core packages (default: $USE_TIDYVERSE_DEFAULT)"
      echo "  -h, --help              Show this help message"
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      echo "Use --help for usage." >&2
      exit 1
      ;;
  esac
done

# 4. Export the final variables (for use in sourced scripts)
export R_VERSION
export USE_QUARTO
export USE_RSTUDIO
export USE_TIDYVERSE
export RSTUDIO_VERSION
export QUARTO_VERSION
export NCPUS

# --- Environment Variables (sensible defaults) ---
export LANG=${LANG:-en_US.UTF-8}
export LC_ALL=${LC_ALL:-en_US.UTF-8}
export ARCH=$(uname -m)
export DEBIAN_FRONTEND=noninteractive

# --- Log the final configuration ---
echo "--- Configuration ---"
echo "R Version:         $R_VERSION"
echo "RStudio Version:   $RSTUDIO_VERSION"
echo "Quarto Version:    $QUARTO_VERSION"
echo "Install Quarto:    $USE_QUARTO"
echo "Install RStudio:   $USE_RSTUDIO"
echo "Install Tidyverse: $USE_TIDYVERSE"
echo "R Install Cores:   $NCPUS"
echo "Script Directory:  $SCRIPT_DIR"
echo "Project Root:      $PROJECT_ROOT"
echo "---------------------"

echo 'debconf debconf/frontend select Noninteractive' | sudo debconf-set-selections

# --- Idempotent Package Installation Function ---
# This function is used here and in all sourced scripts.
function apt_install() {
    # --- STEP 0: Do nothing if no packages were requested ---
    if [ $# -eq 0 ]; then
        return 0
    fi

    # --- Map basenames to original full package strings ---
    declare -A original_pkg_map
    local basenames_list=()
    local pkg
    for pkg in "$@"; do
        local basename
        basename=$(echo "$pkg" | sed -E 's/[=<>~].*//')
        original_pkg_map["$basename"]="$pkg"
        basenames_list+=("$basename")
    done

    # --- STEP 1: Get all lists (sorted) ---
    local requested_basenames_sorted
    requested_basenames_sorted=$(printf "%s\n" "${basenames_list[@]}" | sort -u)
    local all_available_packages
    all_available_packages=$(apt-cache pkgnames | sort)
    local all_installed_packages
    all_installed_packages=$(dpkg-query -W -f='${Package}\n' | sort)

    # --- STEP 2: Find the intersections (Set logic on BASE names) ---
    local valid_basenames
    valid_basenames=$(comm -12 <(echo "${requested_basenames_sorted}") <(echo "${all_available_packages}"))
    local basenames_to_install_nl
    basenames_to_install_nl=$(comm -23 <(echo "${valid_basenames}") <(echo "${all_installed_packages}"))
    if [ -z "${basenames_to_install_nl}" ]; then
        return 0
    fi

    # --- STEP 3: Build final list & Install ---
    local final_packages_to_install=()
    local basename_to_install
    while IFS= read -r basename_to_install; do
        final_packages_to_install+=("${original_pkg_map["$basename_to_install"]}")
    done < <(echo "${basenames_to_install_nl}")
    if [ ${#final_packages_to_install[@]} -eq 0 ]; then
        return 0
    fi
    [ "$(sudo find /var/lib/apt/lists/* | wc -l)" = "0" ] && sudo apt-get -q update
    sudo apt-get install -qq --no-install-recommends --ignore-missing "${final_packages_to_install[@]}"
}

# ==============================================================================
# 2. SYSTEM PREREQUISITES AND PACKAGE REPOSITORIES
# ==============================================================================
echo "#### 2. Update Base System and Install Prerequisites"

sudo apt-get -qq update && sudo apt-get -qq upgrade

# --- REFACTORED: Base packages only ---
# These are the core dependencies for R and the installer tools.
# gdebi-core is needed by install-rstudio.sh and install-quarto.sh
packages_to_install=(
    software-properties-common
    ca-certificates
    less
    locales
    vim-tiny
    wget
    dirmngr
    gpg
    gpg-agent
    gdebi-core # Needed by sub-scripts to install .deb files
    libpcre2-dev
    libdeflate-dev
    liblzma-dev
    libbz2-dev
    zlib1g-dev
    libzstd-dev
    libicu-dev
    libsass-dev
    libglpk-dev
    pandoc
    libopenblas-dev
    libcurl4-openssl-dev
    libgit2-dev
    libxml2-dev
    libwebpmux3
)
# Dependencies for RStudio, Quarto, and Tidyverse are in their respective scripts.

echo "Installing base system dependencies..."
apt_install "${packages_to_install[@]}"
# --- End of base package installation ---

# Add the CRAN GPG key and repository
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the repo from CRAN -- lsb_release adjusts to 'noble' or 'jammy' or ... as needed
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

sudo apt-get -qq update

# ==============================================================================
# 3. OPENBLAS CONFIGURATION (ROBUST METHOD)
# ==============================================================================
echo "#### 3. Configure OpenBLAS"

DESIRED_BLAS_PATH="/usr/lib/${ARCH}-linux-gnu/openblas-pthread/libblas.so.3"
LINK_GROUP="libblas.so.3-${ARCH}-linux-gnu"

if ! sudo update-alternatives --display "${LINK_GROUP}" | grep -q "link currently points to ${DESIRED_BLAS_PATH}"; then
    echo "Info: Setting OpenBLAS as the default BLAS implementation..."
    sudo update-alternatives --set "${LINK_GROUP}" "${DESIRED_BLAS_PATH}"
fi

# ==============================================================================
# 4. R INSTALLATION AND LOCALE CONFIGURATION
# ==============================================================================
echo "#### 4. Install R and Configure Locales"

apt_install r-base-dev=${R_VERSION}*

echo "en_US.UTF-8 UTF-8" | sudo tee /etc/locale.gen
sudo locale-gen en_US.utf8 >/dev/null
sudo /usr/sbin/update-locale LANG=${LANG}

# ==============================================================================
# 5. R CONFIGURATION (Rprofile.site)
# ==============================================================================
echo "#### 5. Configure R Globally (Rprofile.site)"

export R_HOME=$(R RHOME)

sudo tee -a "${R_HOME}/etc/Rprofile.site" >/dev/null <<'EOF'
  # Configure R to use posit package manager binaries
  repos_list <- c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest",
    PPM = "https://packagemanager.posit.co/all/__linux__/noble/latest",
    PPM_source = "https://packagemanager.posit.co/all/latest",
    CRAN_source    = "https://cloud.r-project.org"
  )

  r <- getOption("repos")

  r[names(repos_list)] <- repos_list

  options(repos = r)
  options(renv.config.repos.override = r)

  # Configure the the R user agent header
  options(HTTPUserAgent = sprintf("R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version$platform, R.version$arch, R.version$os)
      ))

  # Configure renv to use wget
  options(download.file.method = "wget")

  # Configure wget user agent
  options(download.file.extra = sprintf('--header "User-Agent: R (%s)"', paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
EOF

# ==============================================================================
# 6. RESTORE PACKAGE LIBRARY
# ==============================================================================
# Use the robust $PROJECT_ROOT variable to find the lock file
if [ -f "$PROJECT_ROOT/renv.lock" ]; then
    echo "Info: renv.lock found. Restoring package library..."
    # 'renv::install()' must be run from the project root.
    # We also need sudo to write to the system-wide R library.
    (cd "$PROJECT_ROOT" && Rscript -e 'renv::install(repos = getOption("renv.config.repos.override"))')
fi

# ==============================================================================
# 7. RSTUDIO SERVER INSTALLATION (OPTIONAL)
# ==============================================================================
if [[ "${USE_RSTUDIO}" == "true" ]]; then
    echo "#### 7. Sourcing RStudio Server installer ####"
    # This script will install RStudio dependencies and the server .deb
    # Use the robust $SCRIPT_DIR variable for the path
    source "$SCRIPT_DIR/install-rstudio.sh"
fi

# ==============================================================================
# 8. QUARTO CLI INSTALLATION (OPTIONAL)
# ==============================================================================
if [[ "${USE_QUARTO}" == "true" ]]; then
    echo "#### 8. Sourcing Quarto CLI installer ####"
    # This script will install Quarto dependencies and the CLI .deb
    # Use the robust $SCRIPT_DIR variable for the path
    source "$SCRIPT_DIR/install-quarto.sh"
fi

# ==============================================================================
# 9. TIDYVERSE CORE INSTALLATION (OPTIONAL)
# ==============================================================================
if [[ "${USE_TIDYVERSE}" == "true" ]]; then
    echo "#### 9. Sourcing tidyverse installer ####"
    # This script will install tidyverse dependencies and R packages
    # Use the robust $SCRIPT_DIR variable for the path
    source "$SCRIPT_DIR/install-tidyverse.sh"
fi

# ==============================================================================
# 10. FINAL CLEANUP
# ==============================================================================
echo "#### 10. Final System Cleanup ####"

# Clean up apt caches to reduce image size
sudo rm -rf /var/lib/apt/lists/*
# Clean up temporary R package download files
rm -rf /tmp/downloaded_packages

# Strip R package binaries to reduce image size
if [[ -d "/usr/local/lib/R/site-library" ]]; then
    echo "Stripping R package binaries..."
    # Find .so files and ignore errors if none are found
    sudo find /usr/local/lib/R/site-library -type f -name "*.so" -exec strip {} + 2>/dev/null || true
fi

echo "--- Setup Complete ---"
