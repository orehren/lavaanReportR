#!/bin/bash
# This script is sourced by the main setup.sh script.
# It installs RStudio Server and its specific dependencies.

# --- 1. Install Dependencies ---
echo "Info: Installing RStudio system dependencies..."

# lsb-release is needed for $(lsb_release -cs)
rstudio_packages=(
    libclang-dev
    lsb-release
    psmisc
    pwgen
)
apt_install "${rstudio_packages[@]}"


# --- 2. Define Variables ---
DOWNLOAD_FILE="rstudio-server.deb"
export UBUNTU_CODENAME=$(lsb_release -cs)

# --- 3. Download Logic (REVISED AND FIXED) ---

# Alias for user-friendliness
if [ "$RSTUDIO_VERSION" = "latest" ]; then
    RSTUDIO_VERSION="stable"
fi

# --- NEW: Architecture Mapping ---
# Map system arch (x86_64) to RStudio's naming (amd64)
if [ "$ARCH" = "x86_64" ]; then
    RSTUDIO_ARCH="amd64"
elif [ "$ARCH" = "aarch64" ]; then
    RSTUDIO_ARCH="arm64"
else
    echo "Error: Unsupported architecture for RStudio: $ARCH" >&2
    exit 1
fi

# --- Rocker Compatibility Shims ---
# RStudio Server for noble (24.04) uses the jammy (22.04) packages.
if [ "$UBUNTU_CODENAME" = "noble" ]; then
    echo "Info: Mapping 'noble' to 'jammy' for RStudio Server download."
    UBUNTU_CODENAME="jammy"
fi

# Shim for older Ubuntu: map focal to bionic for specific version downloads
if [ "$UBUNTU_CODENAME" = "focal" ]; then
    UBUNTU_CODENAME="bionic"
fi

echo "Info: Downloading RStudio $RSTUDIO_VERSION for $UBUNTU_CODENAME ($RSTUDIO_ARCH)..."

# --- FIXED URLS ---

# Download logic for stable/preview/daily
if [ "$RSTUDIO_VERSION" = "stable" ] || [ "$RSTUDIO_VERSION" = "preview" ] || [ "$RSTUDIO_VERSION" = "daily" ]; then
    if [ "$UBUNTU_CODENAME" = "bionic" ]; then
        UBUNTU_CODENAME="focal"
    fi
    wget "https://rstudio.org/download/latest/${RSTUDIO_VERSION}/server/${UBUNTU_CODENAME}/rstudio-server-latest-${RSTUDIO_ARCH}.deb" -O "$DOWNLOAD_FILE"
else
    wget "https://download2.rstudio.org/server/${UBUNTU_CODENAME}/${RSTUDIO_ARCH}/rstudio-server-${RSTUDIO_VERSION/"+"/"-"}-${RSTUDIO_ARCH}.deb" -O "$DOWNLOAD_FILE" ||
        wget "https://s3.amazonaws.com/rstudio-ide-build/server/${UBUNTU_CODENAME}/${RSTUDIO_ARCH}/rstudio-server-${RSTUDIO_VERSION/"+"/"-"}-${RSTUDIO_ARCH}.deb" -O "$DOWNLOAD_FILE"
fi

# --- 4. Install ---
echo "Info: Installing .deb package..."
sudo gdebi --non-interactive "$DOWNLOAD_FILE"
rm "$DOWNLOAD_FILE"

# --- 5. Post-Install Configuration ---
# (This section is unchanged)
echo "Info: Configuring RStudio Server..."
sudo ln -fs /usr/lib/rstudio-server/bin/rstudio-server /usr/local/bin
sudo ln -fs /usr/lib/rstudio-server/bin/rserver /usr/local/bin
sudo rm -f /var/lib/rstudio-server/secure-cookie-key
sudo mkdir -p /etc/R
R_BIN="$(which R)"
echo "rsession-which-r=${R_BIN}" | sudo tee /etc/rstudio/rserver.conf > /dev/null
echo "lock-type=advisory" | sudo tee /etc/rstudio/file-locks > /dev/null
sudo cp /etc/rstudio/rserver.conf /etc/rstudio/disable_auth_rserver.conf
echo "auth-none=1" | sudo tee -a /etc/rstudio/disable_auth_rserver.conf > /dev/null
sudo mkdir -p /etc/rstudio/
cat <<EOF | sudo tee /etc/rstudio/logging.conf > /dev/null
[*]
log-level=warn
logger-type=syslog
EOF
if [ -n "$CUDA_HOME" ]; then
    sed -i '/^rsession-ld-library-path/d' /etc/rstudio/rserver.conf
    echo "rsession-ld-library-path=$LD_LIBRARY_PATH" | sudo tee -a /etc/rstudio/rserver.conf > /dev/null
fi

echo "Info: RStudio Server installation complete."
echo "Run 'rserver' to start the server."
