#!/bin/bash
# This script is sourced by the main setup.sh script.
# It installs the Quarto CLI and its specific dependencies.

# --- 1. Install Dependencies ---
echo "Info: Installing Quarto system dependencies..."

quarto_packages=(
    wget
    ca-certificates
)
apt_install "${quarto_packages[@]}"


# --- 2. Define Variables ---
DOWNLOAD_FILE="quarto.deb"

# Map architecture (uname -m gives x86_64, Quarto URLs use amd64)
if [ "$ARCH" = "x86_64" ]; then
    QUARTO_ARCH="amd64"
elif [ "$ARCH" = "aarch64" ]; then
    QUARTO_ARCH="arm64"
else
    echo "Error: Unsupported architecture for Quarto: $ARCH" >&2
    exit 1
fi

echo "Info: Downloading Quarto CLI $QUARTO_VERSION for $QUARTO_ARCH..."

# --- 3. Download Logic ---
if [ "$QUARTO_VERSION" = "latest" ]; then
    wget "https://quarto.org/download/latest/quarto-linux-${QUARTO_ARCH}.deb" -O "$DOWNLOAD_FILE"
else
    wget "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${QUARTO_ARCH}.deb" -O "$DOWNLOAD_FILE"
fi

# --- 4. Install ---
echo "Info: Installing .deb package..."
sudo gdebi --non-interactive "$DOWNLOAD_FILE"
rm "$DOWNLOAD_FILE"

echo "Info: Quarto CLI installation complete."
