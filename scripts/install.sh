#!/usr/bin/env bash
set -euo pipefail

# Warp Installer Script
# 
# Downloads the latest warp.sh and warp release assets from 
# Github and installs them to ~/.local/bin

# Config
INSTALL_DIR="${HOME}/.local/bin"
REPO=$(curl -s "https://api.github.com/repos/scveatch/warp/releases/latest" | \
    grep "browser_download_url.*tar.gz" | \
    cut -d '"' -f 4)

# Check for curl existence:
if ! command -v curl >/dev/null 2>&1; then
    echo "Error: curl is required to install Warp."
    exit 1
fi

# Ensure install directory
mkdir -p "${INSTALL_DIR}"

echo "Installing Warp to ${INSTALL_DIR}"

# Download files
curl -sSL -o /tmp/warp.tar.gz "${REPO}"
echo "Downloaded tar to /tmp/warp.tar.gz"

# Unpack Tar 
tar -xzf /tmp/warp.tar.gz -C "${INSTALL_DIR}"

# Ensure Executable
chmod +x "${INSTALL_DIR}/warp" "${INSTALL_DIR}/warp.sh"

# Add install dir to PATH for this session if not already present
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    export PATH="$INSTALL_DIR:$PATH"
    echo "Added $INSTALL_DIR to PATH for current session."
fi

# Add to shell rc for future sessions
SHELL_RC=""
if [ -n "$BASH_VERSION" ]; then
    SHELL_RC="${HOME}/.bashrc"
elif [ -n "$ZSH_VERSION" ]; then
    SHELL_RC="${HOME}/.zshrc"
fi

if [ -n "$SHELL_RC" ] && ! grep -q "$INSTALL_DIR" "$SHELL_RC"; then
    echo "export PATH=\"$INSTALL_DIR:\$PATH\"" >> "$SHELL_RC"
    echo "Added $INSTALL_DIR to PATH in $SHELL_RC. Restart your shell or run 'source $SHELL_RC'."
fi

echo "Warp installation complete! You can now run 'warp' from your terminal."
