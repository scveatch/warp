#!/usr/bin/env bash
set -euo pipefail

# Warp Installer Script
# 
# Downloads the latest warp.sh and warp-bin release assets from 
# Github and installs them to ~/.local/bin

# Config
INSTALL_DIR="${HOME}/.local/bin"
REPO="scveatch/warp"
FILES=("warp.sh" "warp-bin")

# Check for curl existence:
if ! command -v curl >/dev/null 2>&1; then
    echo "Error: curl is required to install Warp."
    exit 1
fi

# Ensure install directory
mkdir -p "${INSTALL_DIR}"

echo "Installing Warp to ${INSTALL_DIR}"

# Download files
for f in "${FILES[@]}"; do 
    URL="https://github.com/${REPO}/releases/latest/download/${f}"
    TARGET="${INSTALL_DIR}/${f}"
    echo "Downloading ${f}..."
    curl -sL "${URL}" -o "${TARGET}"
    chmod +x "${TARGET}"
done 

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
