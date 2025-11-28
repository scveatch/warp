#!/usr/bin/env bash
set -euo pipefail

# ---------------------------------------- 
# Warp CLI Installer
# ---------------------------------------- 
# Downloads the latest release assets 
# (warp + warp.sh) and installs them to 
# ~/.local/bin. Ensures that warp.sh is 
# sourced and ready for future use. 
# ---------------------------------------- 

# Config
INSTALL_DIR="${HOME}/.local/bin"
TMP_TAR="tmp/warp.tar.gz"
REPO_API="https://api.github.com/repos/scveatch/warp/releases/latest"

echo "=== Warp CLI Installer ==="

# Check dependencies
for tool in curl tar; do 
    if ! command -v $tool >/dev/null 2>&1; then 
        echo "Error: $tool is required to install Warp."
        exit 1 
    fi 
done

# Ensure install directory
mkdir -p "${INSTALL_DIR}"

echo "Installing Warp to ${INSTALL_DIR}"

# Determine latest release tarbell
TAR_URL=$(curl -s "$REPO_API" \
    | grep "browser_download_url.*tar.gz" \
    | cut -d '"' -f 4)

# Download files
curl -sSL -o "${TMP_TAR}" "${TAR_URL}"
echo "Downloaded tar to ${TMP_TAR}"

# Unpack Tar 
tar -xzf "${TMP_TAR}" -C "${INSTALL_DIR}"

# Ensure Executable
chmod +x "${INSTALL_DIR}/warp" "${INSTALL_DIR}/warp.sh"

# Source warp.sh 
source "${INSTALL_DIR}/warp.sh"

# Update path for current session
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

if [ -n "$SHELL_RC" ]; then
    # Add source line if not already present
    if ! grep -Fxq "source \"$INSTALL_DIR/warp.sh\"" "$SHELL_RC"; then
        echo "" >> "$SHELL_RC"
        echo "# Warp CLI shell functions" >> "$SHELL_RC"
        echo "source \"$INSTALL_DIR/warp.sh\"" >> "$SHELL_RC"
        echo "Added source line to $SHELL_RC. Run 'source $SHELL_RC' or restart your shell."
    fi

    # Source now for immediate availability
    source "$INSTALL_DIR/warp.sh"
fi

# Cleanup
rm -f "$TMP_TAR"

echo "===================================="
echo "Warp CLI installation complete!"
echo "You can now run 'warp' from your shell."
echo "Try: warp add <name> <path>, warp list, warp <name>"
echo "===================================="
