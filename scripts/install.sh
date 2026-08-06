#!/usr/bin/env bash
# One-line Aether installer for Ubuntu/Debian Linux:
#   curl -sSf https://raw.githubusercontent.com/ignaciosgithub/Aether/main/scripts/install.sh | bash
set -euo pipefail

REPO_URL="https://github.com/ignaciosgithub/Aether.git"
DEST="${AETHER_HOME:-$HOME/Aether}"

echo "==> Installing system packages (sudo required)"
sudo apt-get update -y
sudo apt-get install -y git clang lld build-essential python3-tk curl

if ! command -v cargo >/dev/null 2>&1 && [ ! -x "$HOME/.cargo/bin/cargo" ]; then
  echo "==> Installing Rust via rustup"
  curl https://sh.rustup.rs -sSf | sh -s -- -y
fi
export PATH="$PATH:$HOME/.cargo/bin"

if [ -d "$DEST/.git" ]; then
  echo "==> Updating existing checkout at $DEST"
  git -C "$DEST" pull --ff-only
else
  echo "==> Cloning Aether into $DEST"
  git clone "$REPO_URL" "$DEST"
fi

echo "==> Building the compiler (release)"
cargo build --workspace --release --manifest-path "$DEST/Cargo.toml"

echo "==> Installing application-menu shortcut"
mkdir -p "$HOME/.local/share/applications"
cat > "$HOME/.local/share/applications/aether-editor.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=Aether Editor
Comment=Multi-tab Aether editor (More with less)
Exec=python3 $DEST/tools/aether_editor.py
Terminal=false
Categories=Development;IDE;
EOF

echo
echo "Aether installed."
echo "  - Editor:   python3 $DEST/tools/aether_editor.py  (or 'Aether Editor' in your app menu)"
echo "  - Setup UI: python3 $DEST/tools/aether_setup_gui.py"
echo "  - CLI:      $DEST/target/release/aetherc program.ae --arch x86_64 --os linux -o program.s"

if [ -n "${DISPLAY:-}${WAYLAND_DISPLAY:-}" ]; then
  echo "==> Launching the editor"
  nohup python3 "$DEST/tools/aether_editor.py" >/dev/null 2>&1 &
fi
