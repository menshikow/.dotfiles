#!/usr/bin/env bash
set -euo pipefail

if [[ "$(uname)" == "Darwin" ]] && command -v brew >/dev/null 2>&1; then
  echo "Installing JetBrains Mono + Geist Mono via Homebrew..."
  brew install --cask font-jetbrains-mono font-geist-mono
  echo "Done."
  exit 0
fi

case "$(uname)" in
  Darwin) DST="$HOME/Library/Fonts" ;;
  Linux)  DST="$HOME/.local/share/fonts" ;;
  *) echo "Unsupported OS: $(uname)" >&2; exit 1 ;;
esac
mkdir -p "$DST"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fetch() {
  local url="$1" out="$2"
  echo "Downloading $url..."
  curl -fL --progress-bar -o "$out" "$url"
}

install_zip() {
  local zip="$1"
  local pattern="$2"
  unzip -q -o "$zip" -d "$TMP/unzip"
  find "$TMP/unzip" -type f -iname "$pattern" -exec cp -n {} "$DST/" \; -print
}

if command -v gh >/dev/null 2>&1; then
  JB_URL="$(gh api repos/JetBrains/JetBrainsMono/releases/latest --jq '.assets[] | select(.name | endswith(".zip")) | .browser_download_url' | head -n1)"
  GEIST_URL="https://github.com/vercel/geist-font/releases/latest/download/geist-mono.zip"
else
  JB_URL="$(curl -fsSL https://api.github.com/repos/JetBrains/JetBrainsMono/releases/latest | python3 -c 'import sys,json; d=json.load(sys.stdin); print(next(a["browser_download_url"] for a in d["assets"] if a["name"].endswith(".zip")))')"
  GEIST_URL="https://github.com/vercel/geist-font/releases/latest/download/geist-mono.zip"
fi

JB_ZIP="$TMP/JetBrainsMono.zip"
GEIST_ZIP="$TMP/GeistMono.zip"

fetch "$JB_URL" "$JB_ZIP"
fetch "$GEIST_URL" "$GEIST_ZIP"

echo "Installing JetBrains Mono..."
install_zip "$JB_ZIP" "*.ttf"
echo "Installing Geist Mono..."
install_zip "$GEIST_ZIP" "*.otf"
if ! ls "$DST"/GeistMono* >/dev/null 2>&1; then
  install_zip "$GEIST_ZIP" "*.ttf"
fi

if [[ "$(uname)" == "Linux" ]] && command -v fc-cache >/dev/null 2>&1; then
  fc-cache -f
fi

echo "Done. Installed to $DST"
ls -1 "$DST"/JetBrainsMono* "$DST"/GeistMono* 2>/dev/null | head -n 20
