#!/bin/bash
FONT_SRC="${DOTFILES:-$HOME/.dotfiles}/fonts"

case "$(uname)" in
  Darwin) FONT_DST="$HOME/Library/Fonts" ;;
  Linux)  FONT_DST="$HOME/.local/share/fonts" ;;
esac

mkdir -p "$FONT_DST"

shopt -s nullglob
for f in "$FONT_SRC"/*.{ttf,ttc,otf}; do
  cp -n "$f" "$FONT_DST/" && echo "Installed: $(basename "$f")"
done

case "$(uname)" in
  Linux) fc-cache -fv ;;
esac

echo "Done."
