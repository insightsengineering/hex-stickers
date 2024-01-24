#!/bin/bash

ICON_FOLDER="../ICO"

mkdir -p "$ICON_FOLDER"

shopt -s globstar
for img in ../PNG/*.png; do
  new_ico=$(basename $img)
  new_ico=${new_ico%.png}.ico


  convert -resize x256 -gravity center -crop 256x256+0+0 "$img" -background transparent -extent 256x256 "$ICON_FOLDER/$new_ico"
done
