#!/bin/bash -e
commondims=(16 22 24 32 48 64 128 256)
for dim in "${commondims[@]}"
do
  dims=${dim}x${dim}
  mkdir -p "images/hicolor/${dims}/apps"
  cp images/appicon.png "images/appicon-${dims}.png"
  mogrify -resize "${dims}>" -background none -gravity center -extent "${dims}" "images/appicon-${dims}.png"
done
