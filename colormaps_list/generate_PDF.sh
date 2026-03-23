#!/bin/bash
# Requires ImageMagick

# Back to the root of the project
cd ..

# Cleanup
rm colormaps_list/COLORMAPS_LIST_*.md

fpm run --example colormaps_list
fpm run --example demo

# Ensure output directory exists
mkdir -p colormaps_list/ppm/
# Cleanup
rm colormaps_list/ppm/*.ppm

mv *.ppm colormaps_list/ppm/

cd colormaps_list/
./ppm2png.sh

cd latex
./pdflatex.sh

cd ..
xdg-open ForColormap.pdf

