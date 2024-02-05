#!/bin/bash
# Requires convert program from ImageMagick

# Ensure output directory exists
mkdir -p png

for ppm_file in ppm/*.ppm; do
    if [ -e "$ppm_file" ]; then
        png_file="png/$(basename "$ppm_file" .ppm).png"
        echo "Converting $ppm_file to $png_file"
        convert "$ppm_file" "$png_file"
    fi
done

