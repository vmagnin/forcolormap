#!/bin/bash
# Requires ImageMagick

# Ensure output directory exists
mkdir -p png
# Cleanup
rm png/*.png

# Converting each ppm file to png
for ppm_file in ppm/*.ppm; do
    if [ -e "$ppm_file" ]; then
        png_file="png/$(basename "$ppm_file" .ppm).png"
        echo "Converting $ppm_file to $png_file"
        magick "$ppm_file" "$png_file"
    fi
done

