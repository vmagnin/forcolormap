#! /bin/sh
# A script to copy here the .lut files from the Scientific Colour Map directory
# Arguments: $1 is the path to the Scientific Colour Map directory
# Contributed by Vincent MAGNIN, 2023-10-14
# Last updated: 2023-10-14

# For a safer script:
set -eu

find ${1} -name "*.lut" -exec cp '{}' . \;

echo "$(ls *.lut | wc -l) .lut files copied"

# -m : fill width with a comma separated list of entries
# -Q : enclose entry names in double quotes
ls -mQ *.lut
