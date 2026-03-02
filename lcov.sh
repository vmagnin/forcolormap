#!/usr/bin/env bash
#
# This script:
#   - Cleans the build directory
#   - Builds and runs tests with fpm using gfortran and debug profile, enabling coverage flags
#   - Captures line, function, and branch coverage via lcov
#   - Excludes dependencies and test sources from the report
#   - Generates an HTML coverage report using genhtml
#
# Output:
#   coverage/lcov/index.html
#
# Requirements:
#   - fpm
#   - gfortran
#   - lcov (with genhtml)
#
# Usage:
#   From the project root:
#     bash lcov.sh
#
set -euo pipefail

OUTDIR="coverage/lcov"
INFO="$OUTDIR/lcov.info"

fpm clean --all
fpm test --compiler gfortran --profile debug --flag "--coverage" --verbose

rm -rf "$OUTDIR"
mkdir -p "$OUTDIR"

lcov \
  --capture \
  --rc external=0 \
  --rc function_coverage=1 \
  --rc branch_coverage=1 \
  --rc geninfo_unexecuted_blocks=1 \
  --rc geninfo_adjust_testname=1 \
  --no-checksum \
  --directory build \
  --output-file "$INFO" \
  --exclude '*/dependencies/*' \
  --exclude '*/test/*' \
  --quiet

lcov --summary "$INFO" --quiet
lcov --list "$INFO" --quiet

genhtml \
  "$INFO" \
  --title "Coverage report" \
  --header-title "ForColormap coverage" \
  --rc genhtml_dark_mode=0 \
  --rc legend=1 \
  --rc genhtml_hierarchical=1 \
  --rc genhtml_sort=1 \
  --rc function_coverage=1 \
  --rc branch_coverage=1 \
  --rc genhtml_highlight=1 \
  --frames \
  --output-directory "$OUTDIR" \
  --quiet

echo "HTML report: $OUTDIR/index.html"
