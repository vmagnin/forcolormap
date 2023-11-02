# Changelog
All notable changes to the gtk-fortran project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [forcolormap dev]

### Added
* A `README.md` file in each subdirectory describing the files inside.
* The discrete colormaps of the *Scientific Colour Maps* collection, thanks to the `scripts/gpl_to_lut.f90` program.
* A `reverse` boolean option was added to the methods `set`, `create`, and `load` to reverse a colormap.
  * An `example/demo_reverse.f90` example.
* The "magma", "inferno","plasma", "viridis" matplotlib colormaps in a `matplotlib_colormaps` module.
* A `colormap_parameters` module.
* A `miscellaneous_colormaps` module.
* A `colorbar()` type-bound procedure to write a PPM file with the colorbar.

### Changed
* Code refactoring.
* Moved 'test' subroutine to demo and example1.
* Renamed 'get_current()' to 'get_name()'.
* For writing PPM files, the project [ForImage](https://github.com/gha3mi/forimage) by @gha3mi is now used as a fpm dependency. The related example must now be launched with `fpm run --example example1` and the ForColormap demo by `fpm run --example demo`.
* `src/colormap_class.f90`: `private` statement is now the default.

### Removed
* `scripts/cp_lut_files.sh` since its functionality is now included in the `scripts/gpl_to_lut.f90` program.
* The "inverted_grey" colormap can now be obtained from "grey" with the `reverse=.true.` option.

## [forcolormap 0.8] 2023-10-23

First code commit.
