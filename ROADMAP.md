# Roadmap

## Version 0.9

The main goal of v0.9 is to offer a usable library, sufficiently friendly, with a minimalist API documentation.

### Features

- [x] Methods
  - [x] Implement a `check()` method to fix colormap parameters if needed.
  - [x] Include a few minimalist methods to create or manipulate colormaps. More will be added in subsequent releases.
- [x] Introspection (Colormap_info)
    - [x] Include all available colormaps.
- [x] Add Interpolation functions.
  - [x] Lagrange
  - [x] Bezier
- [x] Add a `src/forcolormap_utils.f90` module where the subroutine `test_colormap()` will be moved.

### Examples

- [x] Create a `modify` example:
  - [x] For the `shift()` method.
- [x] A gtk-fortran example using ForColormap
  - [x] to put in https://github.com/vmagnin/gtk-fortran-extra/tree/main/reaction_diffusion

### Documentation

- [x] Develop a PDF guide with all colorbars to aid in choosing a colormap.
- [x] Develop a web version guide with all colorbars to aid in choosing a colormap.
- [x] Create a simple documentation generated with FORD.
- [x] Include real simulation images (`gallery/` directory).

### Continuous Integration (CI)

- [x] Integrate fpm tests.
- [x] Integrate FORD documentation.

### Building System

- [x] Test with a gtk-fortran fpm project: Linux, FreeBSD, macOS, MSYS2/Windows.
- [x] Implement a CMake building system, allowing installation of a `.so`
    - [x] Test on Linux.
    - [x] Test on FreeBSD.
    - [ ] Test on Windows (MSYS2): issue [#37](https://github.com/vmagnin/forcolormap/issues/37)
    - [x] Test on macOS.

### Others

- [x] Design a logo.

<!-- ### Bug Fixes -->



## Version 1.0

The main goal of v1.0 is to offer a library with a stable API, to improve the FORD documentation and to follow the Diátaxis documentation framework.

### Documentation

- [ ] Improve the FORD documentation, by adding FORD comments in the source code when needed.
- [ ] Initiate a Diátaxis documentation.
    - [ ] Specify which colormaps are colorblind-friendly, perceptually uniform, B&W print safe, etc. This information could be included in the 

### Deprecation

- [ ] Consider removing a few naive colormaps (e.g., "fire" is very similar to "black_body").

### Quality Assurance

- [ ] Add more automatic tests in `test/check.f90`.



## Version 1.1

The main goal of v1.1 is to offer more features and to evolve the Diátaxis documentation.


# Ideas

These concepts are open for implementation or consideration. They may or may not be implemented.

### Features
- [ ] Add facilities to pass 8 or 16-bit integers to C graphical libraries.
    - [ ] Test with gtk-fortran and other graphical libraries.
- [ ] Methods
  - [ ] Colormaps could have an option for logscale.
  - [ ] A `get_colorbar()` function could return an `array(:,:,1:3)` containing the RGB image of the colorbar. The arguments could be the width and height, the direction (horizontal/vertical), etc.
  - [ ] Add a `save()` or `export()` method
    - [ ] Transfer lut file subroutines from ForImage to ForColormap.
    - [ ] Add suport `.xml` file for use in ParaView or other softwares.
  - [ ] Improve the `shift()` method: 
    - [ ] `shift_levels()`, `shift_real()`
    - [ ] Possibly shifting automatically the colormap according to the [zmin, zmax] range. For example [-6, 3].
    - [ ] `shift_region()`: shifting certain segments of colormaps.
  - [ ] `split()` and `combine()` (could be useful to create a multi-sequential colormap) methods.
  - [ ] Convert to greyscale (note: convert to grayscale is implemented in ForImage).
  - [ ] Create and load colormaps based on other color formats (note: Color conversion is implemented in ForImage).
  - [ ] `brighten()` and darken (note: this is implemented in ForImage).
  - [ ] `mix()` two colormaps.
- [ ] Develop a GUI using gtk-fortran to load, show, choose, modify, save, etc., colormaps.
- [ ] Add Interpolation functions
  - [ ] B-Splines
  - [ ] NURBS
- [ ] Colormaps
  - [ ] Add more colormaps, only if similar ones are not already available and cannot be easily obtained with the available methods.

### Examples

- [ ] For the new features.

### Continuous Integration (CI)

- [ ] Set up continuous integration (CI) to automatically generate PDF files and colormap tables on a monthly basis or after each pull request.

### Documentation

- [ ] Include a few images in the `README.md` file.

### Publication

- [ ] Obtain a DOI from Zenodo, for example.
- [ ] And/or submit a JOSS *(Journal of Open Source Software)* paper similar to https://joss.theoj.org/papers/10.21105/joss.02004
