# Roadmap

## Version 1.0

The main goal of v1.0 is to offer a reliable library with a stable API and to improve the FORD documentation.

### Documentation

- [x] Improve the FORD documentation, by adding FORD comments in the source code when needed.
- [x] Include a few images in the `README.md` file.
- [ ] Add properties of the colormaps in the `README.md` file: specify which colormaps are colorblind-friendly, perceptually uniform, B&W print safe, etc.

### Quality Assurance

- [x] Add more automatic tests in `test/check.f90`.

### Publication

- [ ] Submit a JOSS *(Journal of Open Source Software)* paper similar to https://joss.theoj.org/papers/10.21105/joss.02004


## Version 1.1

The main goal of v1.1 is to offer more features.


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
- [ ] Add Interpolation functions
  - [ ] B-Splines
  - [ ] NURBS
- [ ] Colormaps
  - [ ] Add more colormaps, only if similar ones are not already available and cannot be easily obtained with the available methods.
- [ ] Develop a GUI using gtk-fortran to load, show, choose, modify, save, etc., colormaps.

### Examples

- [ ] For the new features.

### Continuous Integration (CI)

- [ ] Set up continuous integration (CI) to automatically generate PDF files and colormap tables on a monthly basis or after each pull request.

### Documentation

- [ ] Follow the [Diátaxis](https://diataxis.fr/) documentation framework, if FORD allows.
