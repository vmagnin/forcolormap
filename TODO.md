# TODO & ideas for further developments

## Documentation
* [ ] Include a few images in the README.md file?
* [ ] Which colormaps are colorblind friendly, perceptually uniform, B&W print safe, etc.
* [ ] Add in `src/colormaps_info.f90` and `COLORMAPS_LIST.md` the Matplotlib and miscellaneous colormaps.

## Colormaps
* [ ] Add more colormaps.
* [ ] Colormaps could have an option for logscale.
* [ ] A `get_colorbar()` function could return an `array(:,:,1:3)` containing the RGB image of the colorbar. The arguments could be the width and height, the direction (horizontal/vertical), etc.
* [ ] A `save()` method could save a colormap as RGB values separated by spaces in a `.lut` text file.
* [ ] Adding functions to create colormaps, for example defined by their two extremes colors: `create_linear(color1, color2)`.

## Development
* [ ] Add more automatic tests in `test/check.f90`.