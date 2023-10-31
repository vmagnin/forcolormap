# ForColormap

This small Fortran colormap fpm library is independent of any graphical toolkit: it just converts a real value to RGB values, that you can use with any toolkit offering bitmap drawing.

It includes:
* a few basic colormaps: "grey", "inverted_grey", "fire", "rainbow", "inverted_rainbow", "zebra",
* the Dave Green's [cubehelix](https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/) colormap,
* the "magma", "inferno","plasma", "viridis" [matplotlib colormaps](https://bids.github.io/colormap/),
* 62 colormaps of the *Scientific colour maps* collection v8.0.1 by Fabio Crameri (the discrete palettes were not imported). See Fabio Crameri's poster ["Scientific Colour Maps"](https://www.fabiocrameri.ch/ws/media-library/a17d02961b3a4544961416de2d7900a4/posterscientificcolourmaps_crameri.pdf) for more information and my [No Bijection!](NO_BIJECTION.md) text about the mysteries and wonders of colors.


## Basic usage

Assuming your graphical library has a `setpixelgb()`-like function and z is in the [0, 2] range, you can write something like:

```fortran
use forcolormap, only: Colormap, colormaps_list, wp
...
type(Colormap) :: cmap
integer  :: red, green, blue
real(wp) :: z, x, y
...
call cmap%set("glasgow", 0.0_wp, 2.0_wp)
...
z = f(x,y)
call cmap%compute_RGB(z, red, green, blue)
call setpixelrgb(x, y, red, green, blue)
```

Look into the `example/demo.f90` file for more usage examples: you can create your own colormap, download a colormap from a text file, etc.

Note that **there is no default colormap** as we consider that each user must choose a colormap adapted to the properties of its data.


## Installation

### Requirements

You need:

* a modern Fortran compiler, for example GFortran or the Intel ifort/ifx compilers. See the [Fortran-lang.org compilers page](https://fortran-lang.org/compilers/) for other compilers.
* The Fortran Package Manager [fpm](https://fpm.fortran-lang.org/).
  * For writing PPM files, the library [ForImage](https://github.com/gha3mi/forimage) is used as a fpm dependency.
* Any operating system.

### Testing the project

If you have a GitHub account, just clone the repository and launch the demo example:

```bash
$ git clone git@github.com:vmagnin/forcolormap.git
$ cd forcolormap
$ fpm run --example demo
```

The demo is creating [PPM files](https://en.wikipedia.org/wiki/Netpbm#File_formats) with colormaps and colorbars for all the available colormaps.

### Using ForColormap as a fpm dependency

To use ForColormap within your own `fpm` project, add the following lines to your `fpm.toml` manifest file:

```toml
[dependencies]
forcolormap = {git = "https://github.com/vmagnin/forcolormap.git" }
```


## TODO / ideas for further developments

* [ ] Create a logo: inspired by Newton/Dark Side of the Moon? Or a rainbow? And using Fortran purple. Or a colored marble?
* [ ] Improve the documentation.
  * [ ] Include a few images in the README.md file.
  * [ ] A table with the characteristics of the colormaps: which are colorblind friendly, perceptually uniform, B&W print safe, etc.
* [ ] In the *Scientific colour maps,* the discrete colormaps were not imported for the time being because there is no `.lut` file.
* [ ] The `set()` method could have an optional `reverse` option to reverse the color order in a palette.
* [ ] Colormaps could have an option for logscale.
* [ ] A `get_colorbar()` function could return an `array(:,:,1:3)` containing the RGB image of the colorbar. The arguments could be the width and height, the direction (horizontal/vertical), etc.
* [ ] A `save()` method could save a colormap as RGB values separated by spaces in a `.lut` text file.
* [ ] Adding functions to create colormaps, for example defined by their two extremes colors: `create_linear(color1, color2)`.


## License

This project is under MIT license.


## Citing colormaps

* For *Scientific colour maps,* please cite these two items:
  * Crameri, F. (2018a), Scientific colour maps. Zenodo. http://doi.org/10.5281/zenodo.1243862
  * Crameri, Fabio, Grace E. Shephard, and Philip J. Heron. “The Misuse of Colour in Science Communication.” Nature Communications 11, no. 1 (October 28, 2020): 5444. https://doi.org/10.1038/s41467-020-19160-7.
* For the matplotlib colormaps, you can cite this webpage https://bids.github.io/colormap/
* For the *cubehelix* colormap, please cite:
  * Green, D. A. “A Colour Scheme for the Display of Astronomical Intensity Images.” arXiv, August 30, 2011. http://arxiv.org/abs/1108.5083.

## References

### Articles and books

* Nuñez, Jamie R., Christopher R. Anderton, and Ryan S. Renslow. “Optimizing Colormaps with Consideration for Color Vision Deficiency to Enable Accurate Interpretation of Scientific Data.” Edited by Jesús Malo. PLOS ONE 13, no. 7 (August 1, 2018): e0199239. https://doi.org/10.1371/journal.pone.0199239.
* Rogowitz, Bernice E, and Lloyd A Treinish. [“Why Should Engineers and Scientists Be Worried About Color?”](https://github.com/amadeusine/interesting-reads/blob/master/ibm-research__why-should-engineers-and-scientists-be-worried-about-color.pdf)
* Thyng, Kristen, Chad Greene, Robert Hetland, Heather Zimmerle, and Steven DiMarco. “True Colors of Oceanography: Guidelines for Effective and Accurate Colormap Selection.” Oceanography 29, no. 3 (September 1, 2016): 9–13. https://doi.org/10.5670/oceanog.2016.66.
* Valeur, Bernard. *La couleur dans tous ses éclats.* Bibliothèque scientifique. Paris: Belin-"Pour la science", 2011, ISBN 9782701158761.
* Valeur, Bernard. *Lumière et luminescence - Ces phénomènes lumineux qui nous entourent.* Bibliothèque scientifique. Paris: Belin-"Pour la science", 2005, ISBN 9782701136035.

### Web pages

#### About colormaps
* https://en.wikipedia.org/wiki/Color_gradient
* https://en.wikipedia.org/wiki/Heat_map
* Ken Hughes, ["Default colormaps: Are Parula and Viridis really an improvement over Jet?"](https://brushingupscience.com/2019/10/01/default-colormaps-are-parula-and-viridis-really-an-improvement-over-jet/), posted on October 1, 2019.
* "In Search of a Perfect Colormap", http://inversed.ru/Blog_2.htm

#### Specific colormaps
* Cubehelix (Dave Green, public domain):
  * https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/
* Scientific colour maps (Fabio Crameri, MIT license):
  * https://www.fabiocrameri.ch/colourmaps/
  * https://s-ink.org/colour-map-guideline
  * https://s-ink.org/scientific-colour-maps
* Matplotlib colormaps (CC0 license / public domain):
  * https://bids.github.io/colormap/
  * Python version: https://github.com/BIDS/colormap/blob/master/colormaps.py
  * Nathaniel Smith and Stéfan van der Walt, *A Better Default Colormap for Matplotlib,* SciPy 2015:  https://www.youtube.com/watch?v=xAoljeRJ3lU
* [Colors for data scientists. Generate and refine palettes of optimally distinct colors.](https://medialab.github.io/iwanthue/)
