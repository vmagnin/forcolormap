# ForColormap

This Fortran fpm library is independent of any graphical toolkit: its main functionality is to convert a real value to RGB values that you can use with any drawing toolkit. It offers various methods and options to manage colormaps. It includes:

* a few basic colormaps: "grey", "fire", "rainbow", "inverted_rainbow", "zebra",
* the Dave Green's [cubehelix](https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/) colormap,
* the "magma", "inferno","plasma", "viridis" [matplotlib colormaps](https://bids.github.io/colormap/),
* the 222 colormaps of the *Scientific colour maps* collection v8.0.1 by Fabio Crameri. See Fabio Crameri's poster ["Scientific Colour Maps"](https://www.fabiocrameri.ch/ws/media-library/a17d02961b3a4544961416de2d7900a4/posterscientificcolourmaps_crameri.pdf) for more information and my [No Bijection!](NO_BIJECTION.md) text about the mysteries and wonders of colors.

## Basic usage

Assuming your graphical library has a `setpixelgb()`-like function and you know your `z` values will be for example in the [0, 2] range, you can write something like:

```fortran
use forcolormap, only: Colormap, colormaps_list, wp
...
type(Colormap) :: cmap
integer  :: red, green, blue
real(wp) :: z, x, y
...
! Let's use the glasgow colormap:
call cmap%set("glasgow", 0.0_wp, 2.0_wp)
...
z = f(x,y)
call cmap%compute_RGB(z, red, green, blue)
call setpixelrgb(x, y, red, green, blue)
```

The library is using the precision `wp=>real64` defined in the module `iso_fortran_env`.

Note that **there is no default colormap** as we consider that the user must choose a colormap adapted to the properties of its data. This [guideline](https://s-ink.org/colour-map-guideline) can help you choosing the right kind of colormap.

## Installation

### Requirements

You need, whatever your operating system:

* a modern Fortran compiler, for example GFortran or the Intel ifort/ifx compilers. See the [Fortran-lang.org compilers page](https://fortran-lang.org/compilers/) for other compilers.
* The Fortran Package Manager [fpm](https://fpm.fortran-lang.org/).
  * For writing PPM files, the library [ForImage](https://github.com/gha3mi/forimage) is used as a fpm dependency.

### Testing the project

If you have a GitHub account, just clone the repository. Then launch the demo example, which is creating [PPM files](https://en.wikipedia.org/wiki/Netpbm#File_formats) with colormaps and colorbars for all the available colormaps:

```bash
$ git clone git@github.com:vmagnin/forcolormap.git
$ cd forcolormap
$ fpm run --example demo
```

### Using ForColormap as a fpm dependency

To use ForColormap within your own `fpm` project, add the following lines to your `fpm.toml` manifest file:

```toml
[dependencies]
forcolormap = {git = "https://github.com/vmagnin/forcolormap.git" }
```

## Learning

In the `example` directory, you will find these commented demos:
 
* `demo.f90` creates demo PPM files for each built-in colormap, plus a PPM file with the corresponding colorbars. It also demonstrates how to create your own colormap defined in an array and how to download a colormap from a `.txt` file.
* `demo_reverse.f90` demonstrates the usage of the `reverse=.true.` option to reverse the direction of a colormap.
* `example1.f90` demonstrates how ForImage can be used to import/export PPM files.
* `extract.f90` demonstrates how to create a specific colormap by extracting a specified number of colors of a colormap.
* `info.f90` demonstrates how to obtain information about a colormap using the `Colormaps_info` class.

They can be launched with the command `fpm run --example name_of_the_example` (without the `.f90` extension).


## TODO / ideas for further developments

* [ ] Create a logo: a coloured marble?
* [ ] Improve the documentation.
  * [ ] Include a few images in the README.md file.
  * [ ] Which colormaps are colorblind friendly, perceptually uniform, B&W print safe, etc.
  * [ ] Add in `src/colormaps_info.f90` and `COLORMAPS_LIST.md` the Matplotlib and miscellaneous colormaps.
* [ ] Add more colormaps.
* [ ] Colormaps could have an option for logscale.
* [ ] A `get_colorbar()` function could return an `array(:,:,1:3)` containing the RGB image of the colorbar. The arguments could be the width and height, the direction (horizontal/vertical), etc.
* [ ] A `save()` method could save a colormap as RGB values separated by spaces in a `.lut` text file.
* [ ] Adding functions to create colormaps, for example defined by their two extremes colors: `create_linear(color1, color2)`.


## License

This project is under MIT license.


## Citing colormaps

As any work, a colormap should be cited:

* For *Scientific colour maps,* please cite these two items:
  * Crameri, F. (2018a), Scientific colour maps. *Zenodo.* http://doi.org/10.5281/zenodo.1243862
  * Crameri, Fabio, Grace E. Shephard, and Philip J. Heron. “The Misuse of Colour in Science Communication.” *Nature Communications* 11, no. 1 (October 28, 2020): 5444. https://doi.org/10.1038/s41467-020-19160-7.
* For the matplotlib colormaps, you can cite this webpage https://bids.github.io/colormap/
* For the *cubehelix* colormap, please cite:
  * Green, D. A. “A Colour Scheme for the Display of Astronomical Intensity Images.” *arXiv,* August 30, 2011. http://arxiv.org/abs/1108.5083.


## References

### Articles and books

* Nuñez, Jamie R., Christopher R. Anderton, and Ryan S. Renslow. “Optimizing Colormaps with Consideration for Color Vision Deficiency to Enable Accurate Interpretation of Scientific Data.” Edited by Jesús Malo. *PLOS ONE* 13, no. 7, August 1, 2018, e0199239. https://doi.org/10.1371/journal.pone.0199239.
* Rogowitz, Bernice E, and Lloyd A Treinish. [“Why Should Engineers and Scientists Be Worried About Color?”](https://github.com/amadeusine/interesting-reads/blob/master/ibm-research__why-should-engineers-and-scientists-be-worried-about-color.pdf)
* Thyng, Kristen, Chad Greene, Robert Hetland, Heather Zimmerle, and Steven DiMarco. “True Colors of Oceanography: Guidelines for Effective and Accurate Colormap Selection.” *Oceanography* 29, no. 3, September 1, 2016, pp. 9–13. https://doi.org/10.5670/oceanog.2016.66.
* Valeur, Bernard. *La couleur dans tous ses éclats.* Bibliothèque scientifique. Paris: Belin-"Pour la science", 2011, ISBN 9782701158761.
* Valeur, Bernard. *Lumière et luminescence - Ces phénomènes lumineux qui nous entourent.* Bibliothèque scientifique. Paris: Belin-"Pour la science", 2005, ISBN 9782701136035.

### Web pages

#### About colormaps
* https://en.wikipedia.org/wiki/Color_gradient
* https://en.wikipedia.org/wiki/Heat_map
* Ken Hughes, ["Default colormaps: Are Parula and Viridis really an improvement over Jet?"](https://brushingupscience.com/2019/10/01/default-colormaps-are-parula-and-viridis-really-an-improvement-over-jet/), posted on October 1, 2019.
* "In Search of a Perfect Colormap", http://inversed.ru/Blog_2.htm

#### Specific colormaps
* Cubehelix (Dave Green, public domain): https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/
* Scientific colour maps (Fabio Crameri, MIT license):
  * https://www.fabiocrameri.ch/colourmaps/
  * https://s-ink.org/colour-map-guideline
  * https://s-ink.org/scientific-colour-maps
  * Seminar talk by Fabio Crameri about the scientific use of colour in science communication for the University of Oslo GeoHyd seminar: https://www.youtube.com/watch?v=iDPzWARbFrs
* Matplotlib colormaps (CC0 license / public domain):
  * https://bids.github.io/colormap/
  * Python version: https://github.com/BIDS/colormap/blob/master/colormaps.py
  * Nathaniel Smith and Stéfan van der Walt, *A Better Default Colormap for Matplotlib,* SciPy 2015:  https://www.youtube.com/watch?v=xAoljeRJ3lU
* [Colors for data scientists. Generate and refine palettes of optimally distinct colors.](https://medialab.github.io/iwanthue/)
