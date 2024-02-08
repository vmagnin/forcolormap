![Logo ForColormap](logo/logo_forcolormap-roma_8.svg)

# ForColormap

This Fortran library is independent of any graphical toolkit: its main functionality is to convert a real value to RGB values that you can use with any drawing toolkit. It offers various methods and options to manage colormaps. It includes:

* the 222 colormaps of the *Scientific colour maps* collection v8.0.1 by Fabio Crameri. See Fabio Crameri's poster ["Scientific Colour Maps"](https://www.fabiocrameri.ch/ws/media-library/a17d02961b3a4544961416de2d7900a4/posterscientificcolourmaps_crameri.pdf) for more information and my [No Bijection!](NO_BIJECTION.md) text about the mysteries and wonders of colors,
* the "magma", "inferno","plasma", "viridis" [matplotlib colormaps](https://bids.github.io/colormap/),
* the Dave Green's [cubehelix](https://people.phy.cam.ac.uk/dag9/CUBEHELIX/) colormap,
* a few basic colormaps: "black_body", "fire", "rainbow", "inverted_rainbow", "zebra".

## Basic usage

Assuming your graphical library has a `setpixelgb()`-like function and you know your `z` values will be for example in the [0, 2] range, you can write something like:

```fortran
use forcolormap, only: Colormap, wp
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

The library is using the precision `wp=>real64` defined in the module `iso_fortran_env`. And depending on the integers expected by your graphical library, you may need to convert the kinds of red, green, blue variables by writing for example `int(red, kind=int16)` if you need 16 bit integers.

This [guideline](https://s-ink.org/colour-map-guideline) can help you choose the right kind of colormap. And you can visually choose the available colormaps in the `colormaps_list/ForColormap.pdf` manual or on this page (under development): https://github.com/gha3mi/forcolormap/tree/dev


## Installation

### Requirements

You need, whatever your operating system:

* a modern Fortran compiler, for example GFortran or the Intel ifort/ifx compilers. See the [Fortran-lang.org compilers page](https://fortran-lang.org/compilers/) for other compilers.
* The Fortran Package Manager [fpm](https://fpm.fortran-lang.org/) or CMake (>=3.24) & pkg-config for building the project.
  * For writing PPM files, the library [ForImage](https://github.com/gha3mi/forimage) is used as a fpm or CMake dependency (automatically downloaded).

### Testing the project with fpm

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

### Using CMake

You can also build the project with CMake:
```bash
$ git clone git@github.com:vmagnin/forcolormap.git
$ cd forcolormap
$ mkdir build && cd build
$ cmake ..
$ make
$ sudo make install
```

#### Static linking
By default, ForColormap is built as a static library by CMake. You can compile your program with the `-static` option:
```bash
$ gfortran -static my_program.f90 $(pkg-config --cflags --libs forcolormap forimage)
```
Note that ForColormap is depending on ForImage, and for linking you must respect that order.

#### Dynamic linking
There is a CMake option to obtain a shared library:
```bash
$ cmake -D BUILD_SHARED_LIBS=true ..
```

You can compile your program like this:
```bash
$ gfortran my_program.f90 $(pkg-config --cflags --libs forcolormap)
```
If you encounter linking problems, you should verify the content of your `PKG_CONFIG_PATH` and `LD_LIBRARY_PATH` environment variables. For example, in Ubuntu the `.pc` files will be installed in `/usr/local/lib/pkgconfig/` and the libraries in `/usr/local/lib/`.

#### Building examples and tests

You can build the examples with:
```bash
$ cmake -D BUILD_FORCOLORMAP_EXAMPLES=true ..
$ make
$ cd example
```

The automatic tests can be run with:
```bash
$ cmake -D BUILD_TESTING=true ..
$ make
$ ctest
```

#### Uninstalling ForColormap

From the `build` directory:
```bash
$ sudo make uninstall_forcolormap
```

Note that its dependency ForImage will also be uninstalled! You will have to reinstall it if needed.

You can also choose and remove files listed in `build/install_manifest.txt` one by one.

See [CMake basics](https://github.com/vmagnin/gtk-fortran/wiki/CMake-basics) for more information.

## Learning

In the `example` directory, you will find these commented demos:
 
* `demo.f90` creates demo PPM files for each built-in colormap, plus a PPM file with the corresponding colorbars. It also demonstrates how to create your own colormap defined in an array and how to download a colormap from a `.txt` file.
* `demo_reverse.f90` demonstrates the usage of the `reverse=.true.` option to reverse the direction of a colormap.
* `colormaps_list.f90` generates the `colormaps_list/COLORMAPS_LIST_*.md` files.
* `example1.f90` demonstrates how ForImage can be used to import/export PPM files.
* `extract.f90` demonstrates how to create a specific colormap by extracting a specified number of colors of a colormap.
* `info.f90` demonstrates how to obtain information about a colormap using the `Colormaps_info` class.

They can be launched with the command `fpm run --example name_of_the_example` (without the `.f90` extension).

## Licenses

This project is under MIT license. The logo files are under [license CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).


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
* Cubehelix (Dave Green, public domain): https://people.phy.cam.ac.uk/dag9/CUBEHELIX/
* Scientific colour maps (Fabio Crameri, MIT license):
  * https://www.fabiocrameri.ch/colourmaps/
  * https://s-ink.org/colour-map-guideline
  * https://s-ink.org/scientific-colour-maps
  * Seminar talk by Fabio Crameri about the scientific use of colour in science communication for the University of Oslo GeoHyd seminar: https://www.youtube.com/watch?v=iDPzWARbFrs
* Matplotlib colormaps (CC0 license / public domain):
  * https://bids.github.io/colormap/
  * Python version: https://github.com/BIDS/colormap/blob/master/colormaps.py
  * Nathaniel Smith and Stéfan van der Walt, *A Better Default Colormap for Matplotlib,* SciPy 2015:  https://www.youtube.com/watch?v=xAoljeRJ3lU
* Black Body colormap (CC0 license / public domain) by Kenneth Moreland: ["Color Map Advice for Scientific Visualization"](https://www.kennethmoreland.com/color-advice/).
* [Colors for data scientists. Generate and refine palettes of optimally distinct colors.](https://medialab.github.io/iwanthue/)
