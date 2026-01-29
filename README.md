![](logo/logo_forcolormap-roma_8.svg)

The ForColormap Fortran library is independent of any graphical toolkit: its main functionality is to convert a real value to RGB values that you can use with any drawing toolkit. It includes:

* the 222 colormaps of the ["Scientific Colour Maps"](https://www.fabiocrameri.ch/ws/media-library/a17d02961b3a4544961416de2d7900a4/posterscientificcolourmaps_crameri.pdf) collection v8.0.1 by Fabio Crameri. They are perceptually uniform, perceptually ordered, colour-vision-deficiency friendly, readable as black-and-white print and citable. They are classified into different palette types (continuous; discrete; categorical) and gradient types (sequential; diverging; multi-sequential; cyclic).
* The "magma", "inferno","plasma", "viridis" [matplotlib colormaps](https://bids.github.io/colormap/) are also perceptually uniform, perceptually ordered, colour-vision-deficiency friendly, readable as black-and-white print.
* The Dave Green's [cubehelix](https://people.phy.cam.ac.uk/dag9/CUBEHELIX/) colormap  is designed to be monotonically increasing in terms of its perceived brightness and readable as black-and-white print. It is citable.
* The "black_body" colormap is perceptually uniform, perceptually ordered and readable as black-and-white print.
* The "zebra" colormap is black and white.
* A few basic colormaps with no specific properties: "fire", "rainbow", "inv_rainbow".

ForColormap also offers various methods and options to manage colormaps. 

## Basic usage

Assuming your graphical library has a `setpixelgb()`-like function and you know your `z` values will be for example in the [0, 2] range, you can write something like:

```fortran
use forcolormap, only: Colormap, wp
...
type(Colormap) :: cmap
integer  :: red, green, blue
real(wp) :: z, x, y
...
! Let's use the lajolla Scientific colormap:
call cmap%set("lajolla", 0.0_wp, 2.0_wp)
...
z = f(x,y)
call cmap%compute_RGB(z, red, green, blue)
call setpixelrgb(x, y, red, green, blue)
```

![Example using the lajolla Scientific colormap](gallery/lajolla_test.png)

![The lajolla Scientific colormap](gallery/lajolla_colorbar.png)

The library is using the precision `wp=>real64` defined in the module `iso_fortran_env`. And depending on the integers expected by your graphical library, you may need to convert the kinds of red, green, blue variables.

This [guideline](https://s-ink.org/colour-map-guideline) can help you choose the right kind of colormap. And you can visually choose the available colormaps in the [colormaps_list/ForColormap.pdf](https://github.com/vmagnin/forcolormap/blob/main/colormaps_list/ForColormap.pdf) manual or on this page (under development): [https://github.com/gha3mi/forcolormap/tree/dev](https://github.com/gha3mi/forcolormap/tree/dev)


## Learning

The API is documented in the [FORD documentation](https://vmagnin.github.io/forcolormap/).

And in the `example` directory, you will find these commented demos:
 
* `demo.f90` creates demo PPM files for each built-in colormap, plus a PPM file with the corresponding colorbars. It also demonstrates how to create your own colormap defined in an array and how to download a colormap from a `.txt` file.
* `demo_reverse.f90` demonstrates the usage of the `reverse=.true.` option to reverse the direction of a colormap.
* `colormaps_list.f90` generates the `colormaps_list/COLORMAPS_LIST_*.md` files.
* `example1.f90` demonstrates how ForImage can be used to import/export PPM files.
* `create.f90` demonstrates creating a custom colormap using methods like `create_lagrange()` and `create_bezier()`.
* `extract.f90` demonstrates how to create a specific colormap by extracting a specified number of colors of a colormap.
* `info.f90` demonstrates how to obtain information about a colormap using the `Colormaps_info` class.
* `modify.f90` demonstrates how you can modify a colormap with methods like `shift()`, in concrete cases.

They can be launched with the command `fpm run --example name_of_the_example` (without the `.f90` extension).

In the gtk-fortran-extra repository, you will also find a [physical model](https://github.com/vmagnin/gtk-fortran-extra/tree/main/reaction_diffusion) demonstrating the use of ForColormap. It creates a movie with Turing patterns, displayed with various colormaps:

[https://www.youtube.com/watch?v=cVHLCVVvZ4U](https://www.youtube.com/watch?v=cVHLCVVvZ4U)

## Contributing

You can report problems and ask support in the GitHub Issues tab and you can contribute to the code by making Pull Requests. We are also present on the [Fortran Discourse](https://fortran-lang.discourse.group/).


## Citing colormaps

As any work, a colormap should be cited:

* For *Scientific colour maps,* please cite these two items:
    * Crameri, F. (2018a), Scientific colour maps. *Zenodo.* [http://doi.org/10.5281/zenodo.1243862](http://doi.org/10.5281/zenodo.1243862)
    * Crameri, Fabio, Grace E. Shephard, and Philip J. Heron. “The Misuse of Colour in Science Communication.” *Nature Communications* 11, no. 1 (October 28, 2020): 5444. [https://doi.org/10.1038/s41467-020-19160-7](https://doi.org/10.1038/s41467-020-19160-7).
* For the matplotlib colormaps, you can cite this webpage [https://bids.github.io/colormap/](https://bids.github.io/colormap/)
* For the *cubehelix* colormap, please cite:
    * Green, D. A. “A Colour Scheme for the Display of Astronomical Intensity Images.” *arXiv,* August 30, 2011. [http://arxiv.org/abs/1108.5083](http://arxiv.org/abs/1108.5083).


## References

### Articles and books

* Nuñez, Jamie R., Christopher R. Anderton, and Ryan S. Renslow. “Optimizing Colormaps with Consideration for Color Vision Deficiency to Enable Accurate Interpretation of Scientific Data.” Edited by Jesús Malo. *PLOS ONE* 13, no. 7, August 1, 2018, e0199239. [https://doi.org/10.1371/journal.pone.0199239](https://doi.org/10.1371/journal.pone.0199239).
* Rogowitz, Bernice E, and Lloyd A Treinish. [“Why Should Engineers and Scientists Be Worried About Color?”](https://github.com/amadeusine/interesting-reads/blob/master/ibm-research__why-should-engineers-and-scientists-be-worried-about-color.pdf)
* Thyng, Kristen, Chad Greene, Robert Hetland, Heather Zimmerle, and Steven DiMarco. “True Colors of Oceanography: Guidelines for Effective and Accurate Colormap Selection.” *Oceanography* 29, no. 3, September 1, 2016, pp. 9–13. [https://doi.org/10.5670/oceanog.2016.66](https://doi.org/10.5670/oceanog.2016.66).
* Valeur, Bernard. *La couleur dans tous ses éclats.* Bibliothèque scientifique. Paris: Belin-"Pour la science", 2011, ISBN 9782701158761.
* Valeur, Bernard. *Lumière et luminescence - Ces phénomènes lumineux qui nous entourent.* Bibliothèque scientifique. Paris: Belin-"Pour la science", 2005, ISBN 9782701136035.

### Web pages

#### About colormaps
* [No Bijection!](NO_BIJECTION.md): a passionate text about the mysteries and wonders of colors.
* [https://en.wikipedia.org/wiki/Color_gradient](https://en.wikipedia.org/wiki/Color_gradient)
* [https://en.wikipedia.org/wiki/Heat_map](https://en.wikipedia.org/wiki/Heat_map)
* Ken Hughes, ["Default colormaps: Are Parula and Viridis really an improvement over Jet?"](https://brushingupscience.com/2019/10/01/default-colormaps-are-parula-and-viridis-really-an-improvement-over-jet/), posted on October 1, 2019.
* [In Search of a Perfect Colormap](http://inversed.ru/Blog_2.htm)
* The [Data Visualisation Guide section about colours](https://data.europa.eu/apps/data-visualisation-guide/tag/colour)

#### Specific colormaps
* Cubehelix (Dave Green, public domain): [https://people.phy.cam.ac.uk/dag9/CUBEHELIX/](https://people.phy.cam.ac.uk/dag9/CUBEHELIX/)
* Scientific colour maps (Fabio Crameri, MIT license):
    * [https://www.fabiocrameri.ch/colourmaps/](https://www.fabiocrameri.ch/colourmaps/)
    * [https://s-ink.org/colour-map-guideline](https://s-ink.org/colour-map-guideline)
    * [https://s-ink.org/scientific-colour-maps](https://s-ink.org/scientific-colour-maps)
    * Seminar talk by Fabio Crameri about the scientific use of colour in science communication for the University of Oslo GeoHyd seminar: [https://www.youtube.com/watch?v=iDPzWARbFrs](https://www.youtube.com/watch?v=iDPzWARbFrs)
* Matplotlib colormaps (CC0 license / public domain):
    * Stéfan van der Walt and Nathaniel Smith: [https://bids.github.io/colormap/](https://bids.github.io/colormap/)
    * Python version: [https://github.com/BIDS/colormap/blob/master/colormaps.py](https://github.com/BIDS/colormap/blob/master/colormaps.py)
    * Nathaniel Smith and Stéfan van der Walt, *A Better Default Colormap for Matplotlib,* SciPy 2015:  [https://www.youtube.com/watch?v=xAoljeRJ3lU](https://www.youtube.com/watch?v=xAoljeRJ3lU)
* Black Body colormap (CC0 license / public domain) by Kenneth Moreland: ["Color Map Advice for Scientific Visualization"](https://www.kennethmoreland.com/color-advice/).
* [Colors for data scientists. Generate and refine palettes of optimally distinct colors.](https://medialab.github.io/iwanthue/)
