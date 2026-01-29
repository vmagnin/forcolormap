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

