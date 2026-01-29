---
title: Learning
---

# API
The API is documented in the [FORD documentation](https://vmagnin.github.io/forcolormap/).

# Examples
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


