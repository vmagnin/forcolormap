# Examples

They can be launched with the command `fpm run --example name_of_the_example` (without the `.f90` extension):

* `demo.f90` creates demo PPM files for each built-in colormap, plus a PPM file with the corresponding colorbars. It also demonstrates how to create your own colormap defined in an array, how to download a colormap from a `.txt` file.
* `demo_reverse.f90` demonstrates the usage of the `reverse=.true.` option to reverse the direction of a colormap.
* `example1.f90` demonstrates how ForImage can be used to import/export PPM files.
* `create.f90` demonstrates creating a custom colormap using methods like `create_lagrange()` and `create_bezier()`.
* `extract.f90` demonstrates how to create a specific colormap by extracting a specified number of colors of a colormap.
* `info.f90` demonstrates how to obtain information about a colormap using the `Colormaps_info` class.
* `modify.f90` demonstrates how you can modify a colormap with methods like shift(), in concrete cases.
