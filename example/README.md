# Examples

They can be launched with the command `fpm run --example name_of_the_example` (without the `.f90` extension):

* `demo.f90` creates demo PPM files for each built-in colormap, plus a PPM file with the corresponding colorbars. It also demonstrates how to create your own colormap defined in an array and how to download a colormap from a `.txt` file.
* `demo_reverse.f90` demonstrates the usage of the `reverse=.true.` option to reverse the direction of a colormap.
* `example1.f90` demonstrates how ForImage can be used to import/export PPM files.