This directory contains the files and scripts necessary to regenerate the `ForColormap.pdf` file containing the list of all colormaps and instructions for using and citing them.

# Regenerating the PDF

In the `example/demo.f90` file, comment temporarily all the colormaps that are outside the `do...endo` loop (those customed colormaps must not appear in the PDF). Comment also in the loop the line generating the `*_test.ppm` files. 

Then from the root directory of the project type these commands:
```bash
$ fpm run --example demo
$ mkdir colormaps_list/ppm/
$ mv *.ppm colormaps_list/ppm/
$ cd colormaps_list/
$ ./ppm2png.sh
$ cd latex
$ ./pdflatex.sh
```
