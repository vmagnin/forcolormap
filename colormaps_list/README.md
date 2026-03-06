This directory contains the files and scripts necessary to regenerate the `ForColormap.pdf` file containing the list of all colormaps and instructions for using and citing them.

# Regenerating the PDF

In the `example/demo.f90` file, comment temporarily all the colormaps that are outside the `do...endo` loop (those customed colormaps must not appear in the PDF).

You can also comment in the loop the line generating the `*_test.ppm` files, to accelerate the process.

Then from the `colormaps_list/` directory, launch the script:

```bash
$ ./generate_PDF.sh
```
