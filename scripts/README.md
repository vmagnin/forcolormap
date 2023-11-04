# Scripts

These programs are used to generate Fortran code for specific colormaps, like the *Scientific Colour Maps* collection:

* `gpl_to_lut.f90` converts `.gpl` files in the *Scientific Colour Map* collection and its subdirectories to `.lut` files, then copies them to the current directory, where they can be used by the `generate_scmap.f90` script. It also prints the list.
* `generate_scmap.f90` generates this two files:
  * `scientific_colour_maps.f90` from the `.lut` files of the *Scientific Colour Maps* collection. It must then be copied to `../src`.
  * `copy-paste_code.f90`: its code must be copied/pasted in the `src/colormap_class.f90` file, in the `select case(self%name)` statement.
