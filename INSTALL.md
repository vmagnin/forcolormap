# Installation

## Requirements

You need, whatever your operating system:

* a modern Fortran compiler, for example GFortran or the Intel ifort/ifx compilers. See the [Fortran-lang.org compilers page](https://fortran-lang.org/compilers/) for other compilers.
* The Fortran Package Manager [fpm](https://fpm.fortran-lang.org/) or CMake (>=3.24) & pkg-config for building the project.
* The only dependency of ForColormap is the library [ForImage](https://github.com/gha3mi/forimage), used especially for writing PPM files. It is automatically downloaded by fpm and CMake.

## Testing the project with fpm

If you have a GitHub account, just clone the repository, else download it as a zip archive. Then launch the demo example, which is creating [PPM files](https://en.wikipedia.org/wiki/Netpbm#File_formats) with colormaps and colorbars for all the available colormaps:

```bash
$ git clone git@github.com:vmagnin/forcolormap.git
$ cd forcolormap
$ fpm run --example demo
```

## Using ForColormap as a fpm dependency

To use ForColormap within your own `fpm` project, add the following lines to your `fpm.toml` manifest file:

```toml
[dependencies]
forcolormap = {git = "https://github.com/vmagnin/forcolormap.git"}
```

## Using CMake

You can also build the project with CMake:

```bash
$ git clone git@github.com:vmagnin/forcolormap.git
$ cd forcolormap
$ mkdir build && cd build
$ cmake ..
$ make
$ sudo make install
```

### Static linking
By default, ForColormap is built as a static library by CMake. You can compile your program with the `-static` option:

```bash
$ gfortran -static my_program.f90 $(pkg-config --cflags --libs forcolormap forimage)
```
Note that ForColormap is depending on ForImage, and for static linking you must respect that order.

### Dynamic linking
There is a CMake option to obtain a shared library:

```bash
$ cmake -D BUILD_SHARED_LIBS=true ..
```

You can compile your program like this:

```bash
$ gfortran my_program.f90 $(pkg-config --cflags --libs forcolormap)
```
If you encounter linking problems, you should verify the content of your `PKG_CONFIG_PATH` and `LD_LIBRARY_PATH` environment variables. For example, in Ubuntu or FreeBSD the `.pc` files will be installed in `/usr/local/lib/pkgconfig/` and the libraries in `/usr/local/lib/`.

```bash
$ export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/
```

### Building examples and tests

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

### Uninstalling ForColormap

From the `build` directory:

```bash
$ sudo make uninstall_forcolormap
```

Note that its dependency ForImage will also be uninstalled! You will have to reinstall it if needed.

You can also choose and remove files listed in `build/install_manifest.txt` one by one.

See [CMake basics](https://github.com/vmagnin/gtk-fortran/wiki/CMake-basics) for more information.