---
title: Basic usage
---

Assuming your graphical library has a classical `setpixelrgb()` procedure and you know your `z` values will be for example in the [0, 2] range, you can write something like:

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

![Example using the lajolla Scientific colormap](../gallery/lajolla_test.png)

![The lajolla Scientific colormap](../gallery/lajolla_colorbar.png)

The library is using the precision `wp=>real64` defined in the module `iso_fortran_env`. And depending on the integers expected by your graphical library, you may need to convert the kinds of red, green, blue variables.

