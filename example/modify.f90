! The MIT License (MIT)
!
! Copyright (c) 2024 Vincent Magnin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
! Contributed by vmagnin: 2024-02-14
! Last modification: vmagnin 2026-01-08
!-------------------------------------------------------------------------------

!> This example shows how you can modify a colormap with methods like shift(),
!> in concrete cases.
program modify
    use forcolormap, only: Colormap, wp

    implicit none
    type(Colormap) :: cmap

    !> In the Scientific colour maps collection, all cyclic colormaps have their
    !> bright part in the middle. But we can shift the dark part towards the
    !> center.
    call cmap%set("bamO", 0.0_wp, 2.0_wp)
    call cmap%colorbar("bamO")
    call cmap%shift(cmap%get_levels() / 2)
    call cmap%colorbar("bamO_shifted")
    print *, "See the bamO.ppm and bamO_shifted.ppm colorbars"

    !> And all categorical colormaps
    !> begin with a dark colour, but a shift can be applied to begin with a
    !> brighter colour.
    call cmap%set("actonS", 0.0_wp, 2.0_wp)
    call cmap%colorbar("actonS")
    call cmap%shift(+2)   !! Two levels towards left
    call cmap%colorbar("actonS_shifted")
    print *, "See the actonS.ppm and actonS_shifted.ppm colorbars"

    !> Finally, starting from a diverging colormap, we can obtain what could be called
    !> a diverging multi-sequential colormap.
    call cmap%set("bam", 0.0_wp, 2.0_wp)
    call cmap%colorbar("bam")
    call cmap%shift(cmap%get_levels() / 2)
    call cmap%colorbar("bam_shifted")
    print *, "See the bam.ppm and bam_shifted.ppm colorbars"

end program modify
