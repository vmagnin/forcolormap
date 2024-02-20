! The MIT License (MIT)
!
! Copyright (c) 2023 Vincent Magnin
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
! Contributed by gha3mi, vmagnin: 2024-02-16
! Last modification: gha3mi 2024-02-16
!-------------------------------------------------------------------------------

!> This example demonstrates creating a custom colormap using methods like
!> create_lagrange() and create_bezier().
program create
    use forcolormap, only: Colormap, wp
    implicit none

    type(Colormap) :: custom_cmap
    integer :: colors(3,3)

    ! Define control colors for the colormap.
    colors(1,:) = [255, 0, 0] ! Red
    colors(2,:) = [0, 255, 0] ! Green
    colors(3,:) = [0, 0, 255] ! Blue

    ! Create a custom colormap using Lagrange interpolation.
    call custom_cmap%create_lagrange('custom_lagrange',  0.0_wp, 2.0_wp, colors, 1024)
    call custom_cmap%colorbar('custom_colorbar_lagrange')
    print *, "See the custom_colorbar_lagrange.ppm colorbar"

    ! Create a custom colormap using Bezier interpolation.
    call custom_cmap%create_bezier('custom_bezier',  0.0_wp, 2.0_wp, colors, 1024)
    call custom_cmap%colorbar('custom_colorbar_bezier')
    print *, "See the custom_colorbar_bezier.ppm colorbar"

end program create
