! The MIT License (MIT)
!
! Copyright (c) 2023  AliG (gha3mi)
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
! Contributed by gha3mi: 2023-10-26
! Last modification: gha3mi 2024-01-06, vmagnin 2024-03-05
!-------------------------------------------------------------------------------

!> This example demonstrates how ForImage can be used to import/export PPM files.
program example1
    use forcolormap, only: Colormap, wp
    use forimage, only: format_pnm
    implicit none

    type(Colormap) :: custom_cmap
    type(format_pnm) :: ex1_colormap, ex1_colorbar
    real(wp), dimension(2), parameter :: xmin = [0.0_wp, 0.0_wp]
    real(wp), dimension(2), parameter :: xmax = [599.0_wp, 599.0_wp]

    ! Create ppm files
    call custom_cmap%load('test_map_to_load.txt', 0.0_wp, 2.0_wp)
    call custom_cmap%colorbar('a_loaded_colormap_ascii_test', encoding='ascii')
    call custom_cmap%colormap('a_loaded_colormap_ascii_colorbar', zfun, xmin, xmax, encoding='ascii', width=200, height=200)
    call custom_cmap%print()
    call custom_cmap%finalize()

    ! Import ascii ppm files
    call ex1_colormap%import_pnm('a_loaded_colormap_ascii_test','ppm', 'ascii')
    call ex1_colorbar%import_pnm('a_loaded_colormap_ascii_colorbar','ppm', 'ascii')

    ! Change colormap and colorbar colors
    ex1_colormap%pixels = int(ex1_colormap%pixels * 1.6)
    ex1_colorbar%pixels = int(ex1_colorbar%pixels * 1.6)

    ! Export binary ppm files
    call ex1_colormap%export_pnm('a_loaded_colormap_binary_test_m', 'binary')
    call ex1_colorbar%export_pnm('a_loaded_colormap_binary_colorbar_m', 'binary')

    ! Deallocate
    call ex1_colormap%finalize()
    call ex1_colorbar%finalize()

contains

   pure function zfun(x,y) result(z)
      real(wp), intent(in) :: x, y
      real(wp) :: z
      z = 1.0_wp + sin(x*y/10000.0_wp) * cos(y/100.0_wp)
   end function

end program example1
