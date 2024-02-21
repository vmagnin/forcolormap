! The MIT License (MIT)
!
! Copyright (c) 2024 vmagnin, gha3mi
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
! Contributed by vmagnin & gha3mi: 2024-02-21
! Last modification: vmagnin 2024-02-21
!-------------------------------------------------------------------------------

!> This module contains miscellaneous procedures and functions.
module forcolormap_utils
   use forcolormap, only: Colormap, wp

   implicit none

   private

   public :: test_colormap

contains

    !> This procedure computes a default z=f(x,y) function and plot it
    !> in a .ppm file using the specified colormap.
    subroutine test_colormap(self, filename, encoding)
        use forimage, only: format_pnm
        type(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        integer :: k, j     ! Pixbuffer coordinates
        integer, parameter :: pixwidth  = 600
        integer, parameter :: pixheight = 600
        integer, dimension(:,:), allocatable :: rgb_image
        integer  :: red, green, blue
        real(wp) :: z
        type(format_pnm) :: ppm
        character(*), intent(in) :: encoding

        allocate(rgb_image(pixheight,pixwidth*3))

        do k = 0, pixwidth-1
            do j = 0, pixheight-1
                ! Computing a z=f(x,y) function:
                z = 1.0_wp + sin(k*j/10000.0_wp) * cos(j/100.0_wp)
                ! The corresponding RGB values in our colormap:
                call self%compute_RGB(z, red, green, blue)
                rgb_image(pixheight-j, 3*(k+1)-2) = red
                rgb_image(pixheight-j, 3*(k+1)-1) = green
                rgb_image(pixheight-j, 3*(k+1))   = blue
            end do
        end do

        call ppm%set_pnm(encoding    = encoding,&
                        file_format = 'ppm',&
                        width       = pixwidth,&
                        height      = pixheight,&
                        max_color   = 255,&
                        comment     = 'comment',&
                        pixels      = rgb_image)
        call ppm%export_pnm(filename)

    end subroutine test_colormap

end module forcolormap_utils
