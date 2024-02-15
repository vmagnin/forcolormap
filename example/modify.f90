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
! Last modification: vmagnin 2024-02-15
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
    call cmap%colorbar("bamO", encoding='binary')
    call cmap%shift(cmap%get_levels() / 2)
    call cmap%colorbar("bamO_shifted", encoding='binary')
    print *, "See the bamO.ppm and bamO_shifted.ppm colorbars"

    !> In the Scientific colour maps collection, all categorical colormaps
    !> begin with a dark colour, but a shift can be applied to begin with a
    !> brighter colour.
    call cmap%set("actonS", 0.0_wp, 2.0_wp)
    call cmap%colorbar("actonS", encoding='binary')
    call cmap%shift(+2)   !! Two levels towards left
    call cmap%colorbar("actonS_shifted", encoding='binary')
    print *, "See the actonS.ppm and actonS_shifted.ppm colorbars"

    !> Starting from a diverging colormap, we can obtain what could be called
    !> a diverging multi-sequential colormap.
    call cmap%set("bam", 0.0_wp, 2.0_wp)
    call cmap%colorbar("bam", encoding='binary')
    call cmap%shift(cmap%get_levels() / 2)
    call cmap%colorbar("bam_shifted", encoding='binary')
    print *, "See the bam.ppm and bam_shifted.ppm colorbars"

    contains

    subroutine test_colormap(self, filename, encoding)
        use forimage, only: format_pnm
        type(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        integer :: k, j     ! Pixbuffer coordinates
        integer, parameter :: pixwidth  = 600
        integer, parameter :: pixheight = 600
        integer, dimension(pixheight,pixwidth*3) :: rgb_image
        integer  :: red, green, blue
        real(wp) :: z
        type(format_pnm) :: ppm
        character(*), intent(in) :: encoding

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
end program modify
