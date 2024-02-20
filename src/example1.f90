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
! Last modification: gha3mi 2024-01-06
!-------------------------------------------------------------------------------

! This example demonstrates how ForImage can be used to import/export PPM files.
program example1
    use forcolormap
    use forimage
    implicit none

    type(Colormap) :: custom_cmap
    type(format_pnm) :: ex1_colormap, ex1_colorbar

    ! Create ppm files
    call custom_cmap%load('test_map_to_load.txt', 0.0_wp, 2.0_wp)
    call custom_cmap%colorbar('a_loaded_colormap_ascii_test', encoding='ascii')
    call test_colormap(custom_cmap, 'a_loaded_colormap_ascii_colorbar', encoding='ascii')
    call custom_cmap%print()

    ! Import ascii ppm files
    call ex1_colormap%import_pnm('a_loaded_colormap_ascii_test','ppm', 'ascii')
    call ex1_colorbar%import_pnm('a_loaded_colormap_ascii_colorbar','ppm', 'ascii')

    ! Change colormap and colorbar colors
    ex1_colormap%pixels = ex1_colormap%pixels * (1.6)
    ex1_colorbar%pixels = ex1_colorbar%pixels * (1.6)

    ! Export binary ppm files
    call ex1_colormap%export_pnm('a_loaded_colormap_binary_test_m', 'binary')
    call ex1_colorbar%export_pnm('a_loaded_colormap_binary_colorbar_m', 'binary')

    ! Deallocate
    call ex1_colormap%finalize()
    call ex1_colorbar%finalize()

    contains

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
end program example1
