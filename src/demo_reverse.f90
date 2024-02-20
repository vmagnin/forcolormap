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
! Contributed by vmagnin: 2023-09-26
! Last modification: gha3mi 2023-11-01, vmagnin 2024-02-15
!-------------------------------------------------------------------------------

program demo_reverse
    use forcolormap, only: Colormap, colormaps_list, wp
    implicit none

    integer :: i
    type(Colormap) :: cmap, custom_cmap
    ! A discrete colormap with 8 levels, from black to white:
    integer, dimension(0:7, 3) :: my_colormap = reshape( [ &
          0,     0,     0,   &
        255,     0,     0,   &
          0,   255,     0,   &
          0,     0,   255,   &
        255,   255,     0,   &
          0,   255,   255,   &
        255,     0,   255,   &
        255,   255,   255 ], &
        shape(my_colormap), order = [2, 1] )

    ! Let's create PPM files for each built-in colormap.
    ! The built-in z=f(x,y) test function is in the [0, 2] range:
    do i = 1, size(colormaps_list)
        call cmap%set(trim(colormaps_list(i)), 0.0_wp, 2.0_wp, reverse=.true.)
        call cmap%colorbar(trim(colormaps_list(i))//'_reverse_colorbar')
        call test_colormap(cmap, trim(colormaps_list(i))//'_reverse_test', encoding='binary')
        print '("Colormap ", A30, " has ", I0, " levels")', trim(cmap%get_name()), cmap%get_levels()
    end do

    ! Cubehelix can also accept other parameters (varargs array):
    call cmap%set("cubehelix", 0.0_wp, 2.0_wp, 1024, [0.5_wp, -1.0_wp, 1.0_wp, 1.0_wp], reverse=.true.)
    ! We change the name for the output test files:
    call cmap%colorbar('cubehelix_customized_reverse_colorbar')
    call test_colormap(cmap, 'cubehelix_customized_reverse_test', encoding='binary')

    ! You can create your own colormap defined in an array:
    call custom_cmap%create('discrete', 0.0_wp, 2.0_wp, my_colormap, reverse=.true.)
    call custom_cmap%colorbar('discrete_reverse_colorbar')
    call test_colormap(custom_cmap, 'discrete_reverse_test', encoding='binary')

    ! Or you can download it from a .txt file:
    call custom_cmap%load("test_map_to_load.txt", 0.0_wp, 2.0_wp, reverse=.true.)
    call custom_cmap%colorbar('a_loaded_reverse_colorbar')
    call test_colormap(custom_cmap, 'a_loaded_reverse_colormap_test', encoding='binary')
    call custom_cmap%print()

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
end program demo_reverse
