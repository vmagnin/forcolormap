! The MIT License (MIT)
!
! Copyright (c) 2023-2024 Vincent Magnin
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
! Last modification: gha3mi 2024-01-28, vmagnin 2026-01-08
!-------------------------------------------------------------------------------

!> This example will create colorbar files for each available colormap and
!> the corresponding test images. It also demonstrates how you can create your
!> own colormap defined in an array, or import it from a text file. 
program demo
    use forcolormap, only: Colormap, colormaps_list, wp
    use forcolormap_utils, only: test_colormap
    implicit none

    integer :: i
    type(Colormap) :: cmap, custom_cmap

    !> A discrete colormap with 8 levels, by @alozada, resembling the color
    !> changes in red cabbage (containing Anthocyanins) with pH:
    integer, dimension(0:7, 3) :: my_colormap = reshape( [ &
        198,    29,    32,   &
        189,    21,    56,   &
        171,    82,   150,   &
        102,    81,   156,   &
         38,    53,   108,   &
          5,    65,    40,   &
        221,   199,    44,   &
        237,   191,    44 ], &
        shape(my_colormap), order = [2, 1] )
    !> The name of your colormap must conform to the max length
    !> defined in colormap_parameters.f90
    ! Use the create() method instead of the set() method.
    call custom_cmap%create('red_cabbage', 0.0_wp, 2.0_wp, my_colormap)
    call custom_cmap%colorbar('red_cabbage_colorbar')
    call test_colormap(custom_cmap, 'red_cabbage_test')

    ! We create PPM files (binary encoded by default) for each built-in colormap.
    ! The built-in z=f(x,y) test function is in the [0, 2] range:
    do i = 1, size(colormaps_list)
        call cmap%set(trim(colormaps_list(i)), 0.0_wp, 2.0_wp)
        call cmap%colorbar(trim(colormaps_list(i))//'_colorbar')
        call test_colormap(cmap, trim(colormaps_list(i))//'_test')
        print '("Colormap ", A30, " has ", I0, " levels")', trim(cmap%get_name()), cmap%get_levels()
    end do

    ! Cubehelix can also accept other parameters (varargs array):
    call cmap%set("cubehelix", 0.0_wp, 2.0_wp, 1024, [0.5_wp, -1.0_wp, 1.0_wp, 1.0_wp])
    ! We change the name for the output test files:
    call cmap%colorbar('cubehelix_customized_colorbar')
    call test_colormap(cmap, 'cubehelix_customized_test')

    !> You can also download your colormap from a .txt file by
    !> using the load() method instead of the set() method.
    call custom_cmap%load("test_map_to_load.txt", 0.0_wp, 2.0_wp)
    call custom_cmap%colorbar('a_loaded_colorbar')
    call test_colormap(custom_cmap, 'a_loaded_colormap_test')
    call custom_cmap%print()

end program demo
