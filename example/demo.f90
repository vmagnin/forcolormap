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
! Last modification: gha3mi 2024-01-28, vmagnin 2024-02-22
!-------------------------------------------------------------------------------

program demo
    use forcolormap, only: Colormap, colormaps_list, wp
    use forcolormap_utils, only: test_colormap
    implicit none

    integer :: i
    type(Colormap) :: cmap, custom_cmap
    integer, allocatable :: colors(:,:)
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

    !> We create PPM files (binary encoded by default) for each built-in colormap.
    !> The built-in z=f(x,y) test function is in the [0, 2] range:
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

    ! You can create your own colormap defined in an array:
    call custom_cmap%create('discrete', 0.0_wp, 2.0_wp, my_colormap)
    call custom_cmap%colorbar('discrete_colorbar')
    call test_colormap(custom_cmap, 'discrete_test')

    ! Or you can download it from a .txt file:
    call custom_cmap%load("test_map_to_load.txt", 0.0_wp, 2.0_wp)
    call custom_cmap%colorbar('a_loaded_colorbar')
    call test_colormap(custom_cmap, 'a_loaded_colormap_test')
    call custom_cmap%print()

end program demo
