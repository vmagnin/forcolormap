! The MIT License (MIT)
!
! Copyright (c) 2023 gha3mi
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
! Contributed by gha3mi: 2023-11-05
! Last modification: gha3mi 2023-11-05
!-------------------------------------------------------------------------------

program write_info
    use forcolormap_info, only: Colormaps_info
    implicit none
 
    type(Colormaps_info) :: info
 
    ! Set all colormap information
    call info%set_all()
 
    !! Print all colormap information using default verbosity (level 1)
    ! verbose: 1 (default) prints a table , 2 prints a box, 3 prints the name
    call info%write()
    
 

    !! Filter and write colormap information
    !! Available filters: name, family, gradient, palette, author, license, levels

    ! Filter by gradient using default verbosity (level 1)
    call info%write(gradient='Sequential', palette='Continuous')
 
    ! Filter by gradient and palette using default verbosity (level 1)
    call info%write(gradient='Sequential', palette='Continuous')
 
    ! Filter by family and palette using verbosity level 2
    call info%write(family='vik', gradient='Diverging', palette='Continuous', verbose=2)
 


    !! Write colormap information to a file
    call info%write(gradient='Sequential', file_name='COLORMAPS_LIST.md')
    call info%write(gradient='Multi-Sequential', file_name='COLORMAPS_LIST.md')
    call info%write(gradient='Diverging', file_name='COLORMAPS_LIST.md')
    call info%write(gradient='Categorical', file_name='COLORMAPS_LIST.md')
    call info%write(gradient='Cyclic', file_name='COLORMAPS_LIST.md')
    

    ! Deallocate all colormap information
    call info%finalize()

 end program write_info

