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

program colormaps_list
    use forcolormap_info, only: Colormaps_info
    implicit none
 
    type(Colormaps_info) :: info
 
    ! Set all colormap information
    call info%set_all()
 
    !! Write colormap information to a file
    call info%write(gradient='Sequential', file_name='colormaps_list/COLORMAPS_LIST_SEQ.md', verbose=4)
    call info%write(gradient='Multi-Sequential', file_name='colormaps_list/COLORMAPS_LIST_MSQ.md', verbose=4)
    call info%write(gradient='Diverging', file_name='colormaps_list/COLORMAPS_LIST_DIV.md', verbose=4)
    call info%write(gradient='Categorical', file_name='colormaps_list/COLORMAPS_LIST_CAT.md', verbose=4)
    call info%write(gradient='Cyclic', file_name='colormaps_list/COLORMAPS_LIST_CYC.md', verbose=4)   

    ! Deallocate all colormap information
    call info%finalize()

end program colormaps_list