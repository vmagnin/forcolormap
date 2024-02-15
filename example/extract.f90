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
! Contributed by gha3mi: 2023-11-07
! Last modification: gha3mi 2023-11-07, vmagnin 2024-02-15
!-------------------------------------------------------------------------------

! This example demonstrates the process of extracting a specified number of colors.
! The variable 'extractedLevels' represents the number of colors to be extracted from the colormap.
program extract

   use forcolormap, only: Colormap, wp

   implicit none

   type(Colormap) :: cmap

   ! Initialize the colormap
   call cmap%set('fes', 0.0_wp, 2.0_wp)

   ! Extract 10 colors from the colormap (extractedLevels=10)
   ! Optional arguments: 'name', 'zmin', 'zmax', and 'reverse' can be provided
   ! The extracted colormap will overwrite the existing colormap type (cmap)
   call cmap%extract(10)

   ! Generate a color bar for the extracted colormap
   call cmap%colorbar('fes10_ex_colorbar')

end program extract
