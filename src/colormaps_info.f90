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
! Last modification: gha3mi 2024-01-06
!-------------------------------------------------------------------------------

module forcolormap_info

   implicit none

   private

   public :: Colormaps_info

   ! Define a derived type named 'table' to store information about a colormap
   type :: table
      character(len=:), allocatable, private :: name        ! Name of the colormap
      character(len=:), allocatable, private :: family      ! Family or category of the colormap
      character(len=:), allocatable, private :: gradient    ! Type of gradient used in the colormap
      character(len=:), allocatable, private :: palette     ! Palette used in the colormap
      character(len=:), allocatable, private :: colorbar    ! Colorbar style associated with the colormap
      character(len=:), allocatable, private :: package     ! Package or library associated with the colormap
      character(len=:), allocatable, private :: author      ! Author of the colormap
      character(len=:), allocatable, private :: license     ! License information for the colormap
      character(len=:), allocatable, private :: url         ! URL or web link to the colormap information
      integer                      , private :: levels      ! Number of discrete levels in the colormap
   contains
      procedure :: set_info   ! Procedure to set information for the colormap
      procedure :: write_info ! Procedure to print information about the colormap
      procedure :: finalize => deallocate_table ! Procedure to finalize the derived type
   end type table

   ! Define a derived type named 'Colormaps_info' to store an array of 'table' type
   type :: Colormaps_info
      type(table), private :: colormaps(223) ! Array of 'table' type to store multiple colormaps
   contains
      procedure :: set_all ! Procedure to set information for all colormaps in the array
      procedure :: write   ! Procedure to filter and write information about the colormaps
      procedure :: finalize => deallocate_Colormaps_info ! Procedure to finalize the derived type
   end type Colormaps_info

contains

   ! set information about the colormap
   pure elemental subroutine set_info(this, package, family, name, gradient, palette, author, license, url, colorbar, levels)
      class(table), intent(inout) :: this
      character(*), intent(in) :: package, family, name, gradient, palette, author, license, url, colorbar
      integer, intent(in) :: levels
      allocate(this%package, source=trim(package))
      allocate(this%family, source=trim(family))
      allocate(this%name, source=trim(name))
      allocate(this%gradient, source=trim(gradient))
      allocate(this%palette, source=trim(palette))
      allocate(this%author, source=trim(author))
      allocate(this%license, source=trim(license))
      allocate(this%url, source=trim(url))
      allocate(this%colorbar, source=trim(colorbar))
      this%levels = levels
   end subroutine set_info

   ! print information about the colormap
   impure subroutine write_info(this, verbose, file_name)
      class(table), intent(inout) :: this
      integer, intent(in), optional :: verbose
      character(*), intent(in), optional :: file_name
      integer :: verbose_, nunit
      character(len=256) :: format_table

      if (present(verbose)) then
         verbose_ = verbose
      else
         verbose_ = 1
      end if

      select case (verbose_)
       case (1)
         if (present(file_name)) then
            write(format_table,&
               '(a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,a,&
            &a)')&
               '(',&
               'a,','a', len_trim(this%name),',',10-len_trim(this%name)+2,'x',',',&
               'a,','a', len_trim(this%family),',',10-len_trim(this%family)+2,'x',',',&
               'a,','a', len_trim(this%gradient),',',18-len_trim(this%gradient)+2,'x',',',&
               'a,','a', len_trim(this%palette),',',12-len_trim(this%palette)+2,'x',',',&
               'a,','I4',',',3,'x',',',&
               'a,','a', len_trim(this%colorbar),',',23-len_trim(this%colorbar)+2,'x',',',&
               'a,','a', len_trim(this%package),',',25-len_trim(this%package)+2,'x',',',&
               'a,','a', len_trim(this%author),',',15-len_trim(this%author)+2,'x',',',&
               'a,','a', len_trim(this%license),',',18-len_trim(this%license)+2,'x',',',&
               'a,','a', len_trim(this%url),',',30-len_trim(this%url)+2,'x',',a',&
               ')'
            open (newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write (nunit,format_table)&
               '|',this%name,&
               '|',this%family,&
               '|',this%gradient,&
               '|',this%palette,&
               '|',this%levels,&
               '|',this%colorbar,&
               '|',this%package,&
               '|',this%author,&
               '|',this%license,&
               '|',this%url,'|'
            close (nunit)
         else
            write(format_table,&
               '(a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a)')&
               '(',&
               'a', len_trim(this%name),',',10-len_trim(this%name)+2,'x',',',&
               'a', len_trim(this%family),',',10-len_trim(this%family)+2,'x',',',&
               'a', len_trim(this%gradient),',',18-len_trim(this%gradient)+2,'x',',',&
               'a', len_trim(this%palette),',',12-len_trim(this%palette)+2,'x',',',&
               'I4',',',3,'x',',',&
               'a', len_trim(this%colorbar),',',23-len_trim(this%colorbar)+2,'x',',',&
               'a', len_trim(this%package),',',25-len_trim(this%package)+2,'x',',',&
               'a', len_trim(this%author),',',15-len_trim(this%author)+2,'x',',',&
               'a', len_trim(this%license),',',18-len_trim(this%license)+2,'x',',',&
               'a', len_trim(this%url),',',30-len_trim(this%url)+2,'x',&
               ')'
            print (format_table),&
               this%name,&
               this%family,&
               this%gradient,&
               this%palette,&
               this%levels,&
               this%colorbar,&
               this%package,&
               this%author,&
               this%license,&
               this%url
         end if
       case (2)
         if (present(file_name)) then
            open (newunit=nunit, file=trim(file_name), access='append', status = 'unknown', action = 'write')
            write(nunit,'(a)')    ''
            write(nunit,'(a)')    '**********************************************'
            write(nunit,'(a,a)')  'Name    : ', this%name
            write(nunit,'(a,a)')  'Family  : ', this%family
            write(nunit,'(a,a)')  'Gradient: ', this%gradient
            write(nunit,'(a,a)')  'Palette : ', this%palette
            write(nunit,'(a,I4)') 'Levels  : ', this%levels
            write(nunit,'(a,a)')  'Colorbar: ', this%colorbar
            write(nunit,'(a,a)')  'Package : ', this%package
            write(nunit,'(a,a)')  'Author  : ', this%author
            write(nunit,'(a,a)')  'Licence : ', this%license
            write(nunit,'(a,a)')  'URL     : ', this%url
            write(nunit,'(a)')    '**********************************************'
            write(nunit,'(a)')    ''
            close(nunit)
         else
            print'(a)'    ,''
            print'(a)'    ,'**********************************************'
            print'(a,a)'  , 'Name    : ', this%name
            print'(a,a)'  , 'Family  : ', this%family
            print'(a,a)'  , 'Gradient: ', this%gradient
            print'(a,a)'  , 'Palette : ', this%palette
            print'(a,I4)' , 'Levels  : ', this%levels
            print'(a,a)'  , 'Colorbar: ', this%colorbar
            print'(a,a)'  , 'Package : ', this%package
            print'(a,a)'  , 'Author  : ', this%author
            print'(a,a)'  , 'Licence : ', this%license
            print'(a,a)'  , 'URL     : ', this%url
            print'(a)'    , '**********************************************'
            print'(a)'    ,''
         end if
       case (3)
         if (present(file_name)) then
            open (newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write(nunit,'(a)') this%name
            close(nunit)
         else
            print'(a)',  this%name
         end if
      case (4)
         if (present(file_name)) then
            write(format_table,&
               '(a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,&
            &a,a,a,g0,a,a,&
            &a,a,g0,a,g0,a,a,a,&
            &a)')&
               '(',&
               'a,','a', len_trim(this%name),',',10-len_trim(this%name)+2,'x',',',&
               'a,','a', len_trim(this%family),',',10-len_trim(this%family)+2,'x',',',&
               'a,','a', len_trim(this%gradient),',',18-len_trim(this%gradient)+2,'x',',',&
               'a,','a', len_trim(this%palette),',',12-len_trim(this%palette)+2,'x',',',&
               'a,','I4',',',3,'x',',',&
               'a,','a', len_trim(this%colorbar),',',23-len_trim(this%colorbar)+2,'x',',a',&
               ')'
            open (newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write (nunit,format_table)&
               '|',this%name,&
               '|',this%family,&
               '|',this%gradient,&
               '|',this%palette,&
               '|',this%levels,&
               '|',this%colorbar,'|'
            close (nunit)
         else
            write(format_table,&
               '(a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a,a,g0,a,a,&
            &a,g0,a,g0,a,a,&
            &a)')&
               '(',&
               'a', len_trim(this%name),',',10-len_trim(this%name)+2,'x',',',&
               'a', len_trim(this%family),',',10-len_trim(this%family)+2,'x',',',&
               'a', len_trim(this%gradient),',',18-len_trim(this%gradient)+2,'x',',',&
               'a', len_trim(this%palette),',',12-len_trim(this%palette)+2,'x',',',&
               'I4',',',3,'x',',',&
               'a', len_trim(this%colorbar),',',23-len_trim(this%colorbar)+2,'x',&
               ')'
            print (format_table),&
               this%name,&
               this%family,&
               this%gradient,&
               this%palette,&
               this%levels,&
               this%colorbar
         end if
      end select
   end subroutine write_info

   ! filter the array of colormaps based on the given criteria and write information about the filtered colormaps
   impure subroutine write(this, verbose, name, family, gradient, palette, author, license, levels, file_name)
      class(Colormaps_info), intent(inout) :: this
      integer, intent(in), optional :: verbose
      character(*), intent(in), optional :: name, family, gradient, palette, author, license
      integer, intent(in), optional :: levels
      character(*), intent(in), optional :: file_name
      integer :: i, k, verbose_, nunit
      integer :: ind(size(this%colormaps),8) ! 1: index, 2: name, 3: family, 4: gradient, 5: palette, 6: author, 7: license, 8: levels
      integer, allocatable :: inter_ind(:)

      ! Set default values
      if (present(verbose)) then
         verbose_ = verbose
      else
         verbose_ = 1
      end if

      ! Print header for verbose = 1
      if (verbose_ == 1) then
         if (present(file_name)) then
            open(newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write(nunit,'(a)')'' ! Print empty line
            write(nunit,'(g0,8x,g0,6x,g0,12x,g0,7x,g0,1x,g0,17x,g0,20x,g0,11x,g0,13x,g0)') &
               '|Name', '|Family', '|Gradient', '|Palette', '|Levels', '|Colorbar', '|Package', '|Author', '|Licence', '|URL|'
            write(nunit,'(a)') '|---|---|---|---|---|---|---|---|---|---|'
            close(nunit)
         else
            print*,'' ! Print empty line
            print '(g0,8x,g0,6x,g0,12x,g0,7x,g0,1x,g0,17x,g0,20x,g0,11x,g0,13x,g0)', &
               'Name', 'Family', 'Gradient', 'Palette', 'Levels', 'Colorbar', 'Package', 'Author', 'Licence', 'URL'
            print'(a)', '**********************************************************************************************&
            &*******************************************************************************************'
         end if
      end if

      ! Print header for verbose = 4
      if (verbose_ == 4) then
         if (present(file_name)) then
            open(newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write(nunit,'(a)')'' ! Print empty line
            write(nunit,'(g0,8x,g0,6x,g0,12x,g0,7x,g0,1x,g0,17x,g0,20x,g0,11x,g0,8x,g0)') &
               '|Name', '|Family', '|Gradient', '|Palette', '|Levels', '|Colorbar                 |'
            write(nunit,'(a)') '|------------|------------|--------------------|--------------|-------|-------------------------|'
            close(nunit)
         else
            print*,'' ! Print empty line
            print '(g0,8x,g0,6x,g0,12x,g0,7x,g0,1x,g0)', &
               'Name', 'Family', 'Gradient', 'Palette', 'Levels', 'Colorbar'
            print'(a)', '**************************************************************************************************'
         end if
      end if

      if (present(name) .or.&
         present(family) .or.&
         present(gradient) .or.&
         present(palette) .or.&
         present(author) .or.&
         present(license) .or.&
         present(levels)) then

         ind = 0
         do i = 1, size(this%colormaps)
            ind(i, 1) = i
         end do

         if (present(name)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%name == name) then
                  ind(k,2) = i
                  k = k + 1
               end if
            end do
         end if
         if (present(family)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%family == family) then
                  ind(k,3) = i
                  k = k + 1
               end if
            end do
         end if
         if (present(gradient)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%gradient == gradient) then
                  ind(k,4) = i
                  k = k + 1
               end if
            end do
         end if
         if (present(palette)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%palette == palette) then
                  ind(k,5) = i
                  k = k + 1
               end if
            end do
         end if
         if (present(author)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%author == author) then
                  ind(k,6) = i
                  k = k + 1
               end if
            end do
         end if
         if (present(license)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%license == license) then
                  ind(k,7) = i
                  k = k + 1
               end if
            end do
         end if
         if (present(levels)) then
            k = 1
            do i = 1, size(this%colormaps)
               if (this%colormaps(i)%levels == levels) then
                  ind(k,8) = i
                  k = k + 1
               end if
            end do
         end if

         inter_ind = findColumnIntersections(ind(:,:))
         do i = 1, size(this%colormaps)
            do k = 1, size(inter_ind)
               if (inter_ind(k) == i) then
                  call this%colormaps(i)%write_info(verbose,file_name)
               end if
            end do
         end do

         if (present(file_name)) then
            open(newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write(nunit,'(a)')'' ! Print empty line
            close(nunit)
         else
            print*,'' ! Print empty line
         end if

      else
         do i = 1, size(this%colormaps)
            call this%colormaps(i)%write_info(verbose,file_name)
         end do
         if (present(file_name)) then
            open(newunit=nunit, file=trim(file_name), access='append', status='unknown', action='write')
            write(nunit,'(a)')'' ! Print empty line
            close(nunit)
         else
            print*,'' ! Print empty line
         end if
      end if

   end subroutine write

   ! set information for all colormaps
   pure elemental subroutine set_all(this)
      class(Colormaps_info), intent(inout) :: this
      integer :: i

      i = 1

      call this%colormaps(i)%set_info(&
         name       = "acton",&
         family     = "acton",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "acton_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "acton10",&
         family     = "acton",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "acton10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "acton25",&
         family     = "acton",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "acton25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "acton50",&
         family     = "acton",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "acton50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "acton100",&
         family     = "acton",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "acton100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "actonS",&
         family     = "acton",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "actonS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bam",&
         family     = "bam",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bam_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bam10",&
         family     = "bam",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "bam10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bam100",&
         family     = "bam",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "bam100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bam25",&
         family     = "bam",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "bam25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bam50",&
         family     = "bam",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "bam50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamako",&
         family     = "bamako",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bamako_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamako10",&
         family     = "bamako",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "bamako10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamako100",&
         family     = "bamako",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "bamako100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamako25",&
         family     = "bamako",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "bamako25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamako50",&
         family     = "bamako",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "bamako50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamakoS",&
         family     = "bamako",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bamakoS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamO",&
         family     = "bam",&
         gradient   = "Cyclic",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bamO_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamO10",&
         family     = "bam",&
         gradient   = "Cyclic",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "bamO10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamO100",&
         family     = "bam",&
         gradient   = "Cyclic",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "bamO100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamO25",&
         family     = "bam",&
         gradient   = "Cyclic",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "bamO25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bamO50",&
         family     = "bam",&
         gradient   = "Cyclic",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "bamO50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlow",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "batlow_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlow10",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "batlow10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlow100",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "batlow100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlow25",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "batlow25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlow50",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "batlow50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowK",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "batlowK_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowK10",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "batlowK10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowK100",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "batlowK100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowK25",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "batlowK25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowK50",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "batlowK50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowKS",&
         family     = "batlow",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "batlowKS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowS",&
         family     = "batlow",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "batlowS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowW",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "batlowW_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowW10",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "batlowW10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowW100",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "batlowW100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowW25",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "batlowW25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowW50",&
         family     = "batlow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "batlowW50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "batlowWS",&
         family     = "batlow",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "batlowWS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "berlin",&
         family     = "berlin",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "berlin_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "berlin10",&
         family     = "berlin",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "berlin10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "berlin100",&
         family     = "berlin",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "berlin100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "berlin25",&
         family     = "berlin",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "berlin25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "berlin50",&
         family     = "berlin",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "berlin50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bilbao",&
         family     = "bilbao",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bilbao_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bilbao10",&
         family     = "bilbao",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "bilbao10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bilbao100",&
         family     = "bilbao",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "bilbao100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bilbao25",&
         family     = "bilbao",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "bilbao25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bilbao50",&
         family     = "bilbao",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "bilbao50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bilbaoS",&
         family     = "bilbao",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bilbaoS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "broc",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "broc_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "broc10",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "broc10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "broc100",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "broc100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "broc25",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "broc25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "broc50",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "broc50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "brocO",&
         family     = "broc",&
         gradient   = "Cyclic",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "brocO_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "brocO10",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "brocO10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "brocO100",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "brocO100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "brocO25",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "brocO25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "brocO50",&
         family     = "broc",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "brocO50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "buda",&
         family     = "buda",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "buda_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "buda10",&
         family     = "buda",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "buda10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "buda100",&
         family     = "buda",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "buda100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "buda25",&
         family     = "buda",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "buda25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "buda50",&
         family     = "buda",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "buda50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "budaS",&
         family     = "buda",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "budaS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bukavu",&
         family     = "bukavu",&
         gradient   = "Multi-Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "bukavu_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bukavu10",&
         family     = "bukavu",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "bukavu10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bukavu100",&
         family     = "bukavu",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "bukavu100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bukavu25",&
         family     = "bukavu",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "bukavu25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "bukavu50",&
         family     = "bukavu",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "bukavu50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "cork",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "cork_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "cork10",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "cork10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "cork100",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "cork100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "cork25",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "cork25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "cork50",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "cork50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "corkO",&
         family     = "cork",&
         gradient   = "Cyclic",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "corkO_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "corkO10",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "corkO10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "corkO100",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "corkO100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "corkO25",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "corkO25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "corkO50",&
         family     = "cork",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "corkO50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "davos",&
         family     = "davos",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "davos_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "davos10",&
         family     = "davos",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "davos10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "davos100",&
         family     = "davos",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "davos100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "davos25",&
         family     = "davos",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "davos25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "davos50",&
         family     = "davos",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "davos50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "davosS",&
         family     = "davos",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "davosS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "devon",&
         family     = "devon",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "devon_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "devon10",&
         family     = "devon",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "devon10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "devon100",&
         family     = "devon",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "devon100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "devon25",&
         family     = "devon",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "devon25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "devon50",&
         family     = "devon",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "devon50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "devonS",&
         family     = "devon",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "devonS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "fes",&
         family     = "fes",&
         gradient   = "Multi-Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "fes_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "fes10",&
         family     = "fes",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "fes10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "fes100",&
         family     = "fes",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "fes100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "fes25",&
         family     = "fes",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "fes25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "fes50",&
         family     = "fes",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "fes50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "glasgow",&
         family     = "glasgow",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "glasgow_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "glasgow10",&
         family     = "glasgow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "glasgow10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "glasgow100",&
         family     = "glasgow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "glasgow100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "glasgow25",&
         family     = "glasgow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "glasgow25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "glasgow50",&
         family     = "glasgow",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "glasgow50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "glasgowS",&
         family     = "glasgow",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "glasgowS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "grayC",&
         family     = "grayC",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "grayC_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "grayC10",&
         family     = "grayC",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "grayC10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "grayC100",&
         family     = "grayC",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "grayC100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "grayC25",&
         family     = "grayC",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "grayC25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "grayC50",&
         family     = "grayC",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "grayC50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "grayCS",&
         family     = "grayC",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "grayCS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "hawaii",&
         family     = "hawaii",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "hawaii_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "hawaii10",&
         family     = "hawaii",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "hawaii10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "hawaii100",&
         family     = "hawaii",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "hawaii100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "hawaii25",&
         family     = "hawaii",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "hawaii25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "hawaii50",&
         family     = "hawaii",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "hawaii50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "hawaiiS",&
         family     = "hawaii",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "hawaiiS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "imola",&
         family     = "imola",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "imola_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "imola10",&
         family     = "imola",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "imola10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "imola100",&
         family     = "imola",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "imola100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "imola25",&
         family     = "imola",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "imola25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "imola50",&
         family     = "imola",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "imola50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "imolaS",&
         family     = "imola",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "imolaS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lajolla",&
         family     = "lajolla",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lajolla_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lajolla10",&
         family     = "lajolla",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "lajolla10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lajolla100",&
         family     = "lajolla",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "lajolla100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lajolla25",&
         family     = "lajolla",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "lajolla25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lajolla50",&
         family     = "lajolla",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "lajolla50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lajollaS",&
         family     = "lajolla",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lajollaS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lapaz",&
         family     = "lapaz",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lapaz_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lapaz10",&
         family     = "lapaz",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "lapaz10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lapaz100",&
         family     = "lapaz",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "lapaz100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lapaz25",&
         family     = "lapaz",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "lapaz25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lapaz50",&
         family     = "lapaz",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "lapaz50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lapazS",&
         family     = "lapaz",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lapazS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lipariS",&
         family     = "lipari",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lipariS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lipari",&
         family     = "lipari",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lipari_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lipari10",&
         family     = "lipari",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "lipari10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lipari100",&
         family     = "lipari",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "lipari100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lipari25",&
         family     = "lipari",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "lipari25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lipari50",&
         family     = "lipari",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "lipari50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lisbon",&
         family     = "lisbon",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "lisbon_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lisbon10",&
         family     = "lisbon",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "lisbon10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lisbon100",&
         family     = "lisbon",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "lisbon100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lisbon25",&
         family     = "lisbon",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "lisbon25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info(&
         name       = "lisbon50",&
         family     = "lisbon",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "lisbon50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "managua",&
         family     = "managua",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "managua_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "managua10",&
         family     = "managua",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "managua10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "managua100",&
         family     = "managua",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "managua100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "managua25",&
         family     = "managua",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "managua25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "managua50",&
         family     = "managua",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "managua50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "navia",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "navia_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "navia10",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "navia10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "navia100",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "navia100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "navia25",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "navia25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "navia50",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "navia50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaS",&
         family     = "navia",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "naviaS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaW",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "naviaW_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaWS",&
         family     = "navia",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "naviaWS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaW10",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "naviaW10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaW100",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "naviaW100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaW25",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "naviaW25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "naviaW50",&
         family     = "navia",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "naviaW50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "nuuk",&
         family     = "nuuk",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "nuuk_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "nuuk10",&
         family     = "nuuk",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "nuuk10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "nuuk100",&
         family     = "nuuk",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "nuuk100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "nuuk25",&
         family     = "nuuk",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "nuuk25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "nuuk50",&
         family     = "nuuk",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "nuuk50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "nuukS",&
         family     = "nuuk",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "nuukS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oleron",&
         family     = "oleron",&
         gradient   = "Multi-Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "oleron_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oleron10",&
         family     = "oleron",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "oleron10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oleron100",&
         family     = "oleron",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "oleron100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oleron25",&
         family     = "oleron",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "oleron25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oleron50",&
         family     = "oleron",&
         gradient   = "Multi-Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "oleron50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oslo",&
         family     = "oslo",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "oslo_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oslo10",&
         family     = "oslo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "oslo10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oslo100",&
         family     = "oslo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "oslo100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oslo25",&
         family     = "oslo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "oslo25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "oslo50",&
         family     = "oslo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "oslo50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "osloS",&
         family     = "oslo",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "osloS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "roma",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "roma_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "roma10",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "roma10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "roma100",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "roma100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "roma25",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "roma25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "roma50",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "roma50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "romaO",&
         family     = "roma",&
         gradient   = "Cyclic",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "romaO_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "romaO10",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "romaO10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "romaO100",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "romaO100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "romaO25",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "romaO25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "romaO50",&
         family     = "roma",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "romaO50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tofino",&
         family     = "tofino",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "tofino_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tofino10",&
         family     = "tofino",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "tofino10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tofino100",&
         family     = "tofino",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "tofino100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tofino25",&
         family     = "tofino",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "tofino25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tofino50",&
         family     = "tofino",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "tofino50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tokyo",&
         family     = "tokyo",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "tokyo_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tokyo10",&
         family     = "tokyo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "tokyo10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tokyo100",&
         family     = "tokyo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "tokyo100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tokyo25",&
         family     = "tokyo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "tokyo25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tokyo50",&
         family     = "tokyo",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "tokyo50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "tokyoS",&
         family     = "tokyo",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "tokyoS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "turku",&
         family     = "turku",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "turku_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "turku10",&
         family     = "turku",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "turku10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "turku100",&
         family     = "turku",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "turku100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "turku25",&
         family     = "turku",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "turku25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "turku50",&
         family     = "turku",&
         gradient   = "Sequential",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "turku50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "turkuS",&
         family     = "turku",&
         gradient   = "Categorical",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "turkuS_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vanimo",&
         family     = "vanimo",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "vanimo_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vanimo10",&
         family     = "vanimo",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "vanimo10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vanimo100",&
         family     = "vanimo",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "vanimo100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vanimo25",&
         family     = "vanimo",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "vanimo25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vanimo50",&
         family     = "vanimo",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "vanimo50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vik",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "vik_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vik10",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "vik10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vik100",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "vik100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vik25",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "vik25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vik50",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "vik50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vikO",&
         family     = "vik",&
         gradient   = "Cyclic",&
         palette    = "Continuous",&
         levels     = 256, &
         colorbar   = "vikO_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vikO10",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 10, &
         colorbar   = "vikO10_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vikO100",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 100, &
         colorbar   = "vikO100_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vikO25",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 25, &
         colorbar   = "vikO25_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "vikO50",&
         family     = "vik",&
         gradient   = "Diverging",&
         palette    = "Discrete",&
         levels     = 50, &
         colorbar   = "vikO50_colorbar.ppm",&
         package    = "Scientific Colour Map",&
         author     = "Fabio Crameri",&
         license    = "MIT license",&
         url        = "https://www.fabiocrameri.ch")

      i = i + 1

      call this%colormaps(i)%set_info( &
         name       = "black_body",&
         family     = "black_body",&
         gradient   = "Sequential",&
         palette    = "Continuous",&
         levels     = 1024, &
         colorbar   = "black_body_colorbar.ppm",&
         package    = "Miscellaneous",&
         author     = "Kenneth Moreland",&
         license    = "Public Domain (CC0)",&
         url        = "https://www.kennethmoreland.com")

   end subroutine set_all

   pure elemental subroutine deallocate_table(this)
      class(table), intent(inout) :: this
      if (allocated(this%name)) deallocate(this%name)
      if (allocated(this%family)) deallocate(this%family)
      if (allocated(this%gradient)) deallocate(this%gradient)
      if (allocated(this%palette)) deallocate(this%palette)
      if (allocated(this%colorbar)) deallocate(this%colorbar)
      if (allocated(this%package)) deallocate(this%package)
      if (allocated(this%author)) deallocate(this%author)
      if (allocated(this%license)) deallocate(this%license)
      if (allocated(this%url)) deallocate(this%url)
   end subroutine deallocate_table

   pure elemental subroutine deallocate_Colormaps_info(this)
      class(Colormaps_info), intent(inout) :: this
      ! integer :: i
      ! do i = 1, size(this%colormaps)
      !    call this%colormaps(i)%deallocate_table()
      ! end do
      call this%colormaps(:)%finalize()
   end subroutine deallocate_Colormaps_info

   ! Finds intersections between elements of the first column and other columns in the input array.
   pure function findColumnIntersections(array) result(intersections)
      integer, intent(in) :: array(:,:)
      integer, allocatable :: intersections(:)
      logical :: found(size(array, 1))
      integer :: i, j

      do i = 1, size(array, 1)
         found(i) = .true.
         do j = 2, size(array, 2)
            ! Check if all elements in the current column are non-zero
            if ( .not. all(array(:, j) == 0)) then
               ! Check if the current element in the first column exists in the current column
               found(i) = found(i) .and. any(array(i, 1) == array(:, j))
            end if
         end do
      end do

      ! Allocate and populate the intersections array
      allocate(intersections(size(pack(array(:, 1), mask=found))))
      intersections = pack(array(:, 1), mask=found)
   end function findColumnIntersections

end module forcolormap_info
