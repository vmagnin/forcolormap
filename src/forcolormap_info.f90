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
! Last modification: gha3mi 2024-02-16, vmagnin 2026-01-07
!-------------------------------------------------------------------------------

!> The Colormaps_info class offers information about each colormap.
module forcolormap_info

   use forcolormap_parameters, only: colormap_name_length, colormap_metadata
   use forcolormap_cm_scientific, only: scientific_metadata
   use forcolormap_cm_matplotlib, only: matplotlib_metadata
   use forcolormap_cm_miscellaneous, only: miscellaneous_metadata

   implicit none

   private

   public :: cmap_info

   !> Define a derived type that stores metadata for all colormaps.
   type :: Colormaps_info
      type(colormap_metadata) :: colormaps(222+4+6) = [ &
         scientific_metadata, &
         miscellaneous_metadata, &
         matplotlib_metadata]
   contains
      procedure :: write          !! Filter and output colormap metadata
      procedure :: get_ncolormaps !! Return the total number of colormaps
      procedure :: get_name       !! Return the name of a colormap by index
      procedure :: get_levels     !! Return the number of levels by index
   end type Colormaps_info

   !> Global instance providing access to colormap metadata.
   type(Colormaps_info), protected :: cmap_info

contains

   !> Return the total number of available colormaps.
   pure elemental function get_ncolormaps(this) result(ncolormaps_)
      class(Colormaps_info), intent(in) :: this
      integer :: ncolormaps_
      ncolormaps_ = size(this%colormaps)
   end function get_ncolormaps

   !> Return the colormap name for a given index.
   pure elemental function get_name(this, idx) result(name)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: name
      name = trim(this%colormaps(idx)%name)
   end function get_name

   !> Return the number of levels for a given index.
   pure function get_levels(this, idx) result(levels)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      integer :: levels
      levels = this%colormaps(idx)%levels
   end function get_levels

   !> Filter colormaps and write metadata.
   impure subroutine write(this, verbose, name, family, gradient, palette, author, license, levels, file_name, append)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in), optional :: verbose
      character(*), intent(in), optional :: name, family, gradient, palette, author, license
      integer, intent(in), optional :: levels
      character(*), intent(in), optional :: file_name
      logical, intent(in), optional :: append
      integer :: i, verbose_, nunit, n
      logical :: keep(this%get_ncolormaps())
      logical :: append_, apply_filter
      type(colormap_metadata) :: metadata

      integer :: w_name, w_family, w_gradient, w_palette, w_colorbar
      integer :: w_package, w_author, w_license, w_url, w_levels
      integer :: total_w1, total_w4
      character(len=1024) :: sep1, sep4
      character(len=32) :: tmp
      character(len=:), allocatable :: line
      character(len=:), allocatable :: f_name, f_family, f_gradient, f_palette, f_levels
      character(len=:), allocatable :: f_colorbar, f_package, f_author, f_license, f_url

      if (present(verbose)) then
         verbose_ = verbose
      else
         verbose_ = 1
      end if

      if (present(append)) then
         append_ = append
      else
         append_ = .false.
      end if

      n = this%get_ncolormaps()
      keep = .true.
      apply_filter = present(name) .or. present(family) .or. present(gradient) .or. present(palette) .or. &
                     present(author) .or. present(license) .or. present(levels)

      do i = 1, n
         if (present(name))     keep(i) = keep(i) .and. (this%colormaps(i)%name == name)
         if (present(family))   keep(i) = keep(i) .and. (this%colormaps(i)%family == family)
         if (present(gradient)) keep(i) = keep(i) .and. (this%colormaps(i)%gradient == gradient)
         if (present(palette))  keep(i) = keep(i) .and. (this%colormaps(i)%palette == palette)
         if (present(author))   keep(i) = keep(i) .and. (this%colormaps(i)%author == author)
         if (present(license))  keep(i) = keep(i) .and. (this%colormaps(i)%license == license)
         if (present(levels))   keep(i) = keep(i) .and. (this%colormaps(i)%levels == levels)
      end do

      ! Compute dynamic column widths.
      w_name     = len_trim('Name')
      w_family   = len_trim('Family')
      w_gradient = len_trim('Gradient')
      w_palette  = len_trim('Palette')
      w_levels   = len_trim('Levels')
      w_colorbar = len_trim('Colorbar')
      w_package  = len_trim('Package')
      w_author   = len_trim('Author')
      w_license  = len_trim('Licence')
      w_url      = len_trim('URL')

      do i = 1, n
         if (apply_filter .and. .not. keep(i)) cycle
         w_name     = max(w_name,     len_trim(this%colormaps(i)%name))
         w_family   = max(w_family,   len_trim(this%colormaps(i)%family))
         w_gradient = max(w_gradient, len_trim(this%colormaps(i)%gradient))
         w_palette  = max(w_palette,  len_trim(this%colormaps(i)%palette))
         write(tmp,'(i0)') this%colormaps(i)%levels
         w_levels   = max(w_levels,   len_trim(tmp))
         w_colorbar = max(w_colorbar, len_trim(this%colormaps(i)%colorbar))
         w_package  = max(w_package,  len_trim(this%colormaps(i)%package))
         w_author   = max(w_author,   len_trim(this%colormaps(i)%author))
         w_license  = max(w_license,  len_trim(this%colormaps(i)%license))
         w_url      = max(w_url,      max(1, len_trim(this%colormaps(i)%url)))
      end do

      total_w1 = w_name + w_family + w_gradient + w_palette + w_levels + w_colorbar + w_package + &
                 w_author + w_license + w_url + 2*9
      total_w4 = w_name + w_family + w_gradient + w_palette + w_levels + w_colorbar + 2*5

      sep1 = repeat('*', total_w1)
      sep4 = repeat('*', total_w4)

      ! Print header for verbose = 1 (full metadata).
      if (verbose_ == 1) then
         if (present(file_name)) then
            if (.not. append_) then
               open(newunit=nunit, file=trim(file_name), status='replace', action='write')
               write(nunit,'(a)') ''
               line = '|'// &
                      'Name'//repeat(' ', max(0, w_name-len_trim('Name')))//'|'// &
                      'Family'//repeat(' ', max(0, w_family-len_trim('Family')))//'|'// &
                      'Gradient'//repeat(' ', max(0, w_gradient-len_trim('Gradient')))//'|'// &
                      'Palette'//repeat(' ', max(0, w_palette-len_trim('Palette')))//'|'// &
                      'Levels'//repeat(' ', max(0, w_levels-len_trim('Levels')))//'|'// &
                      'Colorbar'//repeat(' ', max(0, w_colorbar-len_trim('Colorbar')))//'|'// &
                      'Package'//repeat(' ', max(0, w_package-len_trim('Package')))//'|'// &
                      'Author'//repeat(' ', max(0, w_author-len_trim('Author')))//'|'// &
                      'Licence'//repeat(' ', max(0, w_license-len_trim('Licence')))//'|'// &
                      'URL'//repeat(' ', max(0, w_url-len_trim('URL')))//'|'
               write(nunit,'(a)') line
               line = '|'// &
                      repeat('-', max(1, w_name))//'|'// &
                      repeat('-', max(1, w_family))//'|'// &
                      repeat('-', max(1, w_gradient))//'|'// &
                      repeat('-', max(1, w_palette))//'|'// &
                      repeat('-', max(1, w_levels))//'|'// &
                      repeat('-', max(1, w_colorbar))//'|'// &
                      repeat('-', max(1, w_package))//'|'// &
                      repeat('-', max(1, w_author))//'|'// &
                      repeat('-', max(1, w_license))//'|'// &
                      repeat('-', max(1, w_url))//'|'
               write(nunit,'(a)') line
               close(nunit)
            end if
         else
            print*,''
            line = 'Name'//repeat(' ', max(0, w_name-len_trim('Name')))//'  '// &
                   'Family'//repeat(' ', max(0, w_family-len_trim('Family')))//'  '// &
                   'Gradient'//repeat(' ', max(0, w_gradient-len_trim('Gradient')))//'  '// &
                   'Palette'//repeat(' ', max(0, w_palette-len_trim('Palette')))//'  '// &
                   'Levels'//repeat(' ', max(0, w_levels-len_trim('Levels')))//'  '// &
                   'Colorbar'//repeat(' ', max(0, w_colorbar-len_trim('Colorbar')))//'  '// &
                   'Package'//repeat(' ', max(0, w_package-len_trim('Package')))//'  '// &
                   'Author'//repeat(' ', max(0, w_author-len_trim('Author')))//'  '// &
                   'Licence'//repeat(' ', max(0, w_license-len_trim('Licence')))//'  '// &
                   'URL'//repeat(' ', max(0, w_url-len_trim('URL')))
            write(*,'(a)') line
            write(*,'(a)') sep1(1:total_w1)
         end if
      end if

      ! Print header for verbose = 4 (compact metadata).
      if (verbose_ == 4) then
         if (present(file_name)) then
            if (.not. append_) then
               open(newunit=nunit, file=trim(file_name), status='replace', action='write')
               write(nunit,'(a)') ''
               line = '|'// &
                      'Name'//repeat(' ', max(0, w_name-len_trim('Name')))//'|'// &
                      'Family'//repeat(' ', max(0, w_family-len_trim('Family')))//'|'// &
                      'Gradient'//repeat(' ', max(0, w_gradient-len_trim('Gradient')))//'|'// &
                      'Palette'//repeat(' ', max(0, w_palette-len_trim('Palette')))//'|'// &
                      'Levels'//repeat(' ', max(0, w_levels-len_trim('Levels')))//'|'// &
                      'Colorbar'//repeat(' ', max(0, w_colorbar-len_trim('Colorbar')))//'|'
               write(nunit,'(a)') line
               line = '|'// &
                      repeat('-', max(1, w_name))//'|'// &
                      repeat('-', max(1, w_family))//'|'// &
                      repeat('-', max(1, w_gradient))//'|'// &
                      repeat('-', max(1, w_palette))//'|'// &
                      repeat('-', max(1, w_levels))//'|'// &
                      repeat('-', max(1, w_colorbar))//'|'
               write(nunit,'(a)') line
               close(nunit)
            end if
         else
            print*,''
            line = 'Name'//repeat(' ', max(0, w_name-len_trim('Name')))//'  '// &
                   'Family'//repeat(' ', max(0, w_family-len_trim('Family')))//'  '// &
                   'Gradient'//repeat(' ', max(0, w_gradient-len_trim('Gradient')))//'  '// &
                   'Palette'//repeat(' ', max(0, w_palette-len_trim('Palette')))//'  '// &
                   'Levels'//repeat(' ', max(0, w_levels-len_trim('Levels')))//'  '// &
                   'Colorbar'//repeat(' ', max(0, w_colorbar-len_trim('Colorbar')))
            write(*,'(a)') line
            write(*,'(a)') sep4(1:total_w4)
         end if
      end if

      if (present(file_name) .and. present(gradient) .and. append_) then
         open(newunit=nunit, file=trim(file_name), position='append', status='unknown', action='write')
         write(nunit,'(a)') ''
         if (verbose_ == 4) then
            line = '|'// &
                   'Name'//repeat(' ', max(0, w_name-len_trim('Name')))//'|'// &
                   'Family'//repeat(' ', max(0, w_family-len_trim('Family')))//'|'// &
                   'Gradient'//repeat(' ', max(0, w_gradient-len_trim('Gradient')))//'|'// &
                   'Palette'//repeat(' ', max(0, w_palette-len_trim('Palette')))//'|'// &
                   'Levels'//repeat(' ', max(0, w_levels-len_trim('Levels')))//'|'// &
                   'Colorbar'//repeat(' ', max(0, w_colorbar-len_trim('Colorbar')))//'|'
            write(nunit,'(a)') line
            line = '|'// &
                   repeat('-', max(1, w_name))//'|'// &
                   repeat('-', max(1, w_family))//'|'// &
                   repeat('-', max(1, w_gradient))//'|'// &
                   repeat('-', max(1, w_palette))//'|'// &
                   repeat('-', max(1, w_levels))//'|'// &
                   repeat('-', max(1, w_colorbar))//'|'
            write(nunit,'(a)') line
         else if (verbose_ == 1) then
            line = '|'// &
                   'Name'//repeat(' ', max(0, w_name-len_trim('Name')))//'|'// &
                   'Family'//repeat(' ', max(0, w_family-len_trim('Family')))//'|'// &
                   'Gradient'//repeat(' ', max(0, w_gradient-len_trim('Gradient')))//'|'// &
                   'Palette'//repeat(' ', max(0, w_palette-len_trim('Palette')))//'|'// &
                   'Levels'//repeat(' ', max(0, w_levels-len_trim('Levels')))//'|'// &
                   'Colorbar'//repeat(' ', max(0, w_colorbar-len_trim('Colorbar')))//'|'// &
                   'Package'//repeat(' ', max(0, w_package-len_trim('Package')))//'|'// &
                   'Author'//repeat(' ', max(0, w_author-len_trim('Author')))//'|'// &
                   'Licence'//repeat(' ', max(0, w_license-len_trim('Licence')))//'|'// &
                   'URL'//repeat(' ', max(0, w_url-len_trim('URL')))//'|'
            write(nunit,'(a)') line
            line = '|'// &
                   repeat('-', max(1, w_name))//'|'// &
                   repeat('-', max(1, w_family))//'|'// &
                   repeat('-', max(1, w_gradient))//'|'// &
                   repeat('-', max(1, w_palette))//'|'// &
                   repeat('-', max(1, w_levels))//'|'// &
                   repeat('-', max(1, w_colorbar))//'|'// &
                   repeat('-', max(1, w_package))//'|'// &
                   repeat('-', max(1, w_author))//'|'// &
                   repeat('-', max(1, w_license))//'|'// &
                   repeat('-', max(1, w_url))//'|'
            write(nunit,'(a)') line
         end if
         close(nunit)
      end if

      do i = 1, n
         if (apply_filter .and. .not. keep(i)) cycle

         metadata = this%colormaps(i)

         select case (verbose_)
          case (1)
            write(tmp,'(i0)') metadata%levels
            f_name     = adjustl(trim(metadata%name))
            f_family   = adjustl(trim(metadata%family))
            f_gradient = adjustl(trim(metadata%gradient))
            f_palette  = adjustl(trim(metadata%palette))
            f_levels   = adjustl(trim(tmp))
            f_colorbar = adjustl(trim(metadata%colorbar))
            f_package  = adjustl(trim(metadata%package))
            f_author   = adjustl(trim(metadata%author))
            f_license  = adjustl(trim(metadata%license))
            f_url      = adjustl(trim(metadata%url))

            if (present(file_name)) then
               line = '|'// &
                      f_name(1:min(w_name,len_trim(f_name)))//repeat(' ', max(0, w_name-len_trim(f_name)))//'|'// &
                      f_family(1:min(w_family,len_trim(f_family)))//repeat(' ', max(0, w_family-len_trim(f_family)))//'|'// &
                      f_gradient(1:min(w_gradient,len_trim(f_gradient)))//repeat(' ', max(0, w_gradient-len_trim(f_gradient)))//'|'// &
                      f_palette(1:min(w_palette,len_trim(f_palette)))//repeat(' ', max(0, w_palette-len_trim(f_palette)))//'|'// &
                      f_levels(1:min(w_levels,len_trim(f_levels)))//repeat(' ', max(0, w_levels-len_trim(f_levels)))//'|'// &
                      f_colorbar(1:min(w_colorbar,len_trim(f_colorbar)))//repeat(' ', max(0, w_colorbar-len_trim(f_colorbar)))//'|'// &
                      f_package(1:min(w_package,len_trim(f_package)))//repeat(' ', max(0, w_package-len_trim(f_package)))//'|'// &
                      f_author(1:min(w_author,len_trim(f_author)))//repeat(' ', max(0, w_author-len_trim(f_author)))//'|'// &
                      f_license(1:min(w_license,len_trim(f_license)))//repeat(' ', max(0, w_license-len_trim(f_license)))//'|'// &
                      f_url(1:min(w_url,len_trim(f_url)))//repeat(' ', max(0, w_url-len_trim(f_url)))//'|'
               open (newunit=nunit, file=trim(file_name), position='append', status='unknown', action='write')
               write (nunit,'(a)') line
               close (nunit)
            else
               line = f_name(1:min(w_name,len_trim(f_name)))//repeat(' ', max(0, w_name-len_trim(f_name)))//'  '// &
                      f_family(1:min(w_family,len_trim(f_family)))//repeat(' ', max(0, w_family-len_trim(f_family)))//'  '// &
                      f_gradient(1:min(w_gradient,len_trim(f_gradient)))//repeat(' ', max(0, w_gradient-len_trim(f_gradient)))//'  '// &
                      f_palette(1:min(w_palette,len_trim(f_palette)))//repeat(' ', max(0, w_palette-len_trim(f_palette)))//'  '// &
                      f_levels(1:min(w_levels,len_trim(f_levels)))//repeat(' ', max(0, w_levels-len_trim(f_levels)))//'  '// &
                      f_colorbar(1:min(w_colorbar,len_trim(f_colorbar)))//repeat(' ', max(0, w_colorbar-len_trim(f_colorbar)))//'  '// &
                      f_package(1:min(w_package,len_trim(f_package)))//repeat(' ', max(0, w_package-len_trim(f_package)))//'  '// &
                      f_author(1:min(w_author,len_trim(f_author)))//repeat(' ', max(0, w_author-len_trim(f_author)))//'  '// &
                      f_license(1:min(w_license,len_trim(f_license)))//repeat(' ', max(0, w_license-len_trim(f_license)))//'  '// &
                      f_url(1:min(w_url,len_trim(f_url)))//repeat(' ', max(0, w_url-len_trim(f_url)))
               write(*,'(a)') line
            end if
          case (2)
            if (present(file_name)) then
               open (newunit=nunit, file=trim(file_name), position='append', status = 'unknown', action = 'write')
               write(nunit,'(a)')    ''
               write(nunit,'(a)')    '**********************************************'
               write(nunit,'(a,a)')  'Name    : ', metadata%name
               write(nunit,'(a,a)')  'Family  : ', metadata%family
               write(nunit,'(a,a)')  'Gradient: ', metadata%gradient
               write(nunit,'(a,a)')  'Palette : ', metadata%palette
               write(nunit,'(a,I4)') 'Levels  : ', metadata%levels
               write(nunit,'(a,a)')  'Colorbar: ', metadata%colorbar
               write(nunit,'(a,a)')  'Package : ', metadata%package
               write(nunit,'(a,a)')  'Author  : ', metadata%author
               write(nunit,'(a,a)')  'Licence : ', metadata%license
               write(nunit,'(a,a)')  'URL     : ', metadata%url
               write(nunit,'(a)')    '**********************************************'
               write(nunit,'(a)')    ''
               close(nunit)
            else
               print'(a)'    ,''
               print'(a)'    ,'**********************************************'
               print'(a,a)'  , 'Name    : ', metadata%name
               print'(a,a)'  , 'Family  : ', metadata%family
               print'(a,a)'  , 'Gradient: ', metadata%gradient
               print'(a,a)'  , 'Palette : ', metadata%palette
               print'(a,I4)' , 'Levels  : ', metadata%levels
               print'(a,a)'  , 'Colorbar: ', metadata%colorbar
               print'(a,a)'  , 'Package : ', metadata%package
               print'(a,a)'  , 'Author  : ', metadata%author
               print'(a,a)'  , 'Licence : ', metadata%license
               print'(a,a)'  , 'URL     : ', metadata%url
               print'(a)'    , '**********************************************'
               print'(a)'    ,''
            end if
          case (3)
            if (present(file_name)) then
               open (newunit=nunit, file=trim(file_name), position='append', status='unknown', action='write')
               write(nunit,'(a)') metadata%name
               close(nunit)
            else
               print'(a)',  metadata%name
            end if
          case (4)
            write(tmp,'(i0)') metadata%levels
            f_name     = adjustl(trim(metadata%name))
            f_family   = adjustl(trim(metadata%family))
            f_gradient = adjustl(trim(metadata%gradient))
            f_palette  = adjustl(trim(metadata%palette))
            f_levels   = adjustl(trim(tmp))
            f_colorbar = adjustl(trim(metadata%colorbar))

            if (present(file_name)) then
               line = '|'// &
                      f_name(1:min(w_name,len_trim(f_name)))//repeat(' ', max(0, w_name-len_trim(f_name)))//'|'// &
                      f_family(1:min(w_family,len_trim(f_family)))//repeat(' ', max(0, w_family-len_trim(f_family)))//'|'// &
                      f_gradient(1:min(w_gradient,len_trim(f_gradient)))//repeat(' ', max(0, w_gradient-len_trim(f_gradient)))//'|'// &
                      f_palette(1:min(w_palette,len_trim(f_palette)))//repeat(' ', max(0, w_palette-len_trim(f_palette)))//'|'// &
                      f_levels(1:min(w_levels,len_trim(f_levels)))//repeat(' ', max(0, w_levels-len_trim(f_levels)))//'|'// &
                      f_colorbar(1:min(w_colorbar,len_trim(f_colorbar)))//repeat(' ', max(0, w_colorbar-len_trim(f_colorbar)))//'|'
               open (newunit=nunit, file=trim(file_name), position='append', status='unknown', action='write')
               write (nunit,'(a)') line
               close (nunit)
            else
               line = f_name(1:min(w_name,len_trim(f_name)))//repeat(' ', max(0, w_name-len_trim(f_name)))//'  '// &
                      f_family(1:min(w_family,len_trim(f_family)))//repeat(' ', max(0, w_family-len_trim(f_family)))//'  '// &
                      f_gradient(1:min(w_gradient,len_trim(f_gradient)))//repeat(' ', max(0, w_gradient-len_trim(f_gradient)))//'  '// &
                      f_palette(1:min(w_palette,len_trim(f_palette)))//repeat(' ', max(0, w_palette-len_trim(f_palette)))//'  '// &
                      f_levels(1:min(w_levels,len_trim(f_levels)))//repeat(' ', max(0, w_levels-len_trim(f_levels)))//'  '// &
                      f_colorbar(1:min(w_colorbar,len_trim(f_colorbar)))//repeat(' ', max(0, w_colorbar-len_trim(f_colorbar)))
               write(*,'(a)') line
            end if
         end select
      end do

      if (present(file_name)) then
         open(newunit=nunit, file=trim(file_name), position='append', status='unknown', action='write')
         write(nunit,'(a)')'' ! Write a trailing blank line
         close(nunit)
      else
         print*,'' ! Write a trailing blank line
      end if

   end subroutine write

end module forcolormap_info
