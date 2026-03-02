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
      procedure :: find_index     !! Return index by name (0 if not found)
      procedure :: get_family     !! Return the family of a colormap by index
      procedure :: get_gradient   !! Return the gradient type of a colormap by index
      procedure :: get_palette    !! Return the palette type of a colormap by index
      procedure :: get_colorbar   !! Return the colorbar type of a colormap by index
      procedure :: get_package    !! Return the package name of a colormap by index
      procedure :: get_author     !! Return the author of a colormap by index
      procedure :: get_license    !! Return the license of a colormap by index
      procedure :: get_url        !! Return the URL of a colormap by index
   end type Colormaps_info

   !> Global instance providing access to colormap metadata.
   ! type(Colormaps_info), protected :: cmap_info
   ! nvfortan requires to do it this way:
   type(Colormaps_info), protected :: cmap_info  = Colormaps_info([ &
         scientific_metadata, &
         miscellaneous_metadata, &
         matplotlib_metadata])

contains

   !> Return the family of a colormap by its index.
   pure function get_family(this, idx) result(family)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: family
      family = trim(this%colormaps(idx)%family)
   end function get_family

   !> Return the gradient type of a colormap by its index.
   pure function get_gradient(this, idx) result(gradient)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: gradient
      gradient = trim(this%colormaps(idx)%gradient)
   end function get_gradient

   !> Return the palette type of a colormap by its index.
   pure function get_palette(this, idx) result(palette)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: palette
      palette = trim(this%colormaps(idx)%palette)
   end function get_palette

   !> Return the colorbar type of a colormap by its index.
   pure function get_colorbar(this, idx) result(colorbar)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: colorbar
      colorbar = trim(this%colormaps(idx)%colorbar)
   end function get_colorbar

   !> Return the package name of a colormap by its index.
   pure function get_package(this, idx) result(package)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: package
      package = trim(this%colormaps(idx)%package)
   end function get_package

   !> Return the author of a colormap by its index.
   pure function get_author(this, idx) result(author)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: author
      author = trim(this%colormaps(idx)%author)
   end function get_author

   !> Return the license of a colormap by its index.
   pure function get_license(this, idx) result(license)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: license
      license = trim(this%colormaps(idx)%license)
   end function get_license

   !> Return the URL of a colormap by its index.
   pure function get_url(this, idx) result(url)
      class(Colormaps_info), intent(in) :: this
      integer, intent(in) :: idx
      character(colormap_name_length) :: url
      url = trim(this%colormaps(idx)%url)
   end function get_url

   !> Return the index of a colormap by its name. Returns 0 if not found.
   pure function find_index(this, name) result(idx)
      class(Colormaps_info), intent(in) :: this
      character(*), intent(in) :: name
      integer :: idx
      integer :: i
      character(colormap_name_length) :: key

      key = adjustl(trim(name))

      idx = 0
      do i = 1, size(this%colormaps)
         if (key == this%colormaps(i)%name) then
               idx = i
               exit
         end if
      end do
   end function find_index

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
      use, intrinsic :: iso_fortran_env, only: output_unit
      class(Colormaps_info), intent(in) :: this
      integer, intent(in), optional :: verbose
      character(*), intent(in), optional :: name, family, gradient, palette, author, license
      integer, intent(in), optional :: levels
      character(*), intent(in), optional :: file_name
      logical, intent(in), optional :: append
      integer :: i, verbose_, unit
      logical :: append_, apply_filter
      integer :: w_name, w_family, w_gradient, w_palette, w_colorbar, w_package, w_author, w_license, w_url, w_levels
      character(len=32) :: tmp
      character(*), parameter :: H_NAME     = 'Name'
      character(*), parameter :: H_FAMILY   = 'Family'
      character(*), parameter :: H_GRADIENT = 'Gradient'
      character(*), parameter :: H_PALETTE  = 'Palette'
      character(*), parameter :: H_LEVELS   = 'Levels'
      character(*), parameter :: H_COLORBAR = 'Colorbar'
      character(*), parameter :: H_PACKAGE  = 'Package'
      character(*), parameter :: H_AUTHOR   = 'Author'
      character(*), parameter :: H_LICENSE  = 'Licence'
      character(*), parameter :: H_URL      = 'URL'
      integer, parameter :: LH_NAME     = len(H_NAME)
      integer, parameter :: LH_FAMILY   = len(H_FAMILY)
      integer, parameter :: LH_GRADIENT = len(H_GRADIENT)
      integer, parameter :: LH_PALETTE  = len(H_PALETTE)
      integer, parameter :: LH_LEVELS   = len(H_LEVELS)
      integer, parameter :: LH_COLORBAR = len(H_COLORBAR)
      integer, parameter :: LH_PACKAGE  = len(H_PACKAGE)
      integer, parameter :: LH_AUTHOR   = len(H_AUTHOR)
      integer, parameter :: LH_LICENSE  = len(H_LICENSE)
      integer, parameter :: LH_URL      = len(H_URL)
      character(len=1), parameter :: SEP1 = '|'
      character(len=1), parameter :: SEP2 = '-'

      ! defaults
      verbose_ = 1
      if (present(verbose)) verbose_ = verbose
      append_  = .false.
      if (present(append)) append_  = append

      ! output unit
      if (present(file_name)) then
         if (append_) then
            open(newunit=unit, file=trim(file_name), status='unknown', position='append', action='write')
         else
            open(newunit=unit, file=trim(file_name), status='replace', action='write')
         end if
      else
         unit = output_unit
      end if

      apply_filter = present(name) .or. present(family) .or. present(gradient) .or. present(palette) .or. present(author) .or. present(license) .or. present(levels)

      ! initialize column widths from headers
      w_name     = LH_NAME
      w_family   = LH_FAMILY
      w_gradient = LH_GRADIENT
      w_palette  = LH_PALETTE
      w_levels   = LH_LEVELS
      w_colorbar = LH_COLORBAR
      w_package  = LH_PACKAGE
      w_author   = LH_AUTHOR
      w_license  = LH_LICENSE
      w_url      = LH_URL

      ! determine maximum column widths
      do i = 1, this%get_ncolormaps()
         if (.not. passes_filter(i)) cycle
         w_name     = max(w_name,     len_trim(this%colormaps(i)%name))
         w_family   = max(w_family,   len_trim(this%colormaps(i)%family))
         w_gradient = max(w_gradient, len_trim(this%colormaps(i)%gradient))
         w_palette  = max(w_palette,  len_trim(this%colormaps(i)%palette))
         write(tmp,'(i4)') this%colormaps(i)%levels
         w_levels   = max(w_levels,   len_trim(tmp))
         w_colorbar = max(w_colorbar, len_trim(this%colormaps(i)%colorbar))
         w_package  = max(w_package,  len_trim(this%colormaps(i)%package))
         w_author   = max(w_author,   len_trim(this%colormaps(i)%author))
         w_license  = max(w_license,  len_trim(this%colormaps(i)%license))
         w_url      = max(w_url,      len_trim(this%colormaps(i)%url))
      end do

      ! header
      select case (verbose_)
       case (1) ! full table
         write(unit,'(a)') SEP1// &
            cell(H_NAME,     w_name)     //SEP1// &
            cell(H_FAMILY,   w_family)   //SEP1// &
            cell(H_GRADIENT, w_gradient) //SEP1// &
            cell(H_PALETTE,  w_palette)  //SEP1// &
            cell(H_LEVELS,   w_levels)   //SEP1// &
            cell(H_COLORBAR, w_colorbar) //SEP1// &
            cell(H_PACKAGE,  w_package)  //SEP1// &
            cell(H_AUTHOR,   w_author)   //SEP1// &
            cell(H_LICENSE,  w_license)  //SEP1// &
            cell(H_URL,      w_url)      //SEP1

         write(unit,'(a)') SEP1// &
            repeat(SEP2, w_name)     //SEP1// &
            repeat(SEP2, w_family)   //SEP1// &
            repeat(SEP2, w_gradient) //SEP1// &
            repeat(SEP2, w_palette)  //SEP1// &
            repeat(SEP2, w_levels)   //SEP1// &
            repeat(SEP2, w_colorbar) //SEP1// &
            repeat(SEP2, w_package)  //SEP1// &
            repeat(SEP2, w_author)   //SEP1// &
            repeat(SEP2, w_license)  //SEP1// &
            repeat(SEP2, w_url)      //SEP1

       case (4) ! minimal table
         write(unit,'(a)') SEP1// &
            cell(H_NAME,     w_name)     //SEP1// &
            cell(H_FAMILY,   w_family)   //SEP1// &
            cell(H_GRADIENT, w_gradient) //SEP1// &
            cell(H_PALETTE,  w_palette)  //SEP1// &
            cell(H_LEVELS,   w_levels)   //SEP1// &
            cell(H_COLORBAR, w_colorbar) //SEP1

         write(unit,'(a)') SEP1// &
            repeat(SEP2, w_name)     //SEP1// &
            repeat(SEP2, w_family)   //SEP1// &
            repeat(SEP2, w_gradient) //SEP1// &
            repeat(SEP2, w_palette)  //SEP1// &
            repeat(SEP2, w_levels)   //SEP1// &
            repeat(SEP2, w_colorbar) //SEP1

       case default
         ! no header

      end select

      ! data
      do i = 1, this%get_ncolormaps()
         if (.not. passes_filter(i)) cycle

         select case (verbose_)
          case (1) ! full table
            write(tmp,'(i4)') this%colormaps(i)%levels
            write(unit,'(a)') SEP1// &
               cell(this%colormaps(i)%name,     w_name)     //SEP1// &
               cell(this%colormaps(i)%family,   w_family)   //SEP1// &
               cell(this%colormaps(i)%gradient, w_gradient) //SEP1// &
               cell(this%colormaps(i)%palette,  w_palette)  //SEP1// &
               cell(tmp,                        w_levels)   //SEP1// &
               cell(this%colormaps(i)%colorbar, w_colorbar) //SEP1// &
               cell(this%colormaps(i)%package,  w_package)  //SEP1// &
               cell(this%colormaps(i)%author,   w_author)   //SEP1// &
               cell(this%colormaps(i)%license,  w_license)  //SEP1// &
               cell(this%colormaps(i)%url,      w_url)      //SEP1

          case (2) ! box
            write(unit,'(a)')    ''
            write(unit,'(a)')    '**********************************************'
            write(unit,'(a,a)')  H_NAME//'    : ', this%colormaps(i)%name
            write(unit,'(a,a)')  H_FAMILY//'  : ', this%colormaps(i)%family
            write(unit,'(a,a)')  H_GRADIENT//': ', this%colormaps(i)%gradient
            write(unit,'(a,a)')  H_PALETTE//' : ', this%colormaps(i)%palette
            write(unit,'(a,I4)') H_LEVELS//'  : ', this%colormaps(i)%levels
            write(unit,'(a,a)')  H_COLORBAR//': ', this%colormaps(i)%colorbar
            write(unit,'(a,a)')  H_PACKAGE//' : ', this%colormaps(i)%package
            write(unit,'(a,a)')  H_AUTHOR//'  : ', this%colormaps(i)%author
            write(unit,'(a,a)')  H_LICENSE//' : ', this%colormaps(i)%license
            write(unit,'(a,a)')  H_URL//'     : ', this%colormaps(i)%url
            write(unit,'(a)')    '**********************************************'
            write(unit,'(a)')    ''

          case (3) ! name only
            write(unit,'(a)') this%colormaps(i)%name

          case (4) ! minimal table
            write(tmp,'(i4)') this%colormaps(i)%levels
            write(unit,'(a)') SEP1// &
               cell(this%colormaps(i)%name,     w_name)     //SEP1// &
               cell(this%colormaps(i)%family,   w_family)   //SEP1// &
               cell(this%colormaps(i)%gradient, w_gradient) //SEP1// &
               cell(this%colormaps(i)%palette,  w_palette)  //SEP1// &
               cell(tmp,                        w_levels)   //SEP1// &
               cell(this%colormaps(i)%colorbar, w_colorbar) //SEP1

          case default
            print *, 'Invalid verbose level: ', verbose_
            return
         end select
      end do

      write(unit,'(a)') ''
      if (present(file_name)) close(unit)

   contains

      pure logical function passes_filter(idx) result(ok)
         integer, intent(in) :: idx
         ok = .true.
         if (.not. apply_filter) return
         if (present(name))     ok = ok .and. (this%colormaps(idx)%name     == name)
         if (present(family))   ok = ok .and. (this%colormaps(idx)%family   == family)
         if (present(gradient)) ok = ok .and. (this%colormaps(idx)%gradient == gradient)
         if (present(palette))  ok = ok .and. (this%colormaps(idx)%palette  == palette)
         if (present(author))   ok = ok .and. (this%colormaps(idx)%author   == author)
         if (present(license))  ok = ok .and. (this%colormaps(idx)%license  == license)
         if (present(levels))   ok = ok .and. (this%colormaps(idx)%levels   == levels)
      end function passes_filter

      pure function cell(s, w) result(out)
         character(*), intent(in) :: s
         integer,      intent(in) :: w
         character(len=w)         :: out
         integer :: n
         character(len=len(s)) :: t
         t = adjustl(trim(s))
         n = min(w, len_trim(t))
         out = t(1:n)//repeat(' ', w - n)
      end function cell

   end subroutine write

end module forcolormap_info
