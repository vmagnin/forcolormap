!> Global parameters of ForColormap.
module forcolormap_parameters
   use iso_fortran_env, only: wp=>real64
   implicit none
   private
   public :: wp, pi, colormap_name_length, colormap_metadata

   integer, parameter :: colormap_name_length = 30
   real(wp), parameter :: pi = 4 * atan(1.0_wp)

   type :: colormap_metadata
      character(len=colormap_name_length) :: name
      character(len=32)                   :: family
      character(len=24)                   :: gradient
      character(len=16)                   :: palette
      integer                             :: levels
      character(len=64)                   :: colorbar
      character(len=32)                   :: package
      character(len=32)                   :: author
      character(len=64)                   :: license
      character(len=64)                   :: url
   end type colormap_metadata

end module forcolormap_parameters
