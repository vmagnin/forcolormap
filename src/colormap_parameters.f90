!> Global parameters of ForColormap.
module colormap_parameters
    use iso_fortran_env, only: wp=>real64
    implicit none
    private
    public :: wp, pi, colormap_name_length

    integer, parameter :: colormap_name_length = 30
    real(wp), parameter :: pi = 4 * atan(1.0_wp)
end module colormap_parameters
