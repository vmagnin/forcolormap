module miscellaneous_colormaps
    use colormap_parameters, only: colormap_name_length, wp, pi
    implicit none
    private

    public :: grey_colormap, inverted_grey_colormap, fire_colormap,&
              rainbow_colormap, inverted_rainbow_colormap, zebra_colormap,&
              cubehelix_colormap
    
    character(*), dimension(*), parameter, public :: miscellaneous_colormaps_list = &
        [character(colormap_name_length) :: &
        "grey", "inverted_grey", "fire", "rainbow", "inverted_rainbow", "zebra", "cubehelix"]

    contains

    pure subroutine grey_colormap(map)
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer :: levels, last, i

        ! The user can not choose the number of levels:
        levels = 256
        last = levels - 1
        allocate(map(0:last, 1:3))
        do concurrent(i = 0:last)
            map(i, :) = i
        end do
    end subroutine grey_colormap

    pure subroutine inverted_grey_colormap(map)
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer :: levels, last, i

        ! The user can not choose the number of levels:
        levels = 256
        last = levels - 1
        allocate(map(0:last, 1:3))
        do concurrent(i = 0:last)
            map(i, :) = last - i
        end do
    end subroutine inverted_grey_colormap

    pure subroutine fire_colormap(levels, map)
        integer, intent(in) :: levels
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer :: last, i

        ! Best with 256 levels but you can try other numbers:
        last = levels - 1
        allocate(map(0:last, 1:3))
        do concurrent (i = 0:last)
            map(i, 1) = nint(last * exp(-((last-i) / 200.0_wp)**7.0_wp))
            map(i, 2) = nint(last * exp(-((last-i) / 120.0_wp)**1.8_wp))
            map(i, 3) = nint(last * exp(-((last-i) /  40.0_wp)**0.7_wp))
        end do
    end subroutine fire_colormap

    pure subroutine rainbow_colormap(map)
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer :: levels, last, i

        ! The user can not choose the number of levels:
        levels = 256
        last = levels - 1
        allocate(map(0:last, 1:3))
        ! We add three gaussians (red, green, blue):
        do concurrent (i = 0:last)
            map(i, 1) = nint(last * exp(-((206-i) / 70.0_wp)**2.0_wp))
            map(i, 2) = nint(last * exp(-((156-i) / 70.0_wp)**2.0_wp))
            map(i, 3) = nint(last * exp(-((106-i) / 70.0_wp)**2.0_wp))
        end do
    end subroutine rainbow_colormap

    pure subroutine inverted_rainbow_colormap(map)
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer :: levels, last, i

        ! The user can not choose the number of levels:
        levels = 256
        last = levels - 1
        allocate(map(0:last, 1:3))
        ! We add three gaussians (red, green, blue):
        do concurrent (i = 0:last)
            map(i, 1) = nint(last * exp(-((106-i) / 70.0_wp)**2.0_wp))
            map(i, 2) = nint(last * exp(-((156-i) / 70.0_wp)**2.0_wp))
            map(i, 3) = nint(last * exp(-((206-i) / 70.0_wp)**2.0_wp))
        end do
    end subroutine inverted_rainbow_colormap

    pure subroutine zebra_colormap(map)
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer :: levels, last, i

        ! The user can not choose the number of levels:
        levels = 256
        last = levels - 1
        allocate(map(0:last, 1:3))
        ! Black and white zebras:
        do i = 0, 224, 32
            map(i   :i+15, :) = 0
            map(i+16:i+31, :) = 255
        end do
    end subroutine zebra_colormap

    !---------------------------------------------------------------------
    ! This subroutine is based on the public domain FORTRAN 77 subroutine
    ! published by D.A. Green:
    !   Green, D. A., 2011, Bulletin of the Astronomical Society of India,
    !      Vol.39, p.289
    ! For more information on the parameters of cubehelix, see his page:
    ! https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/
    !---------------------------------------------------------------------
    pure subroutine cubehelix_colormap(map, nlev, varargs)
        integer, dimension(:,:), allocatable, intent(out) :: map
        integer, intent(in) :: nlev
        real(wp), dimension(:), intent(in), optional :: varargs
        integer  :: i
        real(wp) :: start, rots, hue, gamma
        real(wp) :: fract, angle, amp

        if (present(varargs)) then
            if (size(varargs) /= 4) error stop "ERROR: cubehelix varargs(:) must have 4 values"
            start = varargs(1)
            rots  = varargs(2)
            hue   = varargs(3)
            gamma = varargs(4)
        else
            ! Default values:
            start = 0.5_wp
            rots  = -1.5_wp
            hue   = 1.0_wp
            gamma = 1.0_wp
        end if

        allocate(map(0:nlev-1, 1:3))

        do concurrent (i = 0:nlev-1)
            fract = real(i, kind=wp) / (nlev-1)
            angle = 2*pi * (start/3 + 1 + rots*fract)
            fract = fract ** gamma
            amp   = hue * fract * (1-fract)/2

            map(i, 1) = nint(255*(fract + amp*(-0.14861_wp*cos(angle) + 1.78277_wp*sin(angle))))
            map(i, 2) = nint(255*(fract + amp*(-0.29227_wp*cos(angle) - 0.90649_wp*sin(angle))))
            map(i, 3) = nint(255*(fract + amp*(+1.97294_wp*cos(angle))))
        end do
    end subroutine cubehelix_colormap
end module miscellaneous_colormaps