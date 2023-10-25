! The MIT License (MIT)
!
! Copyright (c) 2023 Vincent Magnin
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
! Last modification: vmagnin 2023-10-19
!-------------------------------------------------------------------------------


module forcolormap
    use iso_fortran_env, only: wp=>real64
    use scientific_colour_maps

    implicit none

    real(wp), private, parameter :: pi = 4 * atan(1.0_wp)

    ! List of built-in colormaps:
    character(*), dimension(*), public, parameter :: colormaps_list = &
        [character(colormap_name_length) :: "grey", "inverted_grey", "fire", &
        & "rainbow", "inverted_rainbow", "zebra", &
        & "cubehelix", scientific_colour_maps_list]

    ! The Colormap class:
    type, public :: Colormap
        character(colormap_name_length), private :: name
        integer, private  :: levels         ! Number of levels
        real(wp), private :: zmin, zmax     ! z range
        ! An array containing for each level the associated RGB values:
        integer, dimension(:, :), allocatable, private :: map
    contains
        procedure, public :: set
        procedure, public :: create
        procedure, public :: load
        procedure, public :: get_RGB
        procedure, public :: compute_RGB
        procedure, public :: get_current
        procedure, public :: get_levels
        procedure, public :: get_zmin
        procedure, public :: get_zmax
        procedure, public :: print
        procedure, public :: test
    end type Colormap

    ! Elaborated colormaps are defined in their own subroutines:
    private :: cubehelix_colormap
    ! Auxiliary functions used by the test method:
    private :: write_ppm_test, write_ppm_colorbar

contains

    subroutine set(self, name, zmin, zmax, levels, varargs)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        integer, intent(in), optional :: levels
        real(wp), dimension(:), intent(in), optional :: varargs
        integer :: last
        integer :: i

        self%name = name
        self%zmin = zmin
        self%zmax = zmax

        if (present(levels)) then
            self%levels = levels
        else
            self%levels = 256       ! Default value
        end if
        ! The last level is:
        last = self%levels - 1

        ! Is the current colormap reseted?
        if (allocated(self%map)) then
            deallocate(self%map)
        end if
        ! The second dimension is for RGB: 1=Red, 2=Green, 3=Blue
        allocate(self%map(0:last, 1:3))

        select case(name)
        case("grey")
            ! The user can not choose the number of levels:
            self%levels = 256
            last = self%levels - 1
            do concurrent(i = 0:last)
                self%map(i, :) = i
            end do
        case("inverted_grey")
            ! The user can not choose the number of levels:
            self%levels = 256
            last = self%levels - 1
            do concurrent(i = 0:last)
                self%map(i, :) = last - i
            end do
        case("fire")
            ! Best with 256 levels but you can try other numbers:
            do concurrent (i = 0:last)
                self%map(i, 1) = nint(last * exp(-((last-i) / 200.0_wp)**7.0_wp))
                self%map(i, 2) = nint(last * exp(-((last-i) / 120.0_wp)**1.8_wp))
                self%map(i, 3) = nint(last * exp(-((last-i) /  40.0_wp)**0.7_wp))
            end do
        case("rainbow")
            ! The user can not choose the number of levels:
            self%levels = 256
            last = self%levels - 1
            ! We add three gaussians (red, green, blue):
            do concurrent (i = 0:last)
                self%map(i, 1) = nint(last * exp(-((206-i) / 70.0_wp)**2.0_wp))
                self%map(i, 2) = nint(last * exp(-((156-i) / 70.0_wp)**2.0_wp))
                self%map(i, 3) = nint(last * exp(-((106-i) / 70.0_wp)**2.0_wp))
            end do
        case("inverted_rainbow")
            ! The user can not choose the number of levels:
            self%levels = 256
            last = self%levels - 1
            ! We add three gaussians (red, green, blue):
            do concurrent (i = 0:last)
                self%map(i, 1) = nint(last * exp(-((106-i) / 70.0_wp)**2.0_wp))
                self%map(i, 2) = nint(last * exp(-((156-i) / 70.0_wp)**2.0_wp))
                self%map(i, 3) = nint(last * exp(-((206-i) / 70.0_wp)**2.0_wp))
            end do
        case("zebra")
            ! The user can not choose the number of levels:
            self%levels = 256
            last = self%levels - 1
            ! Black and white zebras:
            do i = 0, 224, 32
                self%map(i   :i+15, :) = 0
                self%map(i+16:i+31, :) = 255
            end do
        case("cubehelix")
            if (present(varargs)) then
                call cubehelix_colormap(self, self%levels, varargs)
            else
                call cubehelix_colormap(self, self%levels)
            end if
        ! Scientific colour maps collection (Fabio Crameri)
        ! (The user can not choose the number of levels)
        case("acton")
            call self%create("acton", self%zmin, self%zmax, acton)
        case("actonS")
            call self%create("actonS", self%zmin, self%zmax, actonS)
        case("bamako")
            call self%create("bamako", self%zmin, self%zmax, bamako)
        case("bamakoS")
            call self%create("bamakoS", self%zmin, self%zmax, bamakoS)
        case("bam")
            call self%create("bam", self%zmin, self%zmax, bam)
        case("bamO")
            call self%create("bamO", self%zmin, self%zmax, bamO)
        case("batlowK")
            call self%create("batlowK", self%zmin, self%zmax, batlowK)
        case("batlowKS")
            call self%create("batlowKS", self%zmin, self%zmax, batlowKS)
        case("batlow")
            call self%create("batlow", self%zmin, self%zmax, batlow)
        case("batlowS")
            call self%create("batlowS", self%zmin, self%zmax, batlowS)
        case("batlowW")
            call self%create("batlowW", self%zmin, self%zmax, batlowW)
        case("batlowWS")
            call self%create("batlowWS", self%zmin, self%zmax, batlowWS)
        case("berlin")
            call self%create("berlin", self%zmin, self%zmax, berlin)
        case("bilbao")
            call self%create("bilbao", self%zmin, self%zmax, bilbao)
        case("bilbaoS")
            call self%create("bilbaoS", self%zmin, self%zmax, bilbaoS)
        case("broc")
            call self%create("broc", self%zmin, self%zmax, broc)
        case("brocO")
            call self%create("brocO", self%zmin, self%zmax, brocO)
        case("buda")
            call self%create("buda", self%zmin, self%zmax, buda)
        case("budaS")
            call self%create("budaS", self%zmin, self%zmax, budaS)
        case("bukavu")
            call self%create("bukavu", self%zmin, self%zmax, bukavu)
        case("cork")
            call self%create("cork", self%zmin, self%zmax, cork)
        case("corkO")
            call self%create("corkO", self%zmin, self%zmax, corkO)
        case("davos")
            call self%create("davos", self%zmin, self%zmax, davos)
        case("davosS")
            call self%create("davosS", self%zmin, self%zmax, davosS)
        case("devon")
            call self%create("devon", self%zmin, self%zmax, devon)
        case("devonS")
            call self%create("devonS", self%zmin, self%zmax, devonS)
        case("fes")
            call self%create("fes", self%zmin, self%zmax, fes)
        case("glasgow")
            call self%create("glasgow", self%zmin, self%zmax, glasgow)
        case("glasgowS")
            call self%create("glasgowS", self%zmin, self%zmax, glasgowS)
        case("grayC")
            call self%create("grayC", self%zmin, self%zmax, grayC)
        case("grayCS")
            call self%create("grayCS", self%zmin, self%zmax, grayCS)
        case("hawaii")
            call self%create("hawaii", self%zmin, self%zmax, hawaii)
        case("hawaiiS")
            call self%create("hawaiiS", self%zmin, self%zmax, hawaiiS)
        case("imola")
            call self%create("imola", self%zmin, self%zmax, imola)
        case("imolaS")
            call self%create("imolaS", self%zmin, self%zmax, imolaS)
        case("lajolla")
            call self%create("lajolla", self%zmin, self%zmax, lajolla)
        case("lajollaS")
            call self%create("lajollaS", self%zmin, self%zmax, lajollaS)
        case("lapaz")
            call self%create("lapaz", self%zmin, self%zmax, lapaz)
        case("lapazS")
            call self%create("lapazS", self%zmin, self%zmax, lapazS)
        case("lipari")
            call self%create("lipari", self%zmin, self%zmax, lipari)
        case("lipariS")
            call self%create("lipariS", self%zmin, self%zmax, lipariS)
        case("lisbon")
            call self%create("lisbon", self%zmin, self%zmax, lisbon)
        case("managua")
            call self%create("managua", self%zmin, self%zmax, managua)
        case("navia")
            call self%create("navia", self%zmin, self%zmax, navia)
        case("naviaS")
            call self%create("naviaS", self%zmin, self%zmax, naviaS)
        case("naviaW")
            call self%create("naviaW", self%zmin, self%zmax, naviaW)
        case("naviaWS")
            call self%create("naviaWS", self%zmin, self%zmax, naviaWS)
        case("nuuk")
            call self%create("nuuk", self%zmin, self%zmax, nuuk)
        case("nuukS")
            call self%create("nuukS", self%zmin, self%zmax, nuukS)
        case("oleron")
            call self%create("oleron", self%zmin, self%zmax, oleron)
        case("oslo")
            call self%create("oslo", self%zmin, self%zmax, oslo)
        case("osloS")
            call self%create("osloS", self%zmin, self%zmax, osloS)
        case("roma")
            call self%create("roma", self%zmin, self%zmax, roma)
        case("romaO")
            call self%create("romaO", self%zmin, self%zmax, romaO)
        case("tofino")
            call self%create("tofino", self%zmin, self%zmax, tofino)
        case("tokyo")
            call self%create("tokyo", self%zmin, self%zmax, tokyo)
        case("tokyoS")
            call self%create("tokyoS", self%zmin, self%zmax, tokyoS)
        case("turku")
            call self%create("turku", self%zmin, self%zmax, turku)
        case("turkuS")
            call self%create("turkuS", self%zmin, self%zmax, turkuS)
        case("vanimo")
            call self%create("vanimo", self%zmin, self%zmax, vanimo)
        case("vik")
            call self%create("vik", self%zmin, self%zmax, vik)
        case("vikO")
            call self%create("vikO", self%zmin, self%zmax, vikO)
        case default
            stop "Unknown colormap!"
        end select
    end subroutine set

    ! You can create a custom colormap:
    subroutine create(self, name, zmin, zmax, map)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        integer, dimension(:, :), intent(in) :: map
        integer :: last

        self%name   = name
        self%levels = size(map(:, 1))
        last  = self%levels - 1
        self%zmin   = zmin
        self%zmax   = zmax

        ! Is the colormap reseted?
        if (allocated(self%map)) then
            deallocate(self%map)
        end if
        ! The second dimension is for RGB: 1=Red, 2=Green, 3=Blue
        allocate(self%map(0:last, 1:3))

        self%map = map
    end subroutine


    ! Load a .txt colormap with RGB integers separated by spaces on each line.
    ! Remark: if no path is indicated in filename, the .txt must be present
    ! at the root of the fpm project of the user.
    subroutine load(self, filename, zmin, zmax)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        real(wp), intent(in) :: zmin, zmax
        integer :: i, n
        integer :: red, green, blue
        logical :: file_found
        integer :: file_unit, ios

        inquire(file=filename, exist=file_found)

        if (file_found) then
            ! We first count the number of lines (RGB triplets):
            n = 0
            open(newunit=file_unit, file=filename)
            do
                read(file_unit, '(3I3)', iostat=ios) red, green, blue
                if (ios /= 0) exit
                n = n + 1
            end do
            close(file_unit)

            ! Is the colormap reseted?
            if (allocated(self%map)) then
                deallocate(self%map)
            end if
            allocate(self%map(0:n-1, 1:3))

            ! Then we read them and put them in the map:
            open(newunit=file_unit,  file=filename)
            do i = 0, n-1
                read(file_unit, *, iostat=ios) red, green, blue
                self%map(i, 1:3) = [red, green, blue]
                ! Should not happen:
                if (ios /= 0) exit
            end do
            close(file_unit)

            self%name   = filename
            self%zmin   = zmin
            self%zmax   = zmax
            self%levels = n
        else
            stop "ERROR: COLORMAP FILE NOT FOUND!"
        end if
    end subroutine load


    ! Most of the time you will just give z to obtain RGB values:
    subroutine compute_RGB(self, z, red, green, blue)
        class(Colormap), intent(inout) :: self
        real(wp), intent(in) :: z
        integer, intent(out) :: red, green, blue
        integer  :: level
        real(wp) :: zc      ! z after correction

        ! If ever z is not in [zmin, zmax], this will be fixed by:
        zc = min(max(z, self%zmin), self%zmax)

        ! Passing from the real scale to the integer scale
        ! (will also work for discrete colormaps)
        !  zmin .........zc...................zmax
        !     ^          ^                    ^
        !     |0|1|....|level|...........|last|levels
        level = int((zc - self%zmin) / (self%zmax - self%zmin) * self%levels)
        ! To avoid being out of range:
        level = min(max(level, 0), self%levels-1)

        call get_RGB(self, level, red, green, blue)
    end subroutine

    ! But you can also obtain RGB by giving directly a level number:
    subroutine get_RGB(self, level, red, green, blue)
        class(Colormap), intent(inout) :: self
        integer, intent(in)  :: level
        integer, intent(out) :: red, green, blue

        red =   self%map(level, 1)
        green = self%map(level, 2)
        blue =  self%map(level, 3)
    end subroutine


    function get_current(self) result(name)
        class(Colormap), intent(inout) :: self
        character(colormap_name_length) :: name

        name = self%name
    end function


    function get_levels(self) result(levels)
        class(Colormap), intent(inout) :: self
        integer :: levels

        levels = self%levels
    end function


    function get_zmin(self) result(zmin)
        class(Colormap), intent(inout) :: self
        real(wp) :: zmin

        zmin = self%zmin
    end function


    function get_zmax(self) result(zmax)
        class(Colormap), intent(inout) :: self
        real(wp) :: zmax

        zmax = self%zmax
    end function

    ! Useful for testing and debugging:
    subroutine print(self)
        class(Colormap), intent(inout) :: self
        integer :: i

        print *, "Name of the colormap: ", self%name
        print *, "zmin: ", self%zmin
        print *, "zmax: ", self%zmax
        print *, "Number of levels: ", self%levels
        do i = 0, self%levels-1
            print *, self%map(i, 1:3)
        end do
    end subroutine

    ! Creates two PPM output files for testing the colormap:
    subroutine test(self, prefix, encoding)
        class(Colormap), intent(inout) :: self
        character(*), optional, intent(in) :: prefix
        character(*), intent(in) :: encoding

        if (present(prefix)) then
            call write_ppm_test(self,     prefix//"_test", encoding)
            call write_ppm_colorbar(self, prefix//"_colorbar", encoding)
        else
            call write_ppm_test(self,     trim(self%name)//"_test", encoding)
            call write_ppm_colorbar(self, trim(self%name)//"_colorbar", encoding)
        end if
    end subroutine


    subroutine write_ppm_test(self, filename, encoding)
        use forimage, only: format_pnm
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        integer :: i, j     ! Pixbuffer coordinates
        integer, parameter :: pixwidth  = 600
        integer, parameter :: pixheight = 600
        integer, dimension(pixheight,pixwidth*3) :: rgb_image
        integer  :: red, green, blue
        real(wp) :: z
        type(format_pnm) :: ppm
        character(*), intent(in) :: encoding

        do i = 0, pixwidth-1
            do j = 0, pixheight-1
                ! Computing a z=f(x,y) function:
                z = 1.0_wp + sin(i*j/10000.0_wp) * cos(j/100.0_wp)
                ! The corresponding RGB values in our colormap:
                call self%compute_RGB(z, red, green, blue)
                rgb_image(pixheight-j, 3*(i+1)-2) = red
                rgb_image(pixheight-j, 3*(i+1)-1) = green
                rgb_image(pixheight-j, 3*(i+1))   = blue  
            end do
        end do

        call ppm%set_pnm(encoding    = encoding,&
                         file_format = 'ppm',&
                         width       = pixwidth,&
                         height      = pixheight,&
                         max_color   = 255,&
                         comment     = 'comment',&
                         pixels      = rgb_image)
        call ppm%export_pnm(filename)
    end subroutine write_ppm_test


    subroutine write_ppm_colorbar(self, filename, encoding)
        use forimage, only: format_pnm
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        integer :: i, j     ! Pixbuffer coordinates
        integer, parameter :: pixwidth  = 600
        integer, parameter :: pixheight = 50
        integer, dimension(pixheight,pixwidth*3) :: rgb_image
        integer  :: red, green, blue
        real(wp) :: z
        type(format_pnm) :: ppm
        character(*), intent(in) :: encoding

        do i = 0, pixwidth-1
            do j = 0, pixheight-1
                z = self%get_zmin() + i / real(pixwidth-1, kind=wp) * (self%get_zmax() - self%get_zmin())
                call self%compute_RGB(z, red, green, blue)
                rgb_image(pixheight-j, 3*(i+1)-2) = red
                rgb_image(pixheight-j, 3*(i+1)-1) = green
                rgb_image(pixheight-j, 3*(i+1))   = blue  
            end do
        end do

        call ppm%set_pnm(encoding    = encoding,&
                         file_format = 'ppm',&
                         width       = pixwidth,&
                         height      = pixheight,&
                         max_color   = 255,&
                         comment     = 'comment',&
                         pixels      = rgb_image)
        call ppm%export_pnm(filename)
    end subroutine write_ppm_colorbar

    !---------------------------------------------------------------------
    ! This subroutine is based on the public domain FORTRAN 77 subroutine
    ! published by D.A. Green:
    !   Green, D. A., 2011, Bulletin of the Astronomical Society of India,
    !      Vol.39, p.289
    ! For more information on the parameters of cubehelix, see his page:
    ! https://www.mrao.cam.ac.uk/~dag/CUBEHELIX/
    !---------------------------------------------------------------------
    subroutine cubehelix_colormap(self, nlev, varargs)
        class(Colormap), intent(inout) :: self
        integer, intent(in) :: nlev
        real(wp), dimension(:), intent(in), optional :: varargs
        integer  :: i
        real(wp) :: start, rots, hue, gamma
        real(wp) :: fract, angle, amp

        if (present(varargs)) then
            if (size(varargs) /= 4) stop "ERROR: cubehelix varargs(:) must have 4 values"
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

        do concurrent (i = 0:nlev-1)
            fract = real(i, kind=wp) / (nlev-1)
            angle = 2*pi * (start/3 + 1 + rots*fract)
            fract = fract ** gamma
            amp   = hue * fract * (1-fract)/2

            self%map(i, 1) = nint(255*(fract + amp*(-0.14861_wp*cos(angle) + 1.78277_wp*sin(angle))))
            self%map(i, 2) = nint(255*(fract + amp*(-0.29227_wp*cos(angle) - 0.90649_wp*sin(angle))))
            self%map(i, 3) = nint(255*(fract + amp*(+1.97294_wp*cos(angle))))
        end do
    end subroutine cubehelix_colormap

end module forcolormap
