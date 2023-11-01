! The MIT License (MIT)
!
! Copyright (c) 2023 Vincent Magnin, gha3mi
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
! Last modification: gha3mi 2023-11-01
!-------------------------------------------------------------------------------


module forcolormap
    use colormap_parameters, only: wp, colormap_name_length
    use scientific_colour_maps
    use matplotlib_colormaps
    use miscellaneous_colormaps

    implicit none
    private

    public :: wp

    ! List of built-in colormaps:
    character(*), dimension(*), public, parameter :: colormaps_list = &
        [character(colormap_name_length) :: &
        miscellaneous_colormaps_list,&
        scientific_colour_maps_list,&
        matplotlib_colormaps_list]

    ! The Colormap class (attributes are encapsulated):
    type, public :: Colormap
        character(colormap_name_length), private :: name
        integer, private  :: levels         ! Number of levels
        real(wp), private :: zmin, zmax     ! z range
        ! An array containing for each level the associated RGB values:
        integer, dimension(:, :), allocatable, private :: map
    contains
        procedure :: set
        procedure :: create
        procedure :: load
        procedure :: get_RGB
        procedure :: compute_RGB
        procedure :: get_name
        procedure :: get_levels
        procedure :: get_zmin
        procedure :: get_zmax
        procedure :: print
        procedure :: colorbar => write_ppm_colorbar
        procedure, private :: reverse_map
    end type Colormap


contains

    pure subroutine set(self, name, zmin, zmax, levels, varargs, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        integer, intent(in), optional :: levels
        real(wp), dimension(:), intent(in), optional :: varargs
        logical, intent(in), optional :: reverse
        integer :: last
        integer :: i

        self%name = trim(name)
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

        select case(self%name)
        ! Miscellaneous colormaps collection
        case("grey")
            ! The user can not choose the number of levels:
            self%levels = 256
            call grey_colormap(self%map)
        case("inverted_grey")
            ! The user can not choose the number of levels:
            self%levels = 256
            call inverted_grey_colormap(self%map)
        case("fire")
            ! Best with 256 levels but you can try other numbers:
            call fire_colormap(self%levels, self%map)
        case("rainbow")
            ! The user can not choose the number of levels:
            self%levels = 256
            call rainbow_colormap(self%map)
        case("inverted_rainbow")
            ! The user can not choose the number of levels:
            self%levels = 256
            call inverted_rainbow_colormap(self%map)
        case("zebra")
            ! The user can not choose the number of levels:
            self%levels = 256
            call zebra_colormap(self%map)
        case("cubehelix")
            if (present(varargs)) then
                call cubehelix_colormap(self%map, self%levels, varargs)
            else
                call cubehelix_colormap(self%map, self%levels)
            end if
        ! Scientific colour maps collection (Fabio Crameri)
        ! (The user can not choose the number of levels)
        case("acton")
            call self%create(self%name, self%zmin, self%zmax, acton)
        case("actonS")
            call self%create(self%name, self%zmin, self%zmax, actonS)
        case("bamako")
            call self%create(self%name, self%zmin, self%zmax, bamako)
        case("bamakoS")
            call self%create(self%name, self%zmin, self%zmax, bamakoS)
        case("bam")
            call self%create(self%name, self%zmin, self%zmax, bam)
        case("bamO")
            call self%create(self%name, self%zmin, self%zmax, bamO)
        case("batlowK")
            call self%create(self%name, self%zmin, self%zmax, batlowK)
        case("batlowKS")
            call self%create(self%name, self%zmin, self%zmax, batlowKS)
        case("batlow")
            call self%create(self%name, self%zmin, self%zmax, batlow)
        case("batlowS")
            call self%create(self%name, self%zmin, self%zmax, batlowS)
        case("batlowW")
            call self%create(self%name, self%zmin, self%zmax, batlowW)
        case("batlowWS")
            call self%create(self%name, self%zmin, self%zmax, batlowWS)
        case("berlin")
            call self%create(self%name, self%zmin, self%zmax, berlin)
        case("bilbao")
            call self%create(self%name, self%zmin, self%zmax, bilbao)
        case("bilbaoS")
            call self%create(self%name, self%zmin, self%zmax, bilbaoS)
        case("broc")
            call self%create(self%name, self%zmin, self%zmax, broc)
        case("brocO")
            call self%create(self%name, self%zmin, self%zmax, brocO)
        case("buda")
            call self%create(self%name, self%zmin, self%zmax, buda)
        case("budaS")
            call self%create(self%name, self%zmin, self%zmax, budaS)
        case("bukavu")
            call self%create(self%name, self%zmin, self%zmax, bukavu)
        case("cork")
            call self%create(self%name, self%zmin, self%zmax, cork)
        case("corkO")
            call self%create(self%name, self%zmin, self%zmax, corkO)
        case("davos")
            call self%create(self%name, self%zmin, self%zmax, davos)
        case("davosS")
            call self%create(self%name, self%zmin, self%zmax, davosS)
        case("devon")
            call self%create(self%name, self%zmin, self%zmax, devon)
        case("devonS")
            call self%create(self%name, self%zmin, self%zmax, devonS)
        case("fes")
            call self%create(self%name, self%zmin, self%zmax, fes)
        case("glasgow")
            call self%create(self%name, self%zmin, self%zmax, glasgow)
        case("glasgowS")
            call self%create(self%name, self%zmin, self%zmax, glasgowS)
        case("grayC")
            call self%create(self%name, self%zmin, self%zmax, grayC)
        case("grayCS")
            call self%create(self%name, self%zmin, self%zmax, grayCS)
        case("hawaii")
            call self%create(self%name, self%zmin, self%zmax, hawaii)
        case("hawaiiS")
            call self%create(self%name, self%zmin, self%zmax, hawaiiS)
        case("imola")
            call self%create(self%name, self%zmin, self%zmax, imola)
        case("imolaS")
            call self%create(self%name, self%zmin, self%zmax, imolaS)
        case("lajolla")
            call self%create(self%name, self%zmin, self%zmax, lajolla)
        case("lajollaS")
            call self%create(self%name, self%zmin, self%zmax, lajollaS)
        case("lapaz")
            call self%create(self%name, self%zmin, self%zmax, lapaz)
        case("lapazS")
            call self%create(self%name, self%zmin, self%zmax, lapazS)
        case("lipari")
            call self%create(self%name, self%zmin, self%zmax, lipari)
        case("lipariS")
            call self%create(self%name, self%zmin, self%zmax, lipariS)
        case("lisbon")
            call self%create(self%name, self%zmin, self%zmax, lisbon)
        case("managua")
            call self%create(self%name, self%zmin, self%zmax, managua)
        case("navia")
            call self%create(self%name, self%zmin, self%zmax, navia)
        case("naviaS")
            call self%create(self%name, self%zmin, self%zmax, naviaS)
        case("naviaW")
            call self%create(self%name, self%zmin, self%zmax, naviaW)
        case("naviaWS")
            call self%create(self%name, self%zmin, self%zmax, naviaWS)
        case("nuuk")
            call self%create(self%name, self%zmin, self%zmax, nuuk)
        case("nuukS")
            call self%create(self%name, self%zmin, self%zmax, nuukS)
        case("oleron")
            call self%create(self%name, self%zmin, self%zmax, oleron)
        case("oslo")
            call self%create(self%name, self%zmin, self%zmax, oslo)
        case("osloS")
            call self%create(self%name, self%zmin, self%zmax, osloS)
        case("roma")
            call self%create(self%name, self%zmin, self%zmax, roma)
        case("romaO")
            call self%create(self%name, self%zmin, self%zmax, romaO)
        case("tofino")
            call self%create(self%name, self%zmin, self%zmax, tofino)
        case("tokyo")
            call self%create(self%name, self%zmin, self%zmax, tokyo)
        case("tokyoS")
            call self%create(self%name, self%zmin, self%zmax, tokyoS)
        case("turku")
            call self%create(self%name, self%zmin, self%zmax, turku)
        case("turkuS")
            call self%create(self%name, self%zmin, self%zmax, turkuS)
        case("vanimo")
            call self%create(self%name, self%zmin, self%zmax, vanimo)
        case("vik")
            call self%create(self%name, self%zmin, self%zmax, vik)
        case("vikO")
            call self%create(self%name, self%zmin, self%zmax, vikO)
        ! Matplotlib colormaps collection
        case("magma")
            call self%create(self%name, self%zmin, self%zmax, magma)
        case("inferno")
            call self%create(self%name, self%zmin, self%zmax, inferno) 
        case("plasma")
            call self%create(self%name, self%zmin, self%zmax, plasma) 
        case("viridis")
            call self%create(self%name, self%zmin, self%zmax, viridis) 
        case default
            error stop "Unknown colormap!"
        end select

        ! Reverse the colormap if requested
        if (present(reverse)) then
            if (reverse) call self%reverse_map()
        end if
    end subroutine set

    ! You can create a custom colormap:
    pure subroutine create(self, name, zmin, zmax, map, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        logical, intent(in), optional :: reverse
        integer, dimension(:, :), intent(in) :: map
        integer :: last

        self%name   = trim(name)
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

        ! Reverse the colormap if requested
        if (present(reverse)) then
            if (reverse) call self%reverse_map()
        end if
    end subroutine


    ! Load a .txt colormap with RGB integers separated by spaces on each line.
    ! Remark: if no path is indicated in filename, the .txt must be present
    ! at the root of the fpm project of the user.
    impure subroutine load(self, filename, zmin, zmax, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        real(wp), intent(in) :: zmin, zmax
        logical, intent(in), optional :: reverse
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

            self%name   = trim(filename)
            self%zmin   = zmin
            self%zmax   = zmax
            self%levels = n

            ! Reverse the colormap if requested
            if (present(reverse)) then
                if (reverse) call self%reverse_map()
            end if
        else
            stop "ERROR: COLORMAP FILE NOT FOUND!"
        end if
    end subroutine load


    ! Most of the time you will just give z to obtain RGB values:
    pure subroutine compute_RGB(self, z, red, green, blue)
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
    pure subroutine get_RGB(self, level, red, green, blue)
        class(Colormap), intent(inout) :: self
        integer, intent(in)  :: level
        integer, intent(out) :: red, green, blue

        red =   self%map(level, 1)
        green = self%map(level, 2)
        blue =  self%map(level, 3)
    end subroutine


    pure function get_name(self) result(name)
        class(Colormap), intent(in) :: self
        character(colormap_name_length) :: name

        name = self%name
    end function


    pure function get_levels(self) result(levels)
        class(Colormap), intent(in) :: self
        integer :: levels

        levels = self%levels
    end function


    pure function get_zmin(self) result(zmin)
        class(Colormap), intent(in) :: self
        real(wp) :: zmin

        zmin = self%zmin
    end function


    pure function get_zmax(self) result(zmax)
        class(Colormap), intent(in) :: self
        real(wp) :: zmax

        zmax = self%zmax
    end function

    ! Useful for testing and debugging:
    impure subroutine print(self)
        class(Colormap), intent(inout) :: self
        integer :: i

        print '(a,a)', "Name of the colormap: ", self%name
        print '(a,g0)', "zmin: ", self%zmin
        print '(a,g0)', "zmax: ", self%zmax
        print '(a,g0)', "Number of levels: ", self%levels
        do i = 0, self%levels-1
            print '(I3,2x,I3,2x,I3)', self%map(i, 1:3)
        end do
    end subroutine

    impure subroutine write_ppm_colorbar(self, filename, width, height, encoding)
        use forimage, only: format_pnm
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: filename
        integer :: i, j     ! Pixbuffer coordinates
        integer, intent(in), optional :: width, height
        integer :: pixwidth, pixheight
        integer, dimension(:,:), allocatable :: rgb_image
        integer  :: red, green, blue
        real(wp) :: z
        type(format_pnm) :: ppm
        character(*), intent(in), optional :: encoding

        if (present(width)) then
            pixwidth = width
        else
            pixwidth = 600
        end if

        if (present(height)) then
            pixheight = height
        else
            pixheight = 50
        end if

        allocate(rgb_image(pixheight,pixwidth*3))

        do i = 0, pixwidth-1
            do j = 0, pixheight-1
                z = self%get_zmin() + i / real(pixwidth-1, kind=wp) * (self%get_zmax() - self%get_zmin())
                call self%compute_RGB(z, red, green, blue)
                rgb_image(pixheight-j, 3*(i+1)-2) = red
                rgb_image(pixheight-j, 3*(i+1)-1) = green
                rgb_image(pixheight-j, 3*(i+1))   = blue  
            end do
        end do

        if (present(encoding)) then
            call ppm%set_format(encoding)
        else
            call ppm%set_format('binary')
        end if

        call ppm%set_pnm(encoding    = encoding,&
                         file_format = 'ppm',&
                         width       = pixwidth,&
                         height      = pixheight,&
                         max_color   = 255,&
                         comment     = 'comment',&
                         pixels      = rgb_image)
        call ppm%export_pnm(filename)
    end subroutine write_ppm_colorbar

    ! Reverse the colormap
    pure subroutine reverse_map(self, name)
        class(Colormap), intent(inout) :: self
        character(*), intent(in), optional :: name
        self%map(:,:) = self%map(size(self%map,1)-1:0:-1, :)
        if (present(name)) then
            self%name = trim(name)
        else
            self%name = trim(self%name)//'_reverse'
        end if
    end subroutine reverse_map
end module forcolormap
