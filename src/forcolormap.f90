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
! Last modification: gha3mi 2024-02-16, vmagnin 2026-01-07
!-------------------------------------------------------------------------------

!> The Colormap class and the `colormaps_list`.
module forcolormap
    use forcolormap_parameters, only: wp, colormap_name_length
    use forcolormap_utils, only: bezier, lagrange
    use forcolormap_cm_scientific
    use forcolormap_cm_matplotlib
    use forcolormap_cm_miscellaneous

    implicit none
    private

    public :: wp

    !> The Colormap class (attributes are encapsulated):
    type, public :: Colormap
        character(colormap_name_length), private :: name
        integer, private  :: levels         ! Number of levels
        real(wp), private :: zmin, zmax     ! z range
        ! An array containing for each level the associated RGB values:
        integer, dimension(:, :), allocatable, private :: map

        logical, private :: status(5) = .false.
        ! status(1): name validity
        ! status(2): zmin < zmax
        ! status(3): levels match colormap
        ! status(4): levels >= 1
        ! status(5): extract validity
    contains
        procedure :: set
        procedure :: finalize
        procedure :: create
        procedure :: create_lagrange
        procedure :: create_bezier
        procedure :: load
        procedure :: get_RGB
        procedure :: compute_RGB
        procedure :: get_name
        procedure :: get_levels
        procedure :: get_zmin
        procedure :: get_zmax
        procedure :: print
        procedure :: colorbar => write_ppm_colorbar
        procedure :: reverse
        procedure :: shift
        procedure :: extract
        procedure, private :: assign_map
        procedure, private :: check
        procedure :: print_status
    end type Colormap


contains

    !> Assign a colormap from a given "map" array
    pure subroutine assign_map(self, map)
        class(Colormap), intent(inout) :: self
        integer, intent(in) :: map(:, :)

        self%levels = size(map, 1)
        if (allocated(self%map)) then
            ! reallocate only if necessary
            if (size(self%map, 1) /= self%levels .or. size(self%map, 2) /= 3) then
                deallocate(self%map)
                allocate(self%map(0:self%levels-1, 1:3), source=map)
            end if
        else
            allocate(self%map(0:self%levels-1, 1:3), source=map)
        end if
    end subroutine assign_map

    !> Choose a colormap and set its parameters
    pure subroutine set(self, name, zmin, zmax, levels, varargs, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        integer, intent(in), optional :: levels
        real(wp), dimension(:), intent(in), optional :: varargs
        logical, intent(in), optional :: reverse

        self%name = trim(name)
        self%zmin = zmin
        self%zmax = zmax

        if (present(levels)) then
            self%levels = levels
        else
            self%levels = -256 ! This value will be fixed in the check() procedure
        end if

        ! Check validity of the colormap and fix it if necessary
        call self%check(check_name=.true., check_bounds=.true., check_levels=.true.)

        select case(self%name)
        ! Miscellaneous colormaps collection
        case("fire")
            ! Best with 256 levels but you can try other numbers:
            call fire_colormap(self%levels, self%map)
        case("rainbow")
            ! The user can not choose the number of levels:
            self%levels = 256
            call rainbow_colormap(self%map)
        case("inv_rainbow")
            ! The user can not choose the number of levels:
            self%levels = 256
            call inv_rainbow_colormap(self%map)
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
            call self%assign_map(acton)
        case("acton10")
            call self%assign_map(acton10)
        case("acton100")
            call self%assign_map(acton100)
        case("acton25")
            call self%assign_map(acton25)
        case("acton50")
            call self%assign_map(acton50)
        case("actonS")
            call self%assign_map(actonS)
        case("bam")
            call self%assign_map(bam)
        case("bam10")
            call self%assign_map(bam10)
        case("bam100")
            call self%assign_map(bam100)
        case("bam25")
            call self%assign_map(bam25)
        case("bam50")
            call self%assign_map(bam50)
        case("bamako")
            call self%assign_map(bamako)
        case("bamako10")
            call self%assign_map(bamako10)
        case("bamako100")
            call self%assign_map(bamako100)
        case("bamako25")
            call self%assign_map(bamako25)
        case("bamako50")
            call self%assign_map(bamako50)
        case("bamakoS")
            call self%assign_map(bamakoS)
        case("bamO")
            call self%assign_map(bamO)
        case("bamO10")
            call self%assign_map(bamO10)
        case("bamO100")
            call self%assign_map(bamO100)
        case("bamO25")
            call self%assign_map(bamO25)
        case("bamO50")
            call self%assign_map(bamO50)
        case("batlow")
            call self%assign_map(batlow)
        case("batlow10")
            call self%assign_map(batlow10)
        case("batlow100")
            call self%assign_map(batlow100)
        case("batlow25")
            call self%assign_map(batlow25)
        case("batlow50")
            call self%assign_map(batlow50)
        case("batlowK")
            call self%assign_map(batlowK)
        case("batlowK10")
            call self%assign_map(batlowK10)
        case("batlowK100")
            call self%assign_map(batlowK100)
        case("batlowK25")
            call self%assign_map(batlowK25)
        case("batlowK50")
            call self%assign_map(batlowK50)
        case("batlowKS")
            call self%assign_map(batlowKS)
        case("batlowS")
            call self%assign_map(batlowS)
        case("batlowW")
            call self%assign_map(batlowW)
        case("batlowW10")
            call self%assign_map(batlowW10)
        case("batlowW100")
            call self%assign_map(batlowW100)
        case("batlowW25")
            call self%assign_map(batlowW25)
        case("batlowW50")
            call self%assign_map(batlowW50)
        case("batlowWS")
            call self%assign_map(batlowWS)
        case("berlin")
            call self%assign_map(berlin)
        case("berlin10")
            call self%assign_map(berlin10)
        case("berlin100")
            call self%assign_map(berlin100)
        case("berlin25")
            call self%assign_map(berlin25)
        case("berlin50")
            call self%assign_map(berlin50)
        case("bilbao")
            call self%assign_map(bilbao)
        case("bilbao10")
            call self%assign_map(bilbao10)
        case("bilbao100")
            call self%assign_map(bilbao100)
        case("bilbao25")
            call self%assign_map(bilbao25)
        case("bilbao50")
            call self%assign_map(bilbao50)
        case("bilbaoS")
            call self%assign_map(bilbaoS)
        case("broc")
            call self%assign_map(broc)
        case("broc10")
            call self%assign_map(broc10)
        case("broc100")
            call self%assign_map(broc100)
        case("broc25")
            call self%assign_map(broc25)
        case("broc50")
            call self%assign_map(broc50)
        case("brocO")
            call self%assign_map(brocO)
        case("brocO10")
            call self%assign_map(brocO10)
        case("brocO100")
            call self%assign_map(brocO100)
        case("brocO25")
            call self%assign_map(brocO25)
        case("brocO50")
            call self%assign_map(brocO50)
        case("buda")
            call self%assign_map(buda)
        case("buda10")
            call self%assign_map(buda10)
        case("buda100")
            call self%assign_map(buda100)
        case("buda25")
            call self%assign_map(buda25)
        case("buda50")
            call self%assign_map(buda50)
        case("budaS")
            call self%assign_map(budaS)
        case("bukavu")
            call self%assign_map(bukavu)
        case("bukavu10")
            call self%assign_map(bukavu10)
        case("bukavu100")
            call self%assign_map(bukavu100)
        case("bukavu25")
            call self%assign_map(bukavu25)
        case("bukavu50")
            call self%assign_map(bukavu50)
        case("cork")
            call self%assign_map(cork)
        case("cork10")
            call self%assign_map(cork10)
        case("cork100")
            call self%assign_map(cork100)
        case("cork25")
            call self%assign_map(cork25)
        case("cork50")
            call self%assign_map(cork50)
        case("corkO")
            call self%assign_map(corkO)
        case("corkO10")
            call self%assign_map(corkO10)
        case("corkO100")
            call self%assign_map(corkO100)
        case("corkO25")
            call self%assign_map(corkO25)
        case("corkO50")
            call self%assign_map(corkO50)
        case("davos")
            call self%assign_map(davos)
        case("davos10")
            call self%assign_map(davos10)
        case("davos100")
            call self%assign_map(davos100)
        case("davos25")
            call self%assign_map(davos25)
        case("davos50")
            call self%assign_map(davos50)
        case("davosS")
            call self%assign_map(davosS)
        case("devon")
            call self%assign_map(devon)
        case("devon10")
            call self%assign_map(devon10)
        case("devon100")
            call self%assign_map(devon100)
        case("devon25")
            call self%assign_map(devon25)
        case("devon50")
            call self%assign_map(devon50)
        case("devonS")
            call self%assign_map(devonS)
        case("fes")
            call self%assign_map(fes)
        case("fes10")
            call self%assign_map(fes10)
        case("fes100")
            call self%assign_map(fes100)
        case("fes25")
            call self%assign_map(fes25)
        case("fes50")
            call self%assign_map(fes50)
        case("glasgow")
            call self%assign_map(glasgow)
        case("glasgow10")
            call self%assign_map(glasgow10)
        case("glasgow100")
            call self%assign_map(glasgow100)
        case("glasgow25")
            call self%assign_map(glasgow25)
        case("glasgow50")
            call self%assign_map(glasgow50)
        case("glasgowS")
            call self%assign_map(glasgowS)
        case("grayC")
            call self%assign_map(grayC)
        case("grayC10")
            call self%assign_map(grayC10)
        case("grayC100")
            call self%assign_map(grayC100)
        case("grayC25")
            call self%assign_map(grayC25)
        case("grayC50")
            call self%assign_map(grayC50)
        case("grayCS")
            call self%assign_map(grayCS)
        case("hawaii")
            call self%assign_map(hawaii)
        case("hawaii10")
            call self%assign_map(hawaii10)
        case("hawaii100")
            call self%assign_map(hawaii100)
        case("hawaii25")
            call self%assign_map(hawaii25)
        case("hawaii50")
            call self%assign_map(hawaii50)
        case("hawaiiS")
            call self%assign_map(hawaiiS)
        case("imola")
            call self%assign_map(imola)
        case("imola10")
            call self%assign_map(imola10)
        case("imola100")
            call self%assign_map(imola100)
        case("imola25")
            call self%assign_map(imola25)
        case("imola50")
            call self%assign_map(imola50)
        case("imolaS")
            call self%assign_map(imolaS)
        case("lajolla")
            call self%assign_map(lajolla)
        case("lajolla10")
            call self%assign_map(lajolla10)
        case("lajolla100")
            call self%assign_map(lajolla100)
        case("lajolla25")
            call self%assign_map(lajolla25)
        case("lajolla50")
            call self%assign_map(lajolla50)
        case("lajollaS")
            call self%assign_map(lajollaS)
        case("lapaz")
            call self%assign_map(lapaz)
        case("lapaz10")
            call self%assign_map(lapaz10)
        case("lapaz100")
            call self%assign_map(lapaz100)
        case("lapaz25")
            call self%assign_map(lapaz25)
        case("lapaz50")
            call self%assign_map(lapaz50)
        case("lapazS")
            call self%assign_map(lapazS)
        case("lipari")
            call self%assign_map(lipari)
        case("lipari10")
            call self%assign_map(lipari10)
        case("lipari100")
            call self%assign_map(lipari100)
        case("lipari25")
            call self%assign_map(lipari25)
        case("lipari50")
            call self%assign_map(lipari50)
        case("lipariS")
            call self%assign_map(lipariS)
        case("lisbon")
            call self%assign_map(lisbon)
        case("lisbon10")
            call self%assign_map(lisbon10)
        case("lisbon100")
            call self%assign_map(lisbon100)
        case("lisbon25")
            call self%assign_map(lisbon25)
        case("lisbon50")
            call self%assign_map(lisbon50)
        case("managua")
            call self%assign_map(managua)
        case("managua10")
            call self%assign_map(managua10)
        case("managua100")
            call self%assign_map(managua100)
        case("managua25")
            call self%assign_map(managua25)
        case("managua50")
            call self%assign_map(managua50)
        case("navia")
            call self%assign_map(navia)
        case("navia10")
            call self%assign_map(navia10)
        case("navia100")
            call self%assign_map(navia100)
        case("navia25")
            call self%assign_map(navia25)
        case("navia50")
            call self%assign_map(navia50)
        case("naviaS")
            call self%assign_map(naviaS)
        case("naviaW")
            call self%assign_map(naviaW)
        case("naviaW10")
            call self%assign_map(naviaW10)
        case("naviaW100")
            call self%assign_map(naviaW100)
        case("naviaW25")
            call self%assign_map(naviaW25)
        case("naviaW50")
            call self%assign_map(naviaW50)
        case("naviaWS")
            call self%assign_map(naviaWS)
        case("nuuk")
            call self%assign_map(nuuk)
        case("nuuk10")
            call self%assign_map(nuuk10)
        case("nuuk100")
            call self%assign_map(nuuk100)
        case("nuuk25")
            call self%assign_map(nuuk25)
        case("nuuk50")
            call self%assign_map(nuuk50)
        case("nuukS")
            call self%assign_map(nuukS)
        case("oleron")
            call self%assign_map(oleron)
        case("oleron10")
            call self%assign_map(oleron10)
        case("oleron100")
            call self%assign_map(oleron100)
        case("oleron25")
            call self%assign_map(oleron25)
        case("oleron50")
            call self%assign_map(oleron50)
        case("oslo")
            call self%assign_map(oslo)
        case("oslo10")
            call self%assign_map(oslo10)
        case("oslo100")
            call self%assign_map(oslo100)
        case("oslo25")
            call self%assign_map(oslo25)
        case("oslo50")
            call self%assign_map(oslo50)
        case("osloS")
            call self%assign_map(osloS)
        case("roma")
            call self%assign_map(roma)
        case("roma10")
            call self%assign_map(roma10)
        case("roma100")
            call self%assign_map(roma100)
        case("roma25")
            call self%assign_map(roma25)
        case("roma50")
            call self%assign_map(roma50)
        case("romaO")
            call self%assign_map(romaO)
        case("romaO10")
            call self%assign_map(romaO10)
        case("romaO100")
            call self%assign_map(romaO100)
        case("romaO25")
            call self%assign_map(romaO25)
        case("romaO50")
            call self%assign_map(romaO50)
        case("tofino")
            call self%assign_map(tofino)
        case("tofino10")
            call self%assign_map(tofino10)
        case("tofino100")
            call self%assign_map(tofino100)
        case("tofino25")
            call self%assign_map(tofino25)
        case("tofino50")
            call self%assign_map(tofino50)
        case("tokyo")
            call self%assign_map(tokyo)
        case("tokyo10")
            call self%assign_map(tokyo10)
        case("tokyo100")
            call self%assign_map(tokyo100)
        case("tokyo25")
            call self%assign_map(tokyo25)
        case("tokyo50")
            call self%assign_map(tokyo50)
        case("tokyoS")
            call self%assign_map(tokyoS)
        case("turku")
            call self%assign_map(turku)
        case("turku10")
            call self%assign_map(turku10)
        case("turku100")
            call self%assign_map(turku100)
        case("turku25")
            call self%assign_map(turku25)
        case("turku50")
            call self%assign_map(turku50)
        case("turkuS")
            call self%assign_map(turkuS)
        case("vanimo")
            call self%assign_map(vanimo)
        case("vanimo10")
            call self%assign_map(vanimo10)
        case("vanimo100")
            call self%assign_map(vanimo100)
        case("vanimo25")
            call self%assign_map(vanimo25)
        case("vanimo50")
            call self%assign_map(vanimo50)
        case("vik")
            call self%assign_map(vik)
        case("vik10")
            call self%assign_map(vik10)
        case("vik100")
            call self%assign_map(vik100)
        case("vik25")
            call self%assign_map(vik25)
        case("vik50")
            call self%assign_map(vik50)
        case("vikO")
            call self%assign_map(vikO)
        case("vikO10")
            call self%assign_map(vikO10)
        case("vikO100")
            call self%assign_map(vikO100)
        case("vikO25")
            call self%assign_map(vikO25)
        case("vikO50")
            call self%assign_map(vikO50)
        ! Matplotlib colormaps collection
        case("magma")
            call self%assign_map(magma)
        case("inferno")
            call self%assign_map(inferno)
        case("plasma")
            call self%assign_map(plasma)
        case("viridis")
            call self%assign_map(viridis)
        !
        case("black_body")
            call self%assign_map(black_body)
        case default
            self%name = "grayC"
            call self%assign_map(grayC)
        end select

        ! Reverse the colormap if requested
        if (present(reverse)) then
            if (reverse) call self%reverse()
        end if
    end subroutine set

    !> A finalizer procedure for memory cleanup:
    pure subroutine finalize(self)
        class(Colormap), intent(inout) :: self
        if (allocated(self%map)) deallocate(self%map)
    end subroutine


    !> Create a custom colormap from a "map" array.
    pure subroutine create(self, name, zmin, zmax, map, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        logical, intent(in), optional :: reverse
        integer, dimension(:, :), intent(in) :: map

        self%name   = trim(name)
        self%levels = size(map, 1)
        self%zmin   = zmin
        self%zmax   = zmax

        call self%check(check_bounds=.true., check_levels=.true.)

        call self%assign_map(map)

        ! Reverse the colormap if requested
        if (present(reverse)) then
            if (reverse) call self%reverse()
        end if
    end subroutine

    !> Create a custom colormap using Lagrange interpolation:
    pure subroutine create_lagrange(self, name, zmin, zmax, colors, levels, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        integer, dimension(:, :), intent(in) :: colors
        integer, intent(in) :: levels
        logical, intent(in), optional :: reverse

        self%name   = trim(name)
        self%levels = levels
        self%zmin   = zmin
        self%zmax   = zmax

        call self%check(check_bounds=.true., check_levels=.true.)

        call self%assign_map(lagrange(colors, self%levels))

        ! Reverse the colormap if requested
        if (present(reverse)) then
            if (reverse) call self%reverse()
        end if
    end subroutine

    !> Create a custom colormap using Bezier interpolation:
    pure subroutine create_bezier(self, name, zmin, zmax, colors, levels, reverse)
        class(Colormap), intent(inout) :: self
        character(*), intent(in) :: name
        real(wp), intent(in) :: zmin, zmax
        integer, dimension(:, :), intent(in) :: colors
        integer, intent(in) :: levels
        logical, intent(in), optional :: reverse

        self%name   = trim(name)
        self%levels = levels
        self%zmin   = zmin
        self%zmax   = zmax

        call self%check(check_bounds=.true., check_levels=.true.)

        call self%assign_map(bezier(colors, self%levels))

        ! Reverse the colormap if requested
        if (present(reverse)) then
            if (reverse) call self%reverse()
        end if
    end subroutine

    !> Load a .txt colormap with RGB integers separated by spaces on each line.
    !> Remark: if no path is indicated in filename, the .txt must be present
    !> at the root of the fpm project of the user.
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

            call self%check(check_bounds=.true.)

            ! Reverse the colormap if requested
            if (present(reverse)) then
                if (reverse) call self%reverse()
            end if
        else
            stop "ERROR: COLORMAP FILE NOT FOUND!"
        end if
    end subroutine load


    !> Compute the RGB values for a z real value
    pure subroutine compute_RGB(self, z, red, green, blue)
        class(Colormap), intent(in) :: self
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

        call self%get_RGB(level, red, green, blue)
    end subroutine

    !> Compute the RGB values directly from an integer level number
    pure subroutine get_RGB(self, level, red, green, blue)
        class(Colormap), intent(in) :: self
        integer, intent(in)  :: level
        integer, intent(out) :: red, green, blue

        red =   self%map(level, 1)
        green = self%map(level, 2)
        blue =  self%map(level, 3)
    end subroutine

   !> Returns the name of the colormap
    pure function get_name(self) result(name)
        class(Colormap), intent(in) :: self
        character(colormap_name_length) :: name

        name = self%name
    end function

    !> Returns the number of levels in the colormap
    pure function get_levels(self) result(levels)
        class(Colormap), intent(in) :: self
        integer :: levels

        levels = self%levels
    end function

    !> Returns the minimal value of the z range
    pure function get_zmin(self) result(zmin)
        class(Colormap), intent(in) :: self
        real(wp) :: zmin

        zmin = self%zmin
    end function

    !> Returns the maximal value of the z range
    pure function get_zmax(self) result(zmax)
        class(Colormap), intent(in) :: self
        real(wp) :: zmax

        zmax = self%zmax
    end function

    !> Useful for testing and debugging:
    impure subroutine print(self)
        class(Colormap), intent(in) :: self
        integer :: i

        print '(a,a)', "Name of the colormap: ", self%name
        print '(a,g0)', "zmin: ", self%zmin
        print '(a,g0)', "zmax: ", self%zmax
        print '(a,g0)', "Number of levels: ", self%levels
        do i = 0, self%levels-1
            print '(I3,2x,I3,2x,I3)', self%map(i, 1), self%map(i, 2), self%map(i, 3)
        end do
    end subroutine

    !> Writes the colorbar of the colormap in a PPM file
    impure subroutine write_ppm_colorbar(self, filename, width, height, encoding)
        use forimage, only: format_pnm
        class(Colormap), intent(in) :: self
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

        do concurrent (i = 0:pixwidth-1) local(z, red, green, blue, j)
            z = self%get_zmin() + i / real(pixwidth-1, kind=wp) * (self%get_zmax() - self%get_zmin())
            call self%compute_RGB(z, red, green, blue)
            do concurrent (j = 0: pixheight-1)
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

        call ppm%set_pnm(encoding = ppm%get_format(),&
            file_format = 'ppm',&
            width       = pixwidth,&
            height      = pixheight,&
            max_color   = 255,&
            comment     = 'comment',&
            pixels      = rgb_image)
        call ppm%export_pnm(filename)
    end subroutine write_ppm_colorbar

    !> Reverse the colormap
    pure subroutine reverse(self, name)
        class(Colormap), intent(inout) :: self
        character(*), intent(in), optional :: name
        self%map(:,:) = self%map(size(self%map,1)-1:0:-1, :)
        if (present(name)) then
            self%name = trim(name)
        else
            self%name = trim(self%name)//'_reverse'
        end if
    end subroutine reverse

    !> Apply a circular shift to the colormap (left is +, right is -)
    pure subroutine shift(self, sh)
        class(Colormap), intent(inout) :: self
        integer, intent(in) :: sh   !! The shift

        self%map(:,:) = cshift(self%map(:,:), sh)
    end subroutine shift

    !> Extracts colors from the colormap based on specified number of levels (extractedLevels).
    pure subroutine extract(self, extractedLevels, name, zmin, zmax, reverse)
        class(Colormap), intent(inout) :: self
        integer, intent(in) :: extractedLevels
        character(*), intent(in), optional :: name
        real(wp), intent(in), optional :: zmin, zmax
        logical, intent(in), optional :: reverse
        integer :: extracted_map(extractedLevels, 3), i, idx
        real(wp) :: factor
        character(3) :: extractedLevels_char

        ! Check if the number of extractedLevels is valid
        call self%check(check_extract=.true., extractedLevels=extractedLevels)
        if (.not. self%status(5)) return

        factor = real(self%levels-1, wp) / real(extractedLevels-1, wp)
        do concurrent (i = 1:extractedLevels) local(idx)
            idx = min(max(nint(real(i-1, wp) * factor), 0), self%levels-1)
            extracted_map(i, :) = self%map(idx, :)
        end do

        ! Name handling
        if (present(name)) then
            self%name = name
        else
            write(extractedLevels_char, '(I3)') extractedLevels
            self%name = self%name//trim(extractedLevels_char)
        end if

        ! Set zmin and zmax if provided
        if (present(zmin)) self%zmin = zmin
        if (present(zmax)) self%zmax = zmax

        ! Replace map
        call self%assign_map(extracted_map)

        if (present(reverse)) then
            if (reverse) call self%reverse()
        end if
    end subroutine extract

    !> Check the validity of the colormap and fix it if necessary
    pure subroutine check(self,check_name, check_bounds, check_levels, check_extract, extractedLevels)
        use forcolormap_info, only: cmap_info

        class(Colormap), intent(inout) :: self
        logical, intent(in), optional :: check_name, check_bounds, check_levels, check_extract
        integer, intent(in), optional :: extractedLevels
        real(wp) :: temp
        integer :: i, levels

        ! Initialize status array
        self%status = .true.

        if (present(check_name)) then
            if (check_name) then

                ! Check if the colormap is valid
                self%status(1) = .false.
                do i = 1, cmap_info%get_ncolormaps()
                    if (self%name == trim(cmap_info%get_name(i))) then
                        self%status(1) = .true.
                        exit
                    end if
                end do

                ! Fix the colormap if it is not valid
                if (self%status(1) .eqv. .false.) self%name = "grayC"

                ! Find the number of levels of the colormap
                do i = 1, cmap_info%get_ncolormaps()
                    if (self%name == trim(cmap_info%get_name(i))) then
                        levels = cmap_info%get_levels(i)
                        exit
                    end if
                end do

                ! Check if the number of levels is valid
                if (levels /= self%levels .or. self%levels < 1) then
                    if (self%levels /= -256) then
                        if (levels /= -1) then
                            self%status(3) = .false.
                            self%levels = levels
                        end if
                    else
                        self%levels = 256
                    end if
                end if

                ! Fix the number of levels if it is not valid
                if (self%status(3) .eqv. .false.) then
                    self%levels = levels
                end if

            end if
        end if

        if (present(check_bounds)) then
            if (check_bounds) then
                ! Check validity of zmin and zmax
                if (self%zmin > self%zmax) self%status(2) = .false.

                ! Fix zmin and zmax if they are not valid
                if (self%status(2) .eqv. .false.) then
                    temp      = self%zmin
                    self%zmin = self%zmax
                    self%zmax = temp
                end if

            end if
        end if

        if (present(check_levels)) then
            if (check_levels) then
                ! Check if the number of levels is valid
                if (self%levels < 1) then
                    self%status(4) = .false.
                    self%levels = 256
                end if
            end if
        end if

        ! Check validity of extractedLevels
        if (present(check_extract)) then
            if (check_extract) then
                if (.not. present(extractedLevels)) then
                    self%status(5) = .false.
                else if (extractedLevels <= 1 .or. extractedLevels > self%levels) then
                    self%status(5) = .false.
                else
                    self%status(5) = .true.
                end if
            end if
        end if

    end subroutine check

    !> Print error and fix messages for unvalid colormaps
    impure subroutine print_status(self)
        class(Colormap), intent(in) :: self
        integer :: i

        if (any(self%status .eqv. .false.)) then

            do i = 1, size(self%status)
                if (.not. self%status(i)) then
                    select case (i)
                    case (1)
                        print'(a)',&
                            "Error 1: Colormap name not found! 'grayC' is set by default."
                    case (2)
                        print'(a)',&
                            "Error 2: Min value (zmin) exceeds Max value (zmax)! zmin and zmax are swapped."
                    case (3)
                        print'(a)',&
                            "Error 3: Number of Levels (levels) doesn't match colormap! Levels adjusted to colormap."
                    case (4)
                        print'(a)',&
                        "Error 4: Number of Levels (levels) is less than 1! Levels adjusted to 256."
                    case (5)
                        print'(a)',&
                        "Error 5: Invalid extractedLevels. No extraction performed."
                    case default
                        print '(a)', "Unknown error!"
                    end select
                end if
            end do

        end if

    end subroutine print_status

end module forcolormap
