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
! Contributed by vmagnin: 2023-10-13
! Last modification: vmagnin 2023-10-31
!-------------------------------------------------------------------------------
! Run with the command:
! gfortran generate_scmap.f90 && ./a.out

program generate_scientific_colour_maps
    implicit none
    integer, parameter :: colormap_name_length = 30
    ! List of the Scientific coulour maps:
    character(*), dimension(*), parameter :: scientific_colour_maps_list = &
        [character(colormap_name_length) :: &
        "acton",&
        "acton10",&
        "acton100",&
        "acton25",&
        "acton50",&
        "actonS",&
        "bam",&
        "bam10",&
        "bam100",&
        "bam25",&
        "bam50",&
        "bamako",&
        "bamako10",&
        "bamako100",&
        "bamako25",&
        "bamako50",&
        "bamakoS",&
        "bamO",&
        "bamO10",&
        "bamO100",&
        "bamO25",&
        "bamO50",&
        "batlow",&
        "batlow10",&
        "batlow100",&
        "batlow25",&
        "batlow50",&
        "batlowK",&
        "batlowK10",&
        "batlowK100",&
        "batlowK25",&
        "batlowK50",&
        "batlowKS",&
        "batlowS",&
        "batlowW",&
        "batlowW10",&
        "batlowW100",&
        "batlowW25",&
        "batlowW50",&
        "batlowWS",&
        "berlin",&
        "berlin10",&
        "berlin100",&
        "berlin25",&
        "berlin50",&
        "bilbao",&
        "bilbao10",&
        "bilbao100",&
        "bilbao25",&
        "bilbao50",&
        "bilbaoS",&
        "broc",&
        "broc10",&
        "broc100",&
        "broc25",&
        "broc50",&
        "brocO",&
        "brocO10",&
        "brocO100",&
        "brocO25",&
        "brocO50",&
        "buda",&
        "buda10",&
        "buda100",&
        "buda25",&
        "buda50",&
        "budaS",&
        "bukavu",&
        "bukavu10",&
        "bukavu100",&
        "bukavu25",&
        "bukavu50",&
        "cork",&
        "cork10",&
        "cork100",&
        "cork25",&
        "cork50",&
        "corkO",&
        "corkO10",&
        "corkO100",&
        "corkO25",&
        "corkO50",&
        "davos",&
        "davos10",&
        "davos100",&
        "davos25",&
        "davos50",&
        "davosS",&
        "devon",&
        "devon10",&
        "devon100",&
        "devon25",&
        "devon50",&
        "devonS",&
        "fes",&
        "fes10",&
        "fes100",&
        "fes25",&
        "fes50",&
        "glasgow",&
        "glasgow10",&
        "glasgow100",&
        "glasgow25",&
        "glasgow50",&
        "glasgowS",&
        "grayC",&
        "grayC10",&
        "grayC100",&
        "grayC25",&
        "grayC50",&
        "grayCS",&
        "hawaii",&
        "hawaii10",&
        "hawaii100",&
        "hawaii25",&
        "hawaii50",&
        "hawaiiS",&
        "imola",&
        "imola10",&
        "imola100",&
        "imola25",&
        "imola50",&
        "imolaS",&
        "lajolla",&
        "lajolla10",&
        "lajolla100",&
        "lajolla25",&
        "lajolla50",&
        "lajollaS",&
        "lapaz",&
        "lapaz10",&
        "lapaz100",&
        "lapaz25",&
        "lapaz50",&
        "lapazS",&
        "lipari",&
        "lipari10",&
        "lipari100",&
        "lipari25",&
        "lipari50",&
        "lipariS",&
        "lisbon",&
        "lisbon10",&
        "lisbon100",&
        "lisbon25",&
        "lisbon50",&
        "managua",&
        "managua10",&
        "managua100",&
        "managua25",&
        "managua50",&
        "navia",&
        "navia10",&
        "navia100",&
        "navia25",&
        "navia50",&
        "naviaS",&
        "naviaW",&
        "naviaW10",&
        "naviaW100",&
        "naviaW25",&
        "naviaW50",&
        "naviaWS",&
        "nuuk",&
        "nuuk10",&
        "nuuk100",&
        "nuuk25",&
        "nuuk50",&
        "nuukS",&
        "oleron",&
        "oleron10",&
        "oleron100",&
        "oleron25",&
        "oleron50",&
        "oslo",&
        "oslo10",&
        "oslo100",&
        "oslo25",&
        "oslo50",&
        "osloS",&
        "roma",&
        "roma10",&
        "roma100",&
        "roma25",&
        "roma50",&
        "romaO",&
        "romaO10",&
        "romaO100",&
        "romaO25",&
        "romaO50",&
        "tofino",&
        "tofino10",&
        "tofino100",&
        "tofino25",&
        "tofino50",&
        "tokyo",&
        "tokyo10",&
        "tokyo100",&
        "tokyo25",&
        "tokyo50",&
        "tokyoS",&
        "turku",&
        "turku10",&
        "turku100",&
        "turku25",&
        "turku50",&
        "turkuS",&
        "vanimo",&
        "vanimo10",&
        "vanimo100",&
        "vanimo25",&
        "vanimo50",&
        "vik",&
        "vik10",&
        "vik100",&
        "vik25",&
        "vik50",&
        "vikO",&
        "vikO10",&
        "vikO100",&
        "vikO25",&
        "vikO50"]
    character(colormap_name_length)   :: cmap_name
    character(colormap_name_length+4) :: filename
    integer :: i, k, n
    integer :: red, green, blue
    ! Input file unit and ios argument:
    integer :: input_file, ios
    logical :: found
    ! Output files units:
    integer :: out1, out2

    ! The scientific_colour_maps module:
    open(newunit=out1, file="scientific_colour_maps.f90")

    write(out1, '(A)') "! The MIT License (MIT)"
    write(out1, '(A)') "!"
    write(out1, '(A)') "! Copyright (c) 2023, Fabio Crameri for the Scientific coulour maps 8.0.1"
    write(out1, '(A)') "!                     and Vincent Magnin for the Fortran translation"
    write(out1, '(A)') "!"
    write(out1, '(A)') "! Permission is hereby granted, free of charge, to any person obtaining a copy"
    write(out1, '(A)') '! of this software and associated documentation files (the "Software"), to deal'
    write(out1, '(A)') "! in the Software without restriction, including without limitation the rights"
    write(out1, '(A)') "! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell"
    write(out1, '(A)') "! copies of the Software, and to permit persons to whom the Software is"
    write(out1, '(A)') "! furnished to do so, subject to the following conditions:"
    write(out1, '(A)') "!"
    write(out1, '(A)') "! The above copyright notice and this permission notice shall be included in all"
    write(out1, '(A)') "! copies or substantial portions of the Software."
    write(out1, '(A)') "!"
    write(out1, '(A)') '! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR'
    write(out1, '(A)') "! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
    write(out1, '(A)') "! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
    write(out1, '(A)') "! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
    write(out1, '(A)') "! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,"
    write(out1, '(A)') "! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"
    write(out1, '(A)') "! SOFTWARE."
    write(out1, '(A)') "!-------------------------------------------------------------------------------"
    write(out1, '()')
    write(out1, '(A)') "module scientific_colour_maps"
    write(out1, '(4x, A)') "use colormap_parameters, only: colormap_name_length"
    write(out1, '(4x, A)') "implicit none"
    write(out1, '(4x, A)') "private"
    write(out1, '()')

    ! Generating the colormaps list:
    write(out1, '(4x, A)') "character(*), dimension(*), parameter, public :: scientific_colour_maps_list = &"
    write(out1, '(8x, A)') "[character(colormap_name_length) :: &"
    write(out1, '(8x)', advance='no')
    do i = 1, size(scientific_colour_maps_list)-1
        write(out1, '(4A)', advance='no') '"', trim(scientific_colour_maps_list(i)), '"', ", "
        ! We want six colormaps names per line:
        if (mod(i, 6) == 0) then
            write(out1, '(A)') "&"
            write(out1, '(8x)', advance='no')
        end if
    end do
    write(out1, '(8x, 4A)') '"', trim(scientific_colour_maps_list(i)), '"', "]"

    ! Generating both the arrays and the cases:
    open(newunit=out2, file="copy-paste_code.f90")
    write(out2, '(8x, A)') "! Scientific colour maps collection (Fabio Crameri):"

    do k = 1, size(scientific_colour_maps_list)
        cmap_name = trim(scientific_colour_maps_list(k))

        ! That code must be copied/pasted in the colormap_class.f90 file:
        write(out2, '( 8x, A, A, A)') 'case("', trim(cmap_name), '")'
        write(out2, '(12x, 5A)') 'call self%create(self%name,&
                                & self%zmin, self%zmax, ', trim(cmap_name), ")"

        ! We use the .lut files (ImageJ format) from the Scientific coulour maps (https://www.fabiocrameri.ch/colourmaps/):
        filename  = trim(cmap_name)//".lut"
        inquire(file=filename, exist=found)

        if (found) then
            print *, filename

            ! First scan: how many RGB triplets does the file contain?
            n = 0
            open(newunit=input_file, file=filename)
            do
                read(input_file, *, iostat=ios) red, green, blue
                if (ios /= 0) exit
                n = n + 1
            end do

            ! Second scan: we can now read the n triplets and put them in a Fortran array
            rewind(input_file)
            write(out1, '()')
            write(out1, '(4x, A, I0, 3A)') "integer, dimension(0:", n-1, &
                        &", 1:3), public :: ", trim(cmap_name), "=reshape( [ &"
            write(out1, '(8x)', advance='no')
            do i = 0, n-1
                read(input_file, *, iostat=ios) red, green, blue

                if (i /= n-1) then
                    write(out1, '(4x, I3, "," , I3, "," ,  I3, ", ")', advance='no') red, green, blue
                    ! We want four columns of RGB triplets:
                    if (mod(i+1, 4) == 0) then
                        write(out1, '(A)') "&"
                        write(out1, '(8x)', advance='no')
                    end if
                else
                    ! The last RGB triplet must not be followed by a comma:
                    write(out1, '(4x, I3, "," , I3, "," ,  I3, "  &")', advance='no') red, green, blue
                    exit
                end if

                ! Should not happen:
                if (ios /= 0) exit
            end do
            close(input_file)

            write(out1, '()')
            write(out1, '(12x, 3A)') "], shape(", trim(cmap_name), "), order=[2, 1] )"
        else
            print *, "ERROR: ", filename, " COLORMAP FILE NOT FOUND!"
        end if
    end do

    write(out1, '(A)') "end module scientific_colour_maps"
    close(out1)
    close(out2)

    print *, "You can now copy the module file by typing:"
    print *, "cp scientific_colour_maps.f90 ../src"
    print *, "and copy/paste the cases in ../src/colormap_class.f90"
end program generate_scientific_colour_maps
