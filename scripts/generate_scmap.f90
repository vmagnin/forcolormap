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
        "acton", "actonS", "bamako", "bamakoS", "bam", &
        "bamO", "batlowK", "batlowKS", "batlow", "batlowS",&
        "batlowW", "batlowWS", "berlin", "bilbao", "bilbaoS",&
        "broc", "brocO", "buda", "budaS", "bukavu",&
        "cork", "corkO", "davos", "davosS", "devon",&
        "devonS", "fes", "glasgow", "glasgowS", "grayC",&
        "grayCS", "hawaii", "hawaiiS", "imola", "imolaS",&
        "lajolla", "lajollaS", "lapaz", "lapazS", "lipari",&
        "lipariS", "lisbon", "managua", "navia", "naviaS", &
        "naviaW", "naviaWS", "nuuk", "nuukS", "oleron",&
        "oslo", "osloS", "roma", "romaO", "tofino", &
        "tokyo", "tokyoS", "turku", "turkuS", "vanimo", &
        "vik", "vikO" ]
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
        write(out2, '(12x, 5A)') 'call self%create("', trim(cmap_name), '", &
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
