! This program converts .gpl files in the Scientific Colour Map directory and its subdirectories to .lut files.
! The .lut files are written to the same directory, and copies are made to the current directory.
! It also prints the list of all .lut files.
! Note: This program is intended for use on Linux/Unix systems.
!-------------------------------------------------------------------------------
! Contributed by gha3mi: 2023-11-02
! Last modification: gha3mi 2023-11-02
!-------------------------------------------------------------------------------
! Run with the command:
! gfortran gpl_to_lut.f90 && ./a.out

program gpl_to_lut
   implicit none

   character(*), parameter :: dir='./ScientificColourMaps8' ! path to Scientific Colour Map directory

   call convert_all(dir)
   call print_lut_files(dir)
   call cp_lut_files(dir)

contains

   subroutine convert_gpl_to_lut(file_name)
      character(len=*), intent(in) :: file_name
      integer :: nunit, i, iostat, num_rows
      integer, dimension(:,:), allocatable :: colors
      logical :: file_exists

      inquire(file=file_name//'.gpl', exist=file_exists)
      if (file_exists) then
         open(newunit=nunit,file=trim(file_name)//'.gpl',status='old',action='read')
         read(nunit,*)
         read(nunit,*)
         read(nunit,*)
         read(nunit,*)
         num_rows = 0
         do
            read(nunit, *, iostat=iostat)
            if (iostat /= 0) exit
            num_rows = num_rows + 1
         end do
         allocate(colors(num_rows,3))
         rewind(nunit)
         read(nunit,*)
         read(nunit,*)
         read(nunit,*)
         read(nunit,*)
         do i = 1, num_rows
            read(nunit, *) colors(i,1:3)
         end do
         close(nunit)
         open(newunit=nunit, file=file_name//'.lut', status='replace', action='write')
         print'(a,a,a)', 'Writing '//file_name//'.lut'
         do i = 1, size(colors,1)
            write(nunit, '(*(I3,1x))') colors(i,:)
         end do
         close(nunit)
      else
         error stop 'File '//file_name//'.gpl does not exist'
      end if
   end subroutine convert_gpl_to_lut

   subroutine convert_all(path)
      character(*), intent(in) :: path
      character(1024) :: file_name, gpl_file_list
      integer :: nunit, iostat
      logical :: file_exists

      inquire(file=path, exist=file_exists)
      if (file_exists) then
         gpl_file_list = 'gpl_file_list.txt'
         call execute_command_line("find "//trim(path)//" -type f -name '*.gpl' | sed 's/\.gpl$//' > "//trim(gpl_file_list))
         inquire(file=trim(gpl_file_list), exist=file_exists)
         if (file_exists) then
            open(newunit=nunit, file=trim(gpl_file_list), status='old', action='read')
            do
               read(nunit, '(a)', iostat=iostat) file_name(:)
               call convert_gpl_to_lut(trim(file_name))
               if (iostat /= 0) exit
            end do
            close(nunit, status='delete')
         else
            error stop 'File '//trim(gpl_file_list)//' does not exist'
         end if
      else
         error stop 'Directory '//path//' does not exist. Please provide the correct path to the Scientific Colour Map directory'
      end if
   end subroutine convert_all

   subroutine print_lut_files(path)
      character(*), intent(in) :: path
      print'(a)','list of all .lut files:'
      call execute_command_line("find "//trim(path)//" -type f -name '*.lut'")
      call execute_command_line("find "//trim(path)//&
      " -type f -name '*.lut' -execdir bash -c 'echo $(basename '{}' .lut)' \; | sort")
   end subroutine print_lut_files

   subroutine cp_lut_files(path)
      character(*), intent(in) :: path
      call execute_command_line("find "//trim(path)//" -type f -name '*.lut' -exec cp {} . \;")
   end subroutine cp_lut_files
end program gpl_to_lut
