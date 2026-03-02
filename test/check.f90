! The MIT License (MIT)
!
! Copyright (c) 2023-2024 Vincent Magnin
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
! Contributed by vmagnin: 2023-10-19
! Last modification: vmagnin 2026-01-08
!-------------------------------------------------------------------------------

!> Automatic tests launched by `fpm test`.
program check

   implicit none
   integer :: id, nfail
   id = 0
   nfail = 0

   write(*,'(a)') "Testing ForColormap"
   write(*,'(a)') "----------------------------------------"
   call test_001(id, nfail)
   call test_002(id, nfail)
   call test_003(id, nfail)
   call test_004(id, nfail)
   call test_005(id, nfail)
   call test_006(id, nfail)
   call test_007(id, nfail)
   call test_008(id, nfail)
   call test_009(id, nfail)
   call test_010(id, nfail)
   call test_011(id, nfail)
   call test_012(id, nfail)
   call test_013(id, nfail)
   call test_014(id, nfail)
   call test_015(id, nfail)
   call test_016(id, nfail)
   call test_017(id, nfail)
   call test_018(id, nfail)
   call test_019(id, nfail)
   call test_020(id, nfail)
   call test_021(id, nfail)
   call test_022(id, nfail)
   call test_023(id, nfail)
   call test_024(id, nfail)
   call test_025(id, nfail)
   call test_026(id, nfail)
   call test_027(id, nfail)
   call test_028(id, nfail)
   call test_029(id, nfail)
   call test_030(id, nfail)
   call test_031(id, nfail)
   call test_032(id, nfail)
   call test_033(id, nfail)
   call test_034(id, nfail)
   call test_035(id, nfail)
   call test_036(id, nfail)
   call test_037(id, nfail)
   call test_038(id, nfail)
   call test_039(id, nfail)
   call test_040(id, nfail)
   call test_041(id, nfail)
   call test_042(id, nfail)
   call test_043(id, nfail)
   call test_044(id, nfail)
   call test_045(id, nfail)
   call test_046(id, nfail)
   call test_047(id, nfail)
   call test_048(id, nfail)
   call test_049(id, nfail)
   call test_050(id, nfail)
   call test_051(id, nfail)
   call test_052(id, nfail)
   call test_053(id, nfail)
   call test_054(id, nfail)
   call test_055(id, nfail)
   call test_056(id, nfail)
   call test_057(id, nfail)
   call test_058(id, nfail)
   call test_059(id, nfail)
   call test_060(id, nfail)
   call test_061(id, nfail)
   call test_062(id, nfail)
   call test_063(id, nfail)
   call test_064(id, nfail)
   call test_065(id, nfail)
   call test_066(id, nfail)
   call test_067(id, nfail)
   call test_068(id, nfail)
   call test_069(id, nfail)
   call test_070(id, nfail)
   call test_071(id, nfail)
   call test_072(id, nfail)
   call test_073(id, nfail)
   call test_074(id, nfail)
   call test_075(id, nfail)
   call test_076(id, nfail)
   call test_077(id, nfail)
   call test_078(id, nfail)
   call test_079(id, nfail)
   call test_080(id, nfail)
   call test_081(id, nfail)
   call test_082(id, nfail)
   call test_083(id, nfail)
   call test_084(id, nfail)
   call test_085(id, nfail)
   call test_086(id, nfail)
   call test_087(id, nfail)
   call test_088(id, nfail)
   call test_089(id, nfail)
   write(*,'(a)') "----------------------------------------"
   write(*,'(a,1x,i6)') "Total tests :", id
   write(*,'(a,1x,i6)') "Passed      :", id - nfail
   write(*,'(a,1x,i6)') "Failed      :", nfail
   write(*,'(a)') "----------------------------------------"

   if (nfail > 0) then
      error stop 1
   end if

contains

   !> Print a single test result.
   !! Increments the global test counter `id`, prints a PASS/FAIL line,
   !! and increments `nfail` when the test fails.
   subroutine report_test(name, ok, id, nfail)
      character(len=*), intent(in)    :: name
      logical,          intent(in)    :: ok
      integer,          intent(inout) :: id, nfail

      id = id + 1
      if (ok) then
         write(*,'("[",i3.3,"] PASSED  ",a)') id, name
      else
         write(*,'("[",i3.3,"] FAILED  ",a)') id, name
         nfail = nfail + 1
      end if
   end subroutine report_test

   !> Delete a file if it exists (best-effort cleanup for tests).
   subroutine delete_if_exists(fname)
      character(len=*), intent(in) :: fname
      logical :: ex
      integer :: u
      inquire(file=fname, exist=ex)
      if (ex) then
         open(newunit=u, file=fname, status="old", action="read")
         close(u, status="delete")
      end if
   end subroutine delete_if_exists

   !> Return `.true.` if `fname` exists on disk.
   logical function exists_file(fname) result(ex)
      character(len=*), intent(in) :: fname
      inquire(file=fname, exist=ex)
   end function exists_file

   !> Simple 1D scalar field for image-writing smoke tests: z(x)=x.
   pure function zfun_1d(x) result(z)
      use forcolormap, only: wp
      real(wp), intent(in) :: x
      real(wp) :: z
      z = x
   end function zfun_1d

   !> Simple 2D scalar field for image-writing smoke tests: z(x,y)=0.5x+0.5y.
   pure function zfun_2d(x, y) result(z)
      use forcolormap, only: wp
      real(wp), intent(in) :: x, y
      real(wp) :: z
      z = 0.5_wp*x + 0.5_wp*y
   end function zfun_2d

   !> Validate the published constant `pi` against `acos(-1)` to working precision.
   subroutine test_001(id, nfail)
      use forcolormap_parameters, only: wp, pi
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "pi ~= acos(-1)"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-15_wp
      real(wp) :: p

      p  = acos(-1.0_wp)
      ok = abs(pi - p) <= tol
      call report_test(name, ok, id, nfail)
   end subroutine test_001

   !> Sanity-check `pi` is in a reasonable floating-point interval (3,4).
   subroutine test_002(id, nfail)
      use forcolormap_parameters, only: wp, pi
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "pi in (3,4)"
      logical :: ok

      ok = (pi > 3.0_wp) .and. (pi < 4.0_wp)
      call report_test(name, ok, id, nfail)
   end subroutine test_002

   !> Verify factorial base case: 0! = 1.
   subroutine test_003(id, nfail)
      use forcolormap_utils, only: factorial
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "factorial: 0! = 1"
      logical :: ok

      ok = (factorial(0) == 1)
      call report_test(name, ok, id, nfail)
   end subroutine test_003

   !> Verify factorial base case: 1! = 1.
   subroutine test_004(id, nfail)
      use forcolormap_utils, only: factorial
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "factorial: 1! = 1"
      logical :: ok

      ok = (factorial(1) == 1)
      call report_test(name, ok, id, nfail)
   end subroutine test_004

   !> Check a known reference value: 5! = 120.
   subroutine test_005(id, nfail)
      use forcolormap_utils, only: factorial
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "factorial: 5! = 120"
      logical :: ok

      ok = (factorial(5) == 120)
      call report_test(name, ok, id, nfail)
   end subroutine test_005

   !> Check a known reference value: 10! = 3628800.
   subroutine test_006(id, nfail)
      use forcolormap_utils, only: factorial
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "factorial: 10! = 3628800"
      logical :: ok

      ok = (factorial(10) == 3628800)
      call report_test(name, ok, id, nfail)
   end subroutine test_006

   !> Confirm the recurrence relation n! = n·(n-1)! for a representative n.
   subroutine test_007(id, nfail)
      use forcolormap_utils, only: factorial
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "factorial: recurrence n! = n*(n-1)!"
      logical :: ok
      integer :: n

      n = 7
      ok = (factorial(n) == n * factorial(n-1))
      call report_test(name, ok, id, nfail)
   end subroutine test_007

   !> Ensure factorial is nondecreasing for small n (0..8).
   subroutine test_008(id, nfail)
      use forcolormap_utils, only: factorial
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "factorial: monotone for n=0..8"
      logical :: ok
      integer :: n, prev, cur

      ok = .true.
      prev = factorial(0)
      do n = 1, 8
         cur = factorial(n)
         ok = ok .and. (cur >= prev)
         prev = cur
      end do
      call report_test(name, ok, id, nfail)
   end subroutine test_008

   !> Lagrange basis should form a partition of unity: sum_i B_i(t) = 1.
   subroutine test_009(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: lagrange_poly
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange_poly: partition of unity at t=0.37 (n=6)"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp

      ok = abs(sum(lagrange_poly(0.37_wp, 6)) - 1.0_wp) <= tol
      call report_test(name, ok, id, nfail)
   end subroutine test_009

   !> At each node t_i, the Lagrange basis should satisfy B_j(t_i)=δ_ij.
   subroutine test_010(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: lagrange_poly
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange_poly: nodal Kronecker delta property (n=7)"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      integer :: n, i, j
      real(wp) :: inv, t
      real(wp), allocatable :: B(:)

      n = 7
      allocate(B(n))
      inv = 1.0_wp / real(n-1, wp)

      ok = .true.
      do i = 1, n
         t = real(i-1, wp) * inv
         B = lagrange_poly(t, n)
         do j = 1, n
            if (j == i) then
               ok = ok .and. (abs(B(j) - 1.0_wp) <= tol)
            else
               ok = ok .and. (abs(B(j)) <= tol)
            end if
         end do
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_010

   !> Endpoints t=0 and t=1 must select the first and last node, respectively.
   subroutine test_011(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: lagrange_poly
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange_poly: endpoints t=0 and t=1 (n=5)"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: B0(5), B1(5)

      B0 = lagrange_poly(0.0_wp, 5)
      B1 = lagrange_poly(1.0_wp, 5)

      ok = (abs(B0(1) - 1.0_wp) <= tol) .and. all(abs(B0(2:)) <= tol) .and. &
         (abs(B1(5) - 1.0_wp) <= tol) .and. all(abs(B1(:4)) <= tol)

      call report_test(name, ok, id, nfail)
   end subroutine test_011

   !> Symmetry check: for n=3, t=0.5 should select the middle basis function.
   subroutine test_012(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: lagrange_poly
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange_poly: t=0.5 gives middle node for n=3"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: B(3)

      B = lagrange_poly(0.5_wp, 3)
      ok = abs(B(2) - 1.0_wp) <= tol .and. abs(B(1)) <= tol .and. abs(B(3)) <= tol
      call report_test(name, ok, id, nfail)
   end subroutine test_012

   !> For n=2, Lagrange interpolation reduces to linear weights [1-t, t] on [0,1].
   subroutine test_013(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: lagrange_poly
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange_poly: n=2 is linear weights in [0,1]"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: t, B(2)

      t = 0.23_wp
      B = lagrange_poly(t, 2)

      ok = abs(B(1) - (1.0_wp - t)) <= tol .and. abs(B(2) - t) <= tol .and. &
         (B(1) >= -tol) .and. (B(2) >= -tol)
      call report_test(name, ok, id, nfail)
   end subroutine test_013

   !> Partition-of-unity should hold across several representative t values.
   subroutine test_014(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: lagrange_poly
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange_poly: partition of unity at multiple t (n=8)"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: ts(5)
      integer :: k

      ts = [ 0.0_wp, 0.1_wp, 0.33_wp, 0.77_wp, 1.0_wp ]

      ok = .true.
      do k = 1, size(ts)
         ok = ok .and. (abs(sum(lagrange_poly(ts(k), 8)) - 1.0_wp) <= tol)
      end do
      call report_test(name, ok, id, nfail)
   end subroutine test_014

   !> Real scaling should map the input minimum to `a` and maximum to `b`.
   subroutine test_015(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,real): min->a, max->b"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: x(3), y(3)
      real(wp) :: a, b

      a = -2.0_wp
      b =  3.0_wp
      x = [ -5.0_wp, 0.0_wp, 5.0_wp ]

      y = scale(x, a, b)
      ok = abs(y(1) - a) <= tol .and. abs(y(3) - b) <= tol
      call report_test(name, ok, id, nfail)
   end subroutine test_015

   !> For constant input, scale() should return the upper bound `b` everywhere.
   subroutine test_016(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,real): constant array -> b"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: x(5), y(5)

      x = 7.0_wp
      y = scale(x, -1.0_wp, 2.0_wp)
      ok = all(abs(y - 2.0_wp) <= tol)
      call report_test(name, ok, id, nfail)
   end subroutine test_016

   !> Monotone input should remain monotone after affine rescaling.
   subroutine test_017(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,real): monotone input stays monotone"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: x(5), y(5)
      integer :: i

      x = [ -2.0_wp, -1.0_wp, 0.0_wp, 1.0_wp, 2.0_wp ]
      y = scale(x, 10.0_wp, 20.0_wp)

      ok = .true.
      do i = 1, 4
         ok = ok .and. (y(i+1) + tol >= y(i))
      end do
      call report_test(name, ok, id, nfail)
   end subroutine test_017

   !> Reversed bounds (a>b) should still map min->a and max->b.
   subroutine test_018(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,real): reversed range a>b maps min->a, max->b"
      logical :: ok
      real(wp), parameter :: tol = 1.0e-12_wp
      real(wp) :: x(3), y(3)
      real(wp) :: a, b

      a = 5.0_wp
      b = -1.0_wp
      x = [ -2.0_wp, 0.0_wp, 2.0_wp ]

      y = scale(x, a, b)
      ok = abs(y(1) - a) <= tol .and. abs(y(3) - b) <= tol
      call report_test(name, ok, id, nfail)
   end subroutine test_018

   !> Integer scaling should map endpoints correctly for the RGB range [0,255].
   subroutine test_019(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,int): endpoints map to [a,b] for [0,255]"
      logical :: ok
      real(wp) :: x(3)
      integer  :: y(3)

      x = [ -2.0_wp, 0.0_wp, 2.0_wp ]
      y = scale(x, 0, 255)

      ok = (y(1) == 0) .and. (y(3) == 255)
      call report_test(name, ok, id, nfail)
   end subroutine test_019

   !> Integer scaling should respect a nontrivial target range [10,20].
   subroutine test_020(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,int): nontrivial integer range [10,20]"
      logical :: ok
      real(wp) :: x(3)
      integer  :: y(3)

      x = [ -1.0_wp, 0.0_wp, 1.0_wp ]
      y = scale(x, 10, 20)

      ok = (y(1) == 10) .and. (y(3) == 20) .and. (y(2) >= 10) .and. (y(2) <= 20)
      call report_test(name, ok, id, nfail)
   end subroutine test_020

   !> Reversed integer bounds [20,10] should still map endpoints and keep values inside the range.
   subroutine test_021(id, nfail)
      use forcolormap_parameters, only: wp
      use forcolormap_utils,      only: scale
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "scale(real,int): reversed integer range [20,10] endpoints"
      logical :: ok
      real(wp) :: x(3)
      integer  :: y(3)

      x = [ -2.0_wp, 0.0_wp, 2.0_wp ]
      y = scale(x, 20, 10)

      ok = (y(1) == 20) .and. (y(3) == 10) .and. (y(2) <= 20) .and. (y(2) >= 10)
      call report_test(name, ok, id, nfail)
   end subroutine test_021

   !> Lagrange colormap generator should default to 256 levels and 3 RGB channels.
   subroutine test_022(id, nfail)
      use forcolormap_utils, only: lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange: default levels=256, shape (256,3)"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: map(:,:)

      colors(1,:) = [ 0, 0, 0 ]
      colors(2,:) = [ 255, 255, 255 ]

      map = lagrange(colors)
      ok = (size(map,1) == 256) .and. (size(map,2) == 3)
      call report_test(name, ok, id, nfail)
   end subroutine test_022

   !> Lagrange colormap generator should honor an explicit `levels` argument.
   subroutine test_023(id, nfail)
      use forcolormap_utils, only: lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange: explicit levels respected"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: map(:,:)
      integer :: levels_

      levels_ = 17
      colors(1,:) = [ 0, 0, 0 ]
      colors(2,:) = [ 255, 255, 255 ]

      map = lagrange(colors, levels_)
      ok = (size(map,1) == levels_) .and. (size(map,2) == 3)
      call report_test(name, ok, id, nfail)
   end subroutine test_023

   !> Lagrange colormap must preserve the first/last control colors as endpoints.
   subroutine test_024(id, nfail)
      use forcolormap_utils, only: lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange: endpoints equal control colors"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: map(:,:)
      integer :: levels_

      levels_ = 11
      colors(1,:) = [ 10, 20, 30 ]
      colors(2,:) = [ 110, 120, 130 ]

      map = lagrange(colors, levels_)
      ok = all(map(1,:) == colors(1,:)) .and. all(map(levels_,:) == colors(2,:))
      call report_test(name, ok, id, nfail)
   end subroutine test_024

   !> Lagrange colormap output must be clamped to valid 8-bit RGB [0,255].
   subroutine test_025(id, nfail)
      use forcolormap_utils, only: lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange: values clamped to [0,255]"
      logical :: ok
      integer :: colors(3,3)
      integer, allocatable :: map(:,:)

      colors(1,:) = [   0,   0,   0 ]
      colors(2,:) = [ 255, 255, 255 ]
      colors(3,:) = [   0, 255,   0 ]

      map = lagrange(colors, 64)
      ok = all(map >= 0) .and. all(map <= 255)
      call report_test(name, ok, id, nfail)
   end subroutine test_025

   !> If all control colors are identical, the generated map should be constant.
   subroutine test_026(id, nfail)
      use forcolormap_utils, only: lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "lagrange: constant control colors -> constant map"
      logical :: ok
      integer :: colors(3,3)
      integer, allocatable :: map(:,:)

      colors(1,:) = [ 7, 8, 9 ]
      colors(2,:) = [ 7, 8, 9 ]
      colors(3,:) = [ 7, 8, 9 ]

      map = lagrange(colors, 32)
      ok = all(map(:,1) == 7) .and. all(map(:,2) == 8) .and. all(map(:,3) == 9)
      call report_test(name, ok, id, nfail)
   end subroutine test_026

   !> Bézier colormap generator should default to 256 levels and 3 RGB channels.
   subroutine test_027(id, nfail)
      use forcolormap_utils, only: bezier
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier: default levels=256, shape (256,3)"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: map(:,:)

      colors(1,:) = [ 0, 0, 0 ]
      colors(2,:) = [ 255, 255, 255 ]

      map = bezier(colors)
      ok = (size(map,1) == 256) .and. (size(map,2) == 3)
      call report_test(name, ok, id, nfail)
   end subroutine test_027

   !> Bézier colormap generator should honor an explicit `levels` argument.
   subroutine test_028(id, nfail)
      use forcolormap_utils, only: bezier
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier: explicit levels respected"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: map(:,:)
      integer :: levels_

      levels_ = 19
      colors(1,:) = [ 0, 0, 0 ]
      colors(2,:) = [ 255, 255, 255 ]

      map = bezier(colors, levels_)
      ok = (size(map,1) == levels_) .and. (size(map,2) == 3)
      call report_test(name, ok, id, nfail)
   end subroutine test_028

   !> Bézier colormap must preserve the first/last control colors as endpoints.
   subroutine test_029(id, nfail)
      use forcolormap_utils, only: bezier
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier: endpoints equal control colors"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: map(:,:)
      integer :: levels_

      levels_ = 9
      colors(1,:) = [  1,  2,  3 ]
      colors(2,:) = [ 11, 12, 13 ]

      map = bezier(colors, levels_)
      ok = all(map(1,:) == colors(1,:)) .and. all(map(levels_,:) == colors(2,:))
      call report_test(name, ok, id, nfail)
   end subroutine test_029

   !> Bézier colormap output must be clamped to valid 8-bit RGB [0,255].
   subroutine test_030(id, nfail)
      use forcolormap_utils, only: bezier
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier: values clamped to [0,255] for 4 control points"
      logical :: ok
      integer :: colors(4,3)
      integer, allocatable :: map(:,:)

      colors(1,:) = [   0,   0,   0 ]
      colors(2,:) = [ 255,   0,   0 ]
      colors(3,:) = [   0, 255,   0 ]
      colors(4,:) = [   0,   0, 255 ]

      map = bezier(colors, 128)
      ok = all(map >= 0) .and. all(map <= 255)
      call report_test(name, ok, id, nfail)
   end subroutine test_030

   !> If all control colors are identical, the generated Bézier map should be constant.
   subroutine test_031(id, nfail)
      use forcolormap_utils, only: bezier
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier: constant control colors -> constant map"
      logical :: ok
      integer :: colors(4,3)
      integer, allocatable :: map(:,:)

      colors(1,:) = [ 7, 8, 9 ]
      colors(2,:) = [ 7, 8, 9 ]
      colors(3,:) = [ 7, 8, 9 ]
      colors(4,:) = [ 7, 8, 9 ]

      map = bezier(colors, 32)
      ok = all(map(:,1) == 7) .and. all(map(:,2) == 8) .and. all(map(:,3) == 9)
      call report_test(name, ok, id, nfail)
   end subroutine test_031

   !> With two control colors (order 1), Bézier and Lagrange interpolation are identical.
   subroutine test_032(id, nfail)
      use forcolormap_utils, only: bezier, lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier == lagrange for 2 colors (order=1)"
      logical :: ok
      integer :: colors(2,3)
      integer, allocatable :: bmap(:,:), lmap(:,:)

      colors(1,:) = [ 0,   128, 255 ]
      colors(2,:) = [ 255,  64,   0 ]

      bmap = bezier(colors, 32)
      lmap = lagrange(colors, 32)
      ok = all(bmap == lmap)
      call report_test(name, ok, id, nfail)
   end subroutine test_032

   !> Both Bézier and Lagrange colormaps must preserve endpoints for 3 control colors.
   subroutine test_033(id, nfail)
      use forcolormap_utils, only: bezier, lagrange
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "bezier and lagrange: endpoints preserved for 3 colors"
      logical :: ok
      integer :: colors(3,3)
      integer, allocatable :: bmap(:,:), lmap(:,:)
      integer :: levels_

      levels_ = 41
      colors(1,:) = [ 5,  10,  15 ]
      colors(2,:) = [ 25, 30,  35 ]
      colors(3,:) = [ 45, 50,  55 ]

      bmap = bezier(colors,  levels_)
      lmap = lagrange(colors, levels_)

      ok = all(bmap(1,:) == colors(1,:)) .and. all(bmap(levels_,:) == colors(3,:)) .and. &
         all(lmap(1,:) == colors(1,:)) .and. all(lmap(levels_,:) == colors(3,:))
      call report_test(name, ok, id, nfail)
   end subroutine test_033

   !> cmap_info should report the expected number of bundled colormaps.
   subroutine test_034(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info: get_ncolormaps == 232"
      logical :: ok

      ok = (cmap_info%get_ncolormaps() == 232)
      call report_test(name, ok, id, nfail)
   end subroutine test_034

   !> get_name(i) should return a non-empty string for representative indices.
   subroutine test_035(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info: get_name non-empty for sample indices"
      logical :: ok
      integer :: n
      character(len=:), allocatable :: s

      n = cmap_info%get_ncolormaps()
      ok = .true.

      s = trim(cmap_info%get_name(1))
      ok = ok .and. (len_trim(s) > 0)

      s = trim(cmap_info%get_name((n+1)/2))
      ok = ok .and. (len_trim(s) > 0)

      s = trim(cmap_info%get_name(n))
      ok = ok .and. (len_trim(s) > 0)

      call report_test(name, ok, id, nfail)
   end subroutine test_035

   !> get_levels(i) should be positive for representative indices.
   subroutine test_036(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info: get_levels positive for sample indices"
      logical :: ok
      integer :: n, lev

      n = cmap_info%get_ncolormaps()
      ok = .true.

      lev = cmap_info%get_levels(1)
      ok = ok .and. (lev > 0)

      lev = cmap_info%get_levels((n+1)/2)
      ok = ok .and. (lev > 0)

      lev = cmap_info%get_levels(n)
      ok = ok .and. (lev > 0)

      call report_test(name, ok, id, nfail)
   end subroutine test_036

   !> get_name(i) should not exceed the published maximum `colormap_name_length`.
   subroutine test_037(id, nfail)
      use forcolormap_info,       only: cmap_info
      use forcolormap_parameters, only: colormap_name_length
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info: get_name length <= colormap_name_length"
      logical :: ok
      integer :: n
      character(len=:), allocatable :: s

      n = cmap_info%get_ncolormaps()
      ok = .true.

      s = trim(cmap_info%get_name(1))
      ok = ok .and. (len(s) <= colormap_name_length)

      s = trim(cmap_info%get_name((n+1)/2))
      ok = ok .and. (len(s) <= colormap_name_length)

      s = trim(cmap_info%get_name(n))
      ok = ok .and. (len(s) <= colormap_name_length)

      call report_test(name, ok, id, nfail)
   end subroutine test_037

   !> set() should select a known built-in map and preserve levels and bounds.
   subroutine test_038(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%set: viridis sets name, levels, zmin/zmax"
      logical :: ok
      type(Colormap) :: cm
      real(wp), parameter :: tol = 1.0e-12_wp

      call cm%set("viridis", 0.0_wp, 1.0_wp)

      ok = (trim(cm%get_name()) == "viridis") .and. &
         (cm%get_levels() == 256) .and. &
         abs(cm%get_zmin() - 0.0_wp) <= tol .and. &
         abs(cm%get_zmax() - 1.0_wp) <= tol

      call report_test(name, ok, id, nfail)
   end subroutine test_038

   !> If zmin>zmax, set() should swap the bounds into ascending order.
   subroutine test_039(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%set: swaps zmin/zmax when zmin>zmax"
      logical :: ok
      type(Colormap) :: cm
      real(wp), parameter :: tol = 1.0e-12_wp

      call cm%set("viridis", 2.0_wp, -3.0_wp)

      ok = (abs(cm%get_zmin() - (-3.0_wp)) <= tol) .and. (abs(cm%get_zmax() - 2.0_wp) <= tol)
      call report_test(name, ok, id, nfail)
   end subroutine test_039

   !> Unknown colormap names should fall back to the default ("grayC").
   subroutine test_040(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%set: invalid name falls back to grayC"
      logical :: ok
      type(Colormap) :: cm

      call cm%set("definitely_not_a_map", 0.0_wp, 1.0_wp)

      ok = (trim(cm%get_name()) == "grayC")
      call report_test(name, ok, id, nfail)
   end subroutine test_040

   !> create() should accept a custom map and allow RGB lookup at the bounds.
   subroutine test_041(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%create: endpoints via compute_RGB"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)
      integer :: r, g, b

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      call cm%create("custom", 0.0_wp, 2.0_wp, map)

      call cm%compute_RGB(0.0_wp, r, g, b)
      ok = (r == 10) .and. (g == 20) .and. (b == 30)

      call cm%compute_RGB(2.0_wp, r, g, b)
      ok = ok .and. (r == 70) .and. (g == 80) .and. (b == 90)

      call report_test(name, ok, id, nfail)
   end subroutine test_041

   !> compute_RGB() should clamp z below/above the bounds to the first/last color.
   subroutine test_042(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%compute_RGB: clamps z outside bounds"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(2,3)
      integer :: r, g, b

      map(1,:) = [  1,  2,  3 ]
      map(2,:) = [ 11, 12, 13 ]

      call cm%create("custom", 0.0_wp, 1.0_wp, map)

      call cm%compute_RGB(-10.0_wp, r, g, b)
      ok = (r == 1) .and. (g == 2) .and. (b == 3)

      call cm%compute_RGB( 10.0_wp, r, g, b)
      ok = ok .and. (r == 11) .and. (g == 12) .and. (b == 13)

      call report_test(name, ok, id, nfail)
   end subroutine test_042

   !> reverse() should flip the map and annotate the name with the "_reverse" suffix.
   subroutine test_043(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%reverse: endpoints swapped and name suffixed"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)
      integer :: r, g, b

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      call cm%create("custom", 0.0_wp, 2.0_wp, map)
      call cm%reverse()

      call cm%get_RGB(0, r, g, b)
      ok = (r == 70) .and. (g == 80) .and. (b == 90)
      ok = ok .and. (index(trim(cm%get_name()), "_reverse") > 0)

      call report_test(name, ok, id, nfail)
   end subroutine test_043

   !> shift(+1) should match the intrinsic circular shift `cshift(map, +1)`.
   subroutine test_044(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%shift: matches cshift for sh=+1"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)
      integer :: ref(3,3)
      integer :: i
      integer :: r, g, b, rr, gg, bb

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      call cm%create("custom", 0.0_wp, 2.0_wp, map)

      ref = cshift(map, 1)
      call cm%shift(1)

      ok = .true.
      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         rr = ref(i+1, 1)
         gg = ref(i+1, 2)
         bb = ref(i+1, 3)
         ok = ok .and. (r == rr) .and. (g == gg) .and. (b == bb)
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_044

   !> extract() should reduce levels while preserving the original endpoint colors.
   subroutine test_045(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%extract: levels reduced and endpoints preserved"
      logical :: ok
      type(Colormap) :: cm
      integer :: r0, g0, b0, r1, g1, b1
      integer :: rr0, gg0, bb0, rr1, gg1, bb1
      integer :: n0

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      n0 = cm%get_levels()

      call cm%get_RGB(0,    r0, g0, b0)
      call cm%get_RGB(n0-1, r1, g1, b1)

      call cm%extract(8)

      call cm%get_RGB(0,                 rr0, gg0, bb0)
      call cm%get_RGB(cm%get_levels()-1, rr1, gg1, bb1)

      ok = (cm%get_levels() == 8) .and. &
         (rr0 == r0) .and. (gg0 == g0) .and. (bb0 == b0) .and. &
         (rr1 == r1) .and. (gg1 == g1) .and. (bb1 == b1)

      call report_test(name, ok, id, nfail)
   end subroutine test_045

   !> create_lagrange() must preserve the first and last control colors as endpoints.
   subroutine test_046(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%create_lagrange: endpoints equal control colors"
      logical :: ok
      type(Colormap) :: cm
      integer :: colors(2,3)
      integer :: r, g, b

      colors(1,:) = [  0, 128, 255 ]
      colors(2,:) = [ 255,  64,   0 ]

      call cm%create_lagrange("lag", 0.0_wp, 1.0_wp, colors, 32)

      call cm%get_RGB(0, r, g, b)
      ok = (r == colors(1,1)) .and. (g == colors(1,2)) .and. (b == colors(1,3))

      call cm%get_RGB(cm%get_levels()-1, r, g, b)
      ok = ok .and. (r == colors(2,1)) .and. (g == colors(2,2)) .and. (b == colors(2,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_046

   !> create_bezier() must preserve the first and last control colors as endpoints.
   subroutine test_047(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%create_bezier: endpoints equal control colors"
      logical :: ok
      type(Colormap) :: cm
      integer :: colors(2,3)
      integer :: r, g, b

      colors(1,:) = [  0, 128, 255 ]
      colors(2,:) = [ 255,  64,   0 ]

      call cm%create_bezier("bez", 0.0_wp, 1.0_wp, colors, 32)

      call cm%get_RGB(0, r, g, b)
      ok = (r == colors(1,1)) .and. (g == colors(1,2)) .and. (b == colors(1,3))

      call cm%get_RGB(cm%get_levels()-1, r, g, b)
      ok = ok .and. (r == colors(2,1)) .and. (g == colors(2,2)) .and. (b == colors(2,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_047

   !> finalize() should be safe and leave the object reusable.
   subroutine test_048(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%finalize: safe to call, object reusable"
      logical :: ok
      type(Colormap) :: cm

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      call cm%finalize()
      call cm%set("magma", -1.0_wp, 2.0_wp)

      ok = (trim(cm%get_name()) == "magma") .and. (cm%get_levels() == 256)
      call report_test(name, ok, id, nfail)
   end subroutine test_048

   !> shift(-1) should match the intrinsic circular shift `cshift(map, -1)`.
   subroutine test_049(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "Colormap%shift: matches cshift for sh=-1"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)
      integer :: ref(3,3)
      integer :: i
      integer :: r, g, b, rr, gg, bb

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      call cm%create("custom", 0.0_wp, 2.0_wp, map)

      ref = cshift(map, -1)
      call cm%shift(-1)

      ok = .true.
      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         rr = ref(i+1, 1)
         gg = ref(i+1, 2)
         bb = ref(i+1, 3)
         ok = ok .and. (r == rr) .and. (g == gg) .and. (b == bb)
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_049

   !> Ensure cmap_info includes known entries and their expected level counts.
   subroutine test_050(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info has acton(256) and acton10(10)"
      logical :: ok
      integer :: i, n, ia, ia10

      n    = cmap_info%get_ncolormaps()
      ia   = 0
      ia10 = 0

      do i = 1, n
         if (trim(cmap_info%get_name(i)) == "acton")   ia   = i
         if (trim(cmap_info%get_name(i)) == "acton10") ia10 = i
      end do

      ok = (n == 232) .and. (ia > 0) .and. (ia10 > 0) .and. &
         (cmap_info%get_levels(ia) == 256) .and. &
         (cmap_info%get_levels(ia10) == 10)

      call report_test(name, ok, id, nfail)
   end subroutine test_050

   !> Create a small discrete colormap and validate basic getters.
   subroutine test_051(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create(discrete) getters ok"
      logical :: ok
      type(Colormap) :: cmap
      integer, dimension(0:6, 3) :: test_colormap
      real(wp), parameter :: tol = 1.0e-12_wp

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cmap%create("discrete", 0.0_wp, 2.0_wp, test_colormap)
      call cmap%print_status()

      ok = (cmap%get_levels() == 7) .and. &
         (abs(cmap%get_zmin() - 0.0_wp) <= tol) .and. &
         (abs(cmap%get_zmax() - 2.0_wp) <= tol) .and. &
         (trim(cmap%get_name()) == "discrete")

      call report_test(name, ok, id, nfail)
   end subroutine test_051

   !> get_RGB(i) must match the stored discrete table at every level.
   subroutine test_052(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "get_RGB matches all discrete levels"
      logical :: ok
      type(Colormap) :: cmap
      integer :: red, green, blue
      integer, dimension(0:6, 3) :: test_colormap
      integer :: i

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cmap%create("discrete", 0.0_wp, 2.0_wp, test_colormap)

      ok = .true.
      do i = 0, 6
         call cmap%get_RGB(i, red, green, blue)
         ok = ok .and. (red   == test_colormap(i,1)) .and. &
            (green == test_colormap(i,2)) .and. &
            (blue  == test_colormap(i,3))
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_052

   !> compute_RGB() should pick the expected discrete index for representative z values.
   subroutine test_053(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "compute_RGB on discrete z=0,1.1,2 hits expected"
      logical :: ok
      type(Colormap) :: cmap
      integer :: red, green, blue
      integer, dimension(0:6, 3) :: test_colormap

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cmap%create("discrete", 0.0_wp, 2.0_wp, test_colormap)

      call cmap%compute_RGB(0.0_wp, red, green, blue)
      ok = (red == test_colormap(0,1)) .and. (green == test_colormap(0,2)) .and. (blue == test_colormap(0,3))

      call cmap%compute_RGB(1.1_wp, red, green, blue)
      ok = ok .and. (red == test_colormap(3,1)) .and. (green == test_colormap(3,2)) .and. (blue == test_colormap(3,3))

      call cmap%compute_RGB(2.0_wp, red, green, blue)
      ok = ok .and. (red == test_colormap(6,1)) .and. (green == test_colormap(6,2)) .and. (blue == test_colormap(6,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_053

   !> A sequence of shifts should match an equivalent sequence of `cshift()` calls.
   subroutine test_054(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "shift(+2,-1,-1) matches cshift reference"
      logical :: ok
      type(Colormap) :: cmap
      integer, dimension(0:6, 3) :: test_colormap, ref
      integer :: i, r, g, b

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cmap%create("discrete", 0.0_wp, 2.0_wp, test_colormap)

      ref = cshift(test_colormap, 2)
      call cmap%shift(2)
      ok = .true.
      do i = 0, 6
         call cmap%get_RGB(i, r, g, b)
         ok = ok .and. (r == ref(i,1)) .and. (g == ref(i,2)) .and. (b == ref(i,3))
      end do

      ref = cshift(ref, -1)
      call cmap%shift(-1)
      do i = 0, 6
         call cmap%get_RGB(i, r, g, b)
         ok = ok .and. (r == ref(i,1)) .and. (g == ref(i,2)) .and. (b == ref(i,3))
      end do

      ref = cshift(ref, -1)
      call cmap%shift(-1)
      do i = 0, 6
         call cmap%get_RGB(i, r, g, b)
         ok = ok .and. (r == ref(i,1)) .and. (g == ref(i,2)) .and. (b == ref(i,3))
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_054

   !> reverse("discrete") should make the new first color equal the old last color.
   subroutine test_055(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "reverse(discrete) makes first=old last"
      logical :: ok
      type(Colormap) :: cmap
      integer, dimension(0:6, 3) :: test_colormap
      integer :: r, g, b

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cmap%create("discrete", 0.0_wp, 2.0_wp, test_colormap)
      call cmap%reverse("discrete")

      call cmap%get_RGB(0, r, g, b)
      ok = (r == test_colormap(6,1)) .and. (g == test_colormap(6,2)) .and. (b == test_colormap(6,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_055

   !> set(acton) followed by extract(10) should yield exactly 10 levels.
   subroutine test_056(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(acton) then extract(10) gives 10 levels"
      logical :: ok
      type(Colormap) :: cmap

      call cmap%set("acton", 0.0_wp, 2.0_wp)
      call cmap%print_status()
      call cmap%extract(10)

      ok = (cmap%get_levels() == 10)
      call report_test(name, ok, id, nfail)
   end subroutine test_056

   !> An invalid colormap name should be corrected to the default ("grayC").
   subroutine test_057(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(invalid name) -> grayC"
      logical :: ok
      type(Colormap) :: cmap

      call cmap%set("actom10", 0.0_wp, 2.0_wp)
      call cmap%print_status()

      ok = (trim(cmap%get_name()) == "grayC")
      call report_test(name, ok, id, nfail)
   end subroutine test_057

   !> set() should swap bounds when called with zmin>zmax.
   subroutine test_058(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(zmin>zmax) swaps bounds"
      logical :: ok
      type(Colormap) :: cmap
      real(wp), parameter :: tol = 1.0e-12_wp

      call cmap%set("acton10", 2.0_wp, 0.0_wp)
      call cmap%print_status()

      ok = (abs(cmap%get_zmin() - 0.0_wp) <= tol) .and. (abs(cmap%get_zmax() - 2.0_wp) <= tol)
      call report_test(name, ok, id, nfail)
   end subroutine test_058

   !> Built-ins with fixed level counts should override an incompatible user request.
   subroutine test_059(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(acton10, levels=256) corrected to 10"
      logical :: ok
      type(Colormap) :: cmap

      call cmap%set("acton10", 0.0_wp, 2.0_wp, 256)
      call cmap%print_status()

      ok = (cmap%get_levels() == 10)
      call report_test(name, ok, id, nfail)
   end subroutine test_059

   !> create() should swap bounds when called with zmin>zmax, even after finalize().
   subroutine test_060(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create(discrete, zmin>zmax) swaps bounds"
      logical :: ok
      type(Colormap) :: cmap
      integer, dimension(0:6, 3) :: test_colormap
      real(wp), parameter :: tol = 1.0e-12_wp

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cmap%finalize()
      call cmap%create("discrete", 2.0_wp, 0.0_wp, test_colormap)
      call cmap%print_status()

      ok = (abs(cmap%get_zmin() - 0.0_wp) <= tol) .and. (abs(cmap%get_zmax() - 2.0_wp) <= tol)
      call report_test(name, ok, id, nfail)
   end subroutine test_060

   !> extract() should ignore invalid requests (here: requesting more levels than available).
   subroutine test_061(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "extract(1000) invalid -> levels unchanged"
      logical :: ok
      type(Colormap) :: cmap
      integer :: n0

      call cmap%set("fes", 0.0_wp, 2.0_wp)
      n0 = cmap%get_levels()

      call cmap%extract(1000)
      call cmap%print_status()

      ok = (cmap%get_levels() == n0)
      call report_test(name, ok, id, nfail)
   end subroutine test_061

   !> extract() should ignore invalid requests (here: non-positive level count).
   subroutine test_062(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "extract(0) invalid -> levels unchanged"
      logical :: ok
      type(Colormap) :: cmap
      integer :: n0

      call cmap%set("fes", 0.0_wp, 2.0_wp)
      n0 = cmap%get_levels()

      call cmap%extract(0)
      call cmap%print_status()

      ok = (cmap%get_levels() == n0)
      call report_test(name, ok, id, nfail)
   end subroutine test_062

   !> set(reverse=.true.) should reverse the built-in map and swap endpoints.
   subroutine test_063(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(reverse=.true.) reverses viridis (endpoints swap)"
      logical :: ok
      type(Colormap) :: a, b
      integer :: rL, gL, bL
      integer :: rr0, gg0, bb0

      call a%set("viridis", 0.0_wp, 1.0_wp)
      call a%get_RGB(a%get_levels()-1, rL, gL, bL)

      call b%set("viridis", 0.0_wp, 1.0_wp, reverse=.true.)
      call b%get_RGB(0, rr0, gg0, bb0)

      ok = (rr0 == rL) .and. (gg0 == gL) .and. (bb0 == bL) .and. &
         (index(trim(b%get_name()), "_reverse") > 0)

      call report_test(name, ok, id, nfail)
   end subroutine test_063

   !> reverse(name="...") should set the name explicitly (no automatic suffixing).
   subroutine test_064(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "reverse(name=...) sets explicit name"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      call cm%create("custom", 0.0_wp, 1.0_wp, map)
      call cm%reverse("myname")

      ok = (trim(cm%get_name()) == "myname")
      call report_test(name, ok, id, nfail)
   end subroutine test_064

   !> Reversing twice should restore the original map (reverse is an involution).
   subroutine test_065(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "reverse(reverse(cm)) == cm (map restored)"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(5,3)
      integer :: ref(0:4,3)
      integer :: i, r, g, b

      map(1,:) = [  1,  2,  3 ]
      map(2,:) = [  4,  5,  6 ]
      map(3,:) = [  7,  8,  9 ]
      map(4,:) = [ 10, 11, 12 ]
      map(5,:) = [ 13, 14, 15 ]

      call cm%create("custom", 0.0_wp, 1.0_wp, map)

      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         ref(i,1) = r; ref(i,2) = g; ref(i,3) = b
      end do

      call cm%reverse()
      call cm%reverse()

      ok = .true.
      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         ok = ok .and. (r == ref(i,1)) .and. (g == ref(i,2)) .and. (b == ref(i,3))
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_065

   !> shift(0) should leave the map unchanged.
   subroutine test_066(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "shift(0) leaves map unchanged"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(4,3)
      integer :: ref(0:3,3)
      integer :: i, r, g, b

      map(1,:) = [ 10,  0,  0 ]
      map(2,:) = [  0, 10,  0 ]
      map(3,:) = [  0,  0, 10 ]
      map(4,:) = [ 10, 10, 10 ]

      call cm%create("custom", 0.0_wp, 1.0_wp, map)

      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         ref(i,1) = r; ref(i,2) = g; ref(i,3) = b
      end do

      call cm%shift(0)

      ok = .true.
      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         ok = ok .and. (r == ref(i,1)) .and. (g == ref(i,2)) .and. (b == ref(i,3))
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_066

   !> Large shifts should wrap modulo the number of levels (circular behavior).
   subroutine test_067(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "shift(100) == cshift(map, mod(100,levels)) for discrete map"
      logical :: ok
      type(Colormap) :: cm
      integer, dimension(0:6,3) :: test_colormap, ref
      integer :: sh, shmod
      integer :: i, r, g, b

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cm%create("discrete", 0.0_wp, 2.0_wp, test_colormap)

      sh    = 100
      shmod = modulo(sh, cm%get_levels())
      ref   = cshift(test_colormap, shmod)

      call cm%shift(sh)

      ok = .true.
      do i = 0, cm%get_levels()-1
         call cm%get_RGB(i, r, g, b)
         ok = ok .and. (r == ref(i,1)) .and. (g == ref(i,2)) .and. (b == ref(i,3))
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_067

   !> extract(2) should reduce the map to exactly the original first and last colors.
   subroutine test_068(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "extract(2) keeps original endpoints"
      logical :: ok
      type(Colormap) :: cm
      integer :: r0, g0, b0, r1, g1, b1
      integer :: rr0, gg0, bb0, rr1, gg1, bb1
      integer :: n0

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      n0 = cm%get_levels()

      call cm%get_RGB(0,    r0, g0, b0)
      call cm%get_RGB(n0-1, r1, g1, b1)

      call cm%extract(2)

      call cm%get_RGB(0, rr0, gg0, bb0)
      call cm%get_RGB(1, rr1, gg1, bb1)

      ok = (cm%get_levels() == 2) .and. &
         (rr0 == r0) .and. (gg0 == g0) .and. (bb0 == b0) .and. &
         (rr1 == r1) .and. (gg1 == g1) .and. (bb1 == b1)

      call report_test(name, ok, id, nfail)
   end subroutine test_068

   !> extract(...,name,zmin,zmax,reverse) should update metadata and reverse endpoints as requested.
   subroutine test_069(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "extract(name,zmin,zmax,reverse) updates fields and reverses endpoints"
      logical :: ok
      type(Colormap) :: cm
      integer, dimension(0:6,3) :: test_colormap
      integer :: r, g, b
      character(len=:), allocatable :: s
      real(wp), parameter :: tol = 1.0e-12_wp

      test_colormap = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9, &
         0, 0, 0, &
         9, 8, 7, &
         6, 5, 4, &
         3, 2, 1 ], shape(test_colormap), order=[2,1])

      call cm%create("discrete", 0.0_wp, 2.0_wp, test_colormap)

      ! For levels=7 and extractedLevels=3, the picked indices are 0,3,6.
      ! With reverse=.true., they become 6,3,0.
      call cm%extract(3, name="ex3", zmin=-1.0_wp, zmax=4.0_wp, reverse=.true.)

      s = trim(cm%get_name())
      ok = (index(s, "ex3") == 1) .and. (index(s, "_reverse") > 0) .and. &
         (abs(cm%get_zmin() - (-1.0_wp)) <= tol) .and. &
         (abs(cm%get_zmax() - ( 4.0_wp)) <= tol) .and. &
         (cm%get_levels() == 3)

      call cm%get_RGB(0, r, g, b)
      ok = ok .and. (r == test_colormap(6,1)) .and. (g == test_colormap(6,2)) .and. (b == test_colormap(6,3))

      call cm%get_RGB(2, r, g, b)
      ok = ok .and. (r == test_colormap(0,1)) .and. (g == test_colormap(0,2)) .and. (b == test_colormap(0,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_069

   !> create(...,reverse=.true.) should reverse a custom map via the optional argument.
   subroutine test_070(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create(reverse=.true.) reverses custom map (endpoints swap)"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)
      integer :: r, g, b

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      call cm%create("custom", 0.0_wp, 1.0_wp, map, reverse=.true.)

      call cm%get_RGB(0, r, g, b)
      ok = (r == 70) .and. (g == 80) .and. (b == 90)

      call cm%get_RGB(cm%get_levels()-1, r, g, b)
      ok = ok .and. (r == 10) .and. (g == 20) .and. (b == 30)

      call report_test(name, ok, id, nfail)
   end subroutine test_070

   !> create_lagrange(...,reverse=.true.) should swap the endpoint colors.
   subroutine test_071(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create_lagrange(reverse=.true.) swaps endpoints"
      logical :: ok
      type(Colormap) :: cm
      integer :: colors(2,3)
      integer :: r, g, b

      colors(1,:) = [  1,  2,  3 ]
      colors(2,:) = [ 11, 12, 13 ]

      call cm%create_lagrange("lag", 0.0_wp, 1.0_wp, colors, 16, reverse=.true.)

      call cm%get_RGB(0, r, g, b)
      ok = (r == colors(2,1)) .and. (g == colors(2,2)) .and. (b == colors(2,3))

      call cm%get_RGB(cm%get_levels()-1, r, g, b)
      ok = ok .and. (r == colors(1,1)) .and. (g == colors(1,2)) .and. (b == colors(1,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_071

   !> create_bezier(...,reverse=.true.) should swap the endpoint colors.
   subroutine test_072(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create_bezier(reverse=.true.) swaps endpoints"
      logical :: ok
      type(Colormap) :: cm
      integer :: colors(2,3)
      integer :: r, g, b

      colors(1,:) = [  1,  2,  3 ]
      colors(2,:) = [ 11, 12, 13 ]

      call cm%create_bezier("bez", 0.0_wp, 1.0_wp, colors, 16, reverse=.true.)

      call cm%get_RGB(0, r, g, b)
      ok = (r == colors(2,1)) .and. (g == colors(2,2)) .and. (b == colors(2,3))

      call cm%get_RGB(cm%get_levels()-1, r, g, b)
      ok = ok .and. (r == colors(1,1)) .and. (g == colors(1,2)) .and. (b == colors(1,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_072

   !> load() should read an RGB text file; reverse=.true. should swap the loaded endpoints.
   subroutine test_073(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "load() reads RGB file (and reverse=.true. swaps endpoints)"
      logical :: ok
      type(Colormap) :: a, b
      character(len=*), parameter :: fname = "test_tmp_colormap_load.txt"
      integer :: u
      integer :: r0, g0, b0, rL, gL, bL
      integer :: rr0, gg0, bb0
      logical :: ex

      ! Create a tiny RGB file (3 lines) for load() tests.
      open(newunit=u, file=fname, status="replace", action="write")
      write(u,'(3I3)')  10,  20,  30
      write(u,'(3I3)')  40,  50,  60
      write(u,'(3I3)')  70,  80,  90
      close(u)

      inquire(file=fname, exist=ex)
      if (.not. ex) then
         ok = .false.
         call report_test(name, ok, id, nfail)
         return
      end if

      call a%load(fname, 0.0_wp, 1.0_wp)
      call a%get_RGB(0, r0, g0, b0)
      call a%get_RGB(a%get_levels()-1, rL, gL, bL)

      ok = (a%get_levels() == 3) .and. &
         (trim(a%get_name()) == fname) .and. &
         (r0 == 10) .and. (g0 == 20) .and. (b0 == 30) .and. &
         (rL == 70) .and. (gL == 80) .and. (bL == 90)

      call b%load(fname, 0.0_wp, 1.0_wp, reverse=.true.)
      call b%get_RGB(0, rr0, gg0, bb0)
      ok = ok .and. (rr0 == 70) .and. (gg0 == 80) .and. (bb0 == 90)

      call delete_if_exists(fname)

      call report_test(name, ok, id, nfail)
   end subroutine test_073

   !> Some built-ins (e.g., "rainbow") force a fixed level count regardless of user input.
   subroutine test_074(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(rainbow, levels=17) forces levels=256"
      logical :: ok
      type(Colormap) :: cm

      call cm%set("rainbow", 0.0_wp, 1.0_wp, 17)

      ok = (trim(cm%get_name()) == "rainbow") .and. (cm%get_levels() == 256)
      call report_test(name, ok, id, nfail)
   end subroutine test_074

   !> For a 3-level map and z=0.5, compute_RGB() should select the middle level.
   subroutine test_075(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "compute_RGB picks interior level for 3-level custom map"
      logical :: ok
      type(Colormap) :: cm
      integer :: map(3,3)
      integer :: r, g, b

      map(1,:) = [  1,  2,  3 ]   ! level 0
      map(2,:) = [ 11, 12, 13 ]   ! level 1 (middle)
      map(3,:) = [ 21, 22, 23 ]   ! level 2

      call cm%create("custom", 0.0_wp, 1.0_wp, map)

      call cm%compute_RGB(0.5_wp, r, g, b)
      ok = (r == 11) .and. (g == 12) .and. (b == 13)

      call report_test(name, ok, id, nfail)
   end subroutine test_075

   !> If no explicit name is provided, extract(levels) should annotate the name with the level count.
   subroutine test_076(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "extract(7) default name appends digits"
      logical :: ok
      type(Colormap) :: cm
      character(len=:), allocatable :: s

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      call cm%extract(7)

      s  = trim(cm%get_name())
      ok = (cm%get_levels() == 7) .and. (index(s, "viridis") == 1) .and. (index(s, "7") > 0)

      call report_test(name, ok, id, nfail)
   end subroutine test_076

   !> Smoke test: print() should be callable after a normal set() without crashing.
   subroutine test_077(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "print() callable after set()"
      logical :: ok
      type(Colormap) :: cm

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      call cm%print()

      ok = .true.
      call report_test(name, ok, id, nfail)
   end subroutine test_077

   !> colorbar() should produce an output image file (existence is the main check here).
   subroutine test_078(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "colorbar() writes a PPM file"
      logical :: ok
      type(Colormap) :: cm
      character(len=*), parameter :: fname = "test_tmp_colorbar.ppm"

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      call cm%colorbar(fname, width=16, height=4)

      ok = exists_file(fname) .or. exists_file(trim(fname)//".ppm") .or. exists_file("test_tmp_colorbar")

      call delete_if_exists(fname)
      call delete_if_exists(trim(fname)//".ppm")
      call delete_if_exists("test_tmp_colorbar")

      call report_test(name, ok, id, nfail)
   end subroutine test_078

   !> colormap(1D) should write an output image file (existence check only).
   subroutine test_079(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "colormap(1D) writes a PPM file"
      logical :: ok
      type(Colormap) :: cm
      character(len=*), parameter :: fname = "test_tmp_colormap_1d.ppm"

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      call cm%colormap(fname, zfun_1d, xmin=0.0_wp, xmax=1.0_wp, width=12, height=3)

      ok = exists_file(fname) .or. exists_file(trim(fname)//".ppm") .or. exists_file("test_tmp_colormap_1d")

      call delete_if_exists(fname)
      call delete_if_exists(trim(fname)//".ppm")
      call delete_if_exists("test_tmp_colormap_1d")

      call report_test(name, ok, id, nfail)
   end subroutine test_079

   !> colormap(2D) should write an output image file (exercises the 2D branch).
   subroutine test_080(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "colormap(2D) writes a PPM file"
      logical :: ok
      type(Colormap) :: cm
      character(len=*), parameter :: fname = "test_tmp_colormap_2d.ppm"
      real(wp) :: xmin(2), xmax(2)

      call cm%set("viridis", 0.0_wp, 1.0_wp)
      xmin = [ 0.0_wp, 0.0_wp ]
      xmax = [ 1.0_wp, 1.0_wp ]

      call cm%colormap(fname, zfun_2d, xmin=xmin, xmax=xmax, width=10, height=8)

      ok = exists_file(fname) .or. exists_file(trim(fname)//".ppm") .or. exists_file("test_tmp_colormap_2d")

      call delete_if_exists(fname)
      call delete_if_exists(trim(fname)//".ppm")
      call delete_if_exists("test_tmp_colormap_2d")

      call report_test(name, ok, id, nfail)
   end subroutine test_080

   !> create_lagrange(levels<=0) should trigger validation and fall back to a safe default (256).
   subroutine test_081(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create_lagrange(levels=0) -> levels fixed to 256 (check case 4)"
      logical :: ok
      type(Colormap) :: cm
      integer :: colors(2,3)

      colors(1,:) = [ 0, 0, 0 ]
      colors(2,:) = [ 255, 255, 255 ]

      call cm%create_lagrange("bad_levels", 0.0_wp, 1.0_wp, colors, 0)
      call cm%print_status()

      ok = (cm%get_levels() == 256)
      call report_test(name, ok, id, nfail)
   end subroutine test_081

   !> If cmap_info contains any entry with levels=-1, cover the check_name (levels==-1) branch.
   subroutine test_082(id, nfail)
      use forcolormap, only: Colormap, wp, cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "check_name: cover levels==-1 branch if present in cmap_info"
      logical :: ok
      type(Colormap) :: cm
      integer :: i, n, idx
      character(len=:), allocatable :: nm

      n   = cmap_info%get_ncolormaps()
      idx = 0

      do i = 1, n
         if (cmap_info%get_levels(i) == -1) then
            idx = i
            exit
         end if
      end do

      if (idx == 0) then
         ! This build has no “levels==-1” entries, so that branch cannot be exercised here.
         ok = .true.
         call report_test(name//" (no levels==-1 in cmap_info; skipped)", ok, id, nfail)
         return
      end if

      nm = trim(cmap_info%get_name(idx))

      ! Request a mismatched levels value; for levels==-1, check_name should not override it.
      call cm%set(nm, 0.0_wp, 1.0_wp, 17)
      call cm%print_status()

      ok = (trim(cm%get_name()) == nm) .and. (cm%get_levels() == 17)
      call report_test(name, ok, id, nfail)
   end subroutine test_082

   !> set(reverse=.false.) must not reverse and must not append the "_reverse" suffix.
   subroutine test_083(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set(reverse=.false.) leaves viridis unchanged (no suffix, endpoints same)"
      logical :: ok
      type(Colormap) :: a, b
      integer :: r0a, g0a, b0a, rLa, gLa, bLa
      integer :: r0b, g0b, b0b, rLb, gLb, bLb
      integer :: n

      call a%set("viridis", 0.0_wp, 1.0_wp)
      n = a%get_levels()

      call a%get_RGB(0,   r0a, g0a, b0a)
      call a%get_RGB(n-1, rLa, gLa, bLa)

      call b%set("viridis", 0.0_wp, 1.0_wp, reverse=.false.)

      call b%get_RGB(0,   r0b, g0b, b0b)
      call b%get_RGB(n-1, rLb, gLb, bLb)

      ok = (trim(b%get_name()) == "viridis") .and. (index(trim(b%get_name()), "_reverse") == 0) .and. &
         (r0b == r0a) .and. (g0b == g0a) .and. (b0b == b0a) .and. &
         (rLb == rLa) .and. (gLb == gLa) .and. (bLb == bLa)

      call report_test(name, ok, id, nfail)
   end subroutine test_083

   !> create*/(reverse=.false.) must preserve endpoints and must not add the "_reverse" suffix.
   subroutine test_084(id, nfail)
      use forcolormap, only: Colormap, wp
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "create*/(reverse=.false.) preserves endpoints (no suffix)"
      logical :: ok
      type(Colormap) :: c1, c2, c3
      integer :: map(3,3)
      integer :: colors(2,3)
      integer :: r, g, b

      map(1,:) = [ 10, 20, 30 ]
      map(2,:) = [ 40, 50, 60 ]
      map(3,:) = [ 70, 80, 90 ]

      colors(1,:) = [  1,  2,  3 ]
      colors(2,:) = [ 11, 12, 13 ]

      ! create(reverse=.false.)
      call c1%create("custom", 0.0_wp, 1.0_wp, map, reverse=.false.)
      call c1%get_RGB(0, r, g, b)
      ok = (r == 10) .and. (g == 20) .and. (b == 30) .and. (index(trim(c1%get_name()), "_reverse") == 0)

      call c1%get_RGB(c1%get_levels()-1, r, g, b)
      ok = ok .and. (r == 70) .and. (g == 80) .and. (b == 90)

      ! create_lagrange(reverse=.false.)
      call c2%create_lagrange("lag", 0.0_wp, 1.0_wp, colors, 16, reverse=.false.)
      call c2%get_RGB(0, r, g, b)
      ok = ok .and. (r == colors(1,1)) .and. (g == colors(1,2)) .and. (b == colors(1,3)) .and. &
         (index(trim(c2%get_name()), "_reverse") == 0)

      call c2%get_RGB(c2%get_levels()-1, r, g, b)
      ok = ok .and. (r == colors(2,1)) .and. (g == colors(2,2)) .and. (b == colors(2,3))

      ! create_bezier(reverse=.false.)
      call c3%create_bezier("bez", 0.0_wp, 1.0_wp, colors, 16, reverse=.false.)
      call c3%get_RGB(0, r, g, b)
      ok = ok .and. (r == colors(1,1)) .and. (g == colors(1,2)) .and. (b == colors(1,3)) .and. &
         (index(trim(c3%get_name()), "_reverse") == 0)

      call c3%get_RGB(c3%get_levels()-1, r, g, b)
      ok = ok .and. (r == colors(2,1)) .and. (g == colors(2,2)) .and. (b == colors(2,3))

      call report_test(name, ok, id, nfail)
   end subroutine test_084

   !> Smoke test: set() should succeed for every name reported by cmap_info.
   subroutine test_085(id, nfail)
      use forcolormap,      only: Colormap, wp
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "set() covers all cmap_info names (smoke test)"
      logical :: ok
      type(Colormap) :: cm
      integer :: i, n, lev_expected, lev_got
      character(len=:), allocatable :: nm
      integer :: r, g, b

      n  = cmap_info%get_ncolormaps()
      ok = .true.

      do i = 1, n
         nm = trim(cmap_info%get_name(i))
         lev_expected = cmap_info%get_levels(i)

         call cm%set(nm, 0.0_wp, 1.0_wp)

         ! Minimal correctness: known names should not fall back to the default.
         ok = ok .and. (trim(cm%get_name()) == nm)

         ! Enforce levels when cmap_info provides a concrete value; otherwise just require positivity.
         lev_got = cm%get_levels()
         if (lev_expected > 0) then
            ok = ok .and. (lev_got == lev_expected)
         else
            ok = ok .and. (lev_got > 0)
         end if

         ! Touch the RGB table path at both ends.
         call cm%compute_RGB(0.0_wp, r, g, b)
         call cm%compute_RGB(1.0_wp, r, g, b)
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_085

   !> Ensure cmap_info does not contain duplicate colormap names.
   subroutine test_086(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info: all colormap names are unique (no duplicates)"
      logical :: ok
      integer :: i, j, n
      character(len=:), allocatable :: ni, nj

      n  = cmap_info%get_ncolormaps()
      ok = .true.

      do i = 1, n
         ni = trim(cmap_info%get_name(i))
         do j = i + 1, n
            nj = trim(cmap_info%get_name(j))
            if (ni == nj) then
               ok = .false.
               exit
            end if
         end do
         if (.not. ok) exit
      end do

      call report_test(name, ok, id, nfail)
   end subroutine test_086

   !> write(verbose=3) should write exactly N non-empty name lines (one per colormap).
   subroutine test_087(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info%write(verbose=3): writes N name lines"
      logical :: ok
      character(len=*), parameter :: fname = "test_tmp_cmap_info_write_v3.txt"
      integer :: n, u, ios, count
      character(len=512) :: line

      n = cmap_info%get_ncolormaps()

      call delete_if_exists(fname)
      call cmap_info%write(verbose=3, file_name=fname)

      ok = exists_file(fname)
      if (.not. ok) then
         call report_test(name, ok, id, nfail)
         return
      end if

      count = 0
      open(newunit=u, file=fname, status="old", action="read")
      do
         read(u,'(a)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) > 0) count = count + 1
      end do
      close(u)

      ok = (count == n)

      call delete_if_exists(fname)
      call report_test(name, ok, id, nfail)
   end subroutine test_087

   !> write(verbose=3,name=...) should filter output to exactly one matching name line.
   subroutine test_088(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info%write(name=...): filters to exactly one entry"
      logical :: ok
      character(len=*), parameter :: fname = "test_tmp_cmap_info_write_filter.txt"
      character(len=:), allocatable :: target
      integer :: u, ios, count
      character(len=512) :: line

      target = trim(cmap_info%get_name(1))

      call delete_if_exists(fname)
      call cmap_info%write(verbose=3, name=target, file_name=fname)

      ok = exists_file(fname)
      if (.not. ok) then
         call report_test(name, ok, id, nfail)
         return
      end if

      count = 0
      ok    = .true.
      open(newunit=u, file=fname, status="old", action="read")
      do
         read(u,'(a)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) == 0) cycle
         count = count + 1
         ok = ok .and. (trim(line) == target)
      end do
      close(u)

      ok = ok .and. (count == 1)

      call delete_if_exists(fname)
      call report_test(name, ok, id, nfail)
   end subroutine test_088

   !> write(...,append=.true.) should append to an existing file (doubling line count for verbose=3).
   subroutine test_089(id, nfail)
      use forcolormap_info, only: cmap_info
      integer, intent(inout) :: id, nfail
      character(len=*), parameter :: name = "cmap_info%write(append=.true.): appends to existing file"
      logical :: ok
      character(len=*), parameter :: fname = "test_tmp_cmap_info_write_append.txt"
      integer :: n, u, ios, count
      character(len=512) :: line

      n = cmap_info%get_ncolormaps()

      call delete_if_exists(fname)

      call cmap_info%write(verbose=3, file_name=fname)
      call cmap_info%write(verbose=3, file_name=fname, append=.true.)

      ok = exists_file(fname)
      if (.not. ok) then
         call report_test(name, ok, id, nfail)
         return
      end if

      count = 0
      open(newunit=u, file=fname, status="old", action="read")
      do
         read(u,'(a)', iostat=ios) line
         if (ios /= 0) exit
         if (len_trim(line) > 0) count = count + 1
      end do
      close(u)

      ok = (count == 2*n)

      call delete_if_exists(fname)
      call report_test(name, ok, id, nfail)
   end subroutine test_089

end program check
