! The MIT License (MIT)
!
! Copyright (c) 2024 vmagnin, gha3mi
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
! Contributed by vmagnin & gha3mi: 2024-02-21
! Last modification: vmagnin 2026-01-07
!-------------------------------------------------------------------------------

!> This module contains miscellaneous procedures and functions.
module forcolormap_utils
    use forcolormap_parameters, only: wp

   implicit none

   private

   public :: bezier, lagrange, lagrange_poly, factorial, scale

   interface scale
       module procedure scale_real_real
       module procedure scale_real_int
   end interface scale

contains

    !> Create a colormap from continuous Bezier interpolation of control colors
    pure function bezier(colors, levels) result(map)
        integer, intent(in) :: colors(:,:)
        integer, intent(in), optional :: levels
        integer, allocatable :: map(:,:)
        integer :: order, i, j, levels_, fact_order
        real(wp) :: r, g, b, coeff, t, omt

        ! Set default value for levels
        if (present(levels)) then
            levels_ = levels
        else
            levels_ = 256
        end if

        ! Order of the Bezier curve
        order = size(colors, 1) - 1
        if (order < 1) error stop "Error: At least two control colors are required for Bezier interpolation."

        allocate(map(levels_, 3))
        fact_order = factorial(order)
        do i = 1, levels_
            t = real(i-1, wp) / real(levels_-1, wp)
            omt = 1.0_wp - t
            r = 0.0_wp
            g = 0.0_wp
            b = 0.0_wp
            do j = 0, order
                coeff = real(fact_order, wp)/real(factorial(j)*factorial(order-j), wp)*t**j*omt**(order-j)
                r = r + real(colors(j+1,1), wp) * coeff
                g = g + real(colors(j+1,2), wp) * coeff
                b = b + real(colors(j+1,3), wp) * coeff
            end do
            map(i,1) = min(255, max(0, nint(r)))
            map(i,2) = min(255, max(0, nint(g)))
            map(i,3) = min(255, max(0, nint(b)))
        end do
    end function bezier

    !> Factorial function used for Bezier interpolation
    pure function factorial(n) result(result)
        integer, intent(in) :: n
        integer :: result, i
        result = 1
        do concurrent (i = 2:n) reduce(*:result)
            result = result * i
        end do
    end function factorial

    !> Create colormap from Lagrange interpolation of control colors
    pure function lagrange(colors, levels) result(map)
        integer, intent(in) :: colors(:,:)
        integer, intent(in), optional :: levels
        integer, allocatable :: map(:,:)
        integer :: order, i, j, n, levels_
        real(wp) :: t, r, g, b
        real(wp), allocatable :: L(:)

        ! Set default value for levels
        if (present(levels)) then
            levels_ = levels
        else
            levels_ = 256
        end if

        ! Order of the Lagrange interpolation.
        order = size(colors, 1) - 1
        if (order < 1) error stop "Error: At least two control colors are required for Lagrange interpolation."

        allocate(map(levels_, 3))
        n = order + 1
        do i = 1, levels_
            t = real(i-1, wp) / real(levels_-1, wp)
            L = lagrange_poly(t, n)
            r = 0.0_wp
            g = 0.0_wp
            b = 0.0_wp
            do j = 1, n
                r = r + L(j) * real(colors(j,1), wp)
                g = g + L(j) * real(colors(j,2), wp)
                b = b + L(j) * real(colors(j,3), wp)
            end do
            map(i,1) = min(255, max(0, nint(r)))
            map(i,2) = min(255, max(0, nint(g)))
            map(i,3) = min(255, max(0, nint(b)))
        end do
    end function lagrange

    !> Interpolates a Lagrange polynomial defined by n equidistant points between 0 and 1
    pure function lagrange_poly(t, n) result(B)
        real(wp), intent(in) :: t
        integer, intent(in) :: n
        real(wp) :: B(n)
        integer :: i, l
        real(wp) :: inv, xi, xl

        if (n <= 1) error stop "Error: Number of points n must be greater than 1 in lagrange_poly."
        if (t < 0.0_wp .or. t > 1.0_wp) error stop "Error: t must be in [0,1] in lagrange_poly."
        B = 1.0_wp
        inv = 1.0_wp/real(n-1, wp)
        do i = 1, n
            xi = real(i-1, wp) * inv
            do l = 1, n
                if (l /= i) then
                    xl = real(l-1, wp) * inv
                    if (abs(xi-xl) >= tiny(0.0_wp)) then
                        B(i) = B(i)*(t-xl)/(xi-xl)
                    end if
                end if
            end do
        end do
    end function lagrange_poly

    !> Normalize the input real array to the range [0, 1]
    pure function scale_real_real(real_array,a,b) result(real_scaled_array)
        real(wp), dimension(:), intent(in) :: real_array
        real(wp), intent(in) :: a, b
        real(wp), dimension(size(real_array)) :: real_scaled_array
        real(wp) :: minValue, maxValue
        real(wp), parameter :: tolerance = 1.0e-12_wp

        ! Find minimum and maximum values in the input real array
        minValue = minval(real_array)
        maxValue = maxval(real_array)

        if (abs(maxValue-minValue) < tolerance) then
            real_scaled_array = b
        else
            real_scaled_array = a + (b - a) * (real_array - minValue) / (maxValue - minValue)
        end if
    end function scale_real_real

    !> Scale the input real array to the integer RGB range [a, b]
    pure function scale_real_int(real_array,a,b) result(int_scaled_array)
        real(wp), dimension(:), intent(in) :: real_array
        integer, intent(in) :: a, b
        real(wp), dimension(size(real_array)) :: normalizedArray
        integer, dimension(size(real_array)) :: int_scaled_array

        ! Normalize the real array elements to the range [0, 1]
        normalizedArray = scale_real_real(real_array, 0.0_wp, 1.0_wp)

        ! Scale the real array elements between a and b
        int_scaled_array = a + nint((b - a) * normalizedArray)
    end function scale_real_int

end module forcolormap_utils
