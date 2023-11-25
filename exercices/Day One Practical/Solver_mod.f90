!     Last change:  AP   25 Jul 2017    4:09 pm
module Solver_mod

  use Types_mod
  use RHS_mod

  implicit none

  contains

  subroutine fd1d_heat_explicit( x_num, x, t, dt, cfl, h, h_new )

    implicit none

    integer(kind=SI) :: x_num

    real(kind=DP), intent(in) :: cfl
    real(kind=DP), intent(in) :: dt
    real(kind=DP), intent(in) :: h(:)
    real(kind=DP), intent(out) :: h_new(:)
    integer(kind=SI) :: j
    real(kind=DP) :: t
    real(kind=DP), intent(in) :: x(:)
    real(kind=DP) :: f(x_num)

    do j = 1, x_num
      f(j) = func( j, x )
    end do

    h_new(1) = 0.0_DP

    do j = 2, x_num - 1
      h_new(j) = h(j) + dt * f(j) + cfl * ( h(j-1) - 2.0_DP * h(j) + h(j+1) )
    end do

    ! set the boundary conditions again
    h_new(1) = 90.0_DP
    h_new(x_num) = 70.0_DP
  end subroutine fd1d_heat_explicit

end module Solver_mod
