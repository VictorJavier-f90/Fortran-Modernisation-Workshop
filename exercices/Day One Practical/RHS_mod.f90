!     Last change:  AP   25 Jul 2017    4:00 pm
module RHS_mod

  use Types_mod

  implicit none

  contains

  function func( j, x ) result ( d )
    implicit none
      
    integer(kind=SI) :: j
    real(kind=DP) :: d
    real(kind=DP) :: x(:)

    d = 0.0_DP
  end function func

end module RHS_mod
