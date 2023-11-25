!     Last change:  AP   25 Jul 2017    4:05 pm
module I0_mod

  use Types_mod

  implicit none

  contains

  subroutine r8mat_write( output_filename, m, n, table )
    implicit none

    integer(kind=SI), intent(in) :: m
    integer(kind=SI), intent(in) :: n

    integer(kind=SI) :: j
    character(len=*) :: output_filename
    integer(kind=SI) :: output_unit_id
    character(len=30) :: string
    real(kind=DP), intent(in) :: table(m,n)

    output_unit_id = 10
    open( unit = output_unit_id, file = output_filename, status = 'replace' )

      write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'

      do j = 1, n
        write ( output_unit_id, string ) table(1:m, j)
      end do

    close( unit = output_unit_id )
  end subroutine r8mat_write

  subroutine r8vec_linspace ( n, a_first, a_last, a )

    implicit none

    integer(kind=SI), intent(in) :: n
    real(kind=DP), intent(out) :: a(n)
    real(kind=DP), intent(in) :: a_first
    real(kind=DP), intent(in) :: a_last
    integer(kind=SI) :: i

    do i = 1, n
      a(i) = ( dble( n - i ) * a_first + dble( i - 1 ) * a_last ) / dble( n - 1 )
    end do

  end subroutine r8vec_linspace

  subroutine r8vec_write ( output_filename, n, x )

    implicit none

    integer(kind=SI), intent(in) :: n

    integer(kind=SI) :: j
    character(len=*), intent(in) :: output_filename
    integer(kind=SI) :: output_unit_id
    real(kind=DP), intent(in) :: x(n)

    output_unit_id = 11
    open( unit = output_unit_id, file = output_filename, status = 'replace' )

      do j = 1, n
        write ( output_unit_id, '(2x,g24.16)' ) x(j)
      end do

    close ( unit = output_unit_id )
  end subroutine r8vec_write

end module I0_mod
