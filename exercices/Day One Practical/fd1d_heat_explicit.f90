!     Last change:  AP   25 Jul 2017    4:43 pm

program fd1d_heat_explicit_prb

      use Types_mod
      use RHS_mod
      use CFL_mod
      use I0_mod
      use Solver_mod

      implicit none

      integer(kind=SI), parameter :: t_num = 201
      integer(kind=SI), parameter :: x_num = 21
      
      real(kind=DP) :: cfl
      real(kind=DP) :: dt
      real(kind=DP), allocatable :: h(:)
      real(kind=DP), allocatable :: h_new(:)
      ! the "matrix" stores all x-values for all t-values
      ! remember Fortran is column major, meaning that rows are contiguous
      real(kind=DP), allocatable :: hmat(:,:)
      integer(kind=SI) :: i
      integer(kind=SI) :: j
      real(kind=DP) :: k

      real(kind=DP), allocatable :: t(:)
      real(kind=DP) :: t_max
      real(kind=DP) :: t_min
      real(kind=DP), allocatable :: x(:)
      real(kind=DP) :: x_max
      real(kind=DP) :: x_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the FD1D_HEAT_EXPLICIT library.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_TEST01:'
      write ( *, '(a)' ) '  Compute an approximate solution to the time-dependent'
      write ( *, '(a)' ) '  one dimensional heat equation:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    dH/dt - K * d2H/dx2 = f(x,t)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Run a simple test case.'

      ! heat coefficient
      k = 0.002_DP

      ! the x-range values
      x_min = 0.0_DP
      x_max = 1.0_DP
      ! x_num is the number of intervals in the x-direction
      allocate( x(1:x_num) )

      call r8vec_linspace( x_num, x_min, x_max, x )

      ! the t-range values. integrate from t_min to t_max
      t_min = 0.0_DP
      t_max = 80.0_DP

      ! t_num is the number of intervals in the t-direction
      allocate( t(1:t_num) )

      dt = ( t_max - t_min ) / dble( t_num - 1 )
      call r8vec_linspace( t_num, t_min, t_max, t )

      ! get the CFL coefficient
      call fd1d_heat_explicit_cfl( k, t_num, t_min, t_max, x_num, x_min, x_max, cfl )

     if ( 0.5_DP <= cfl ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_CFL - Fatal error!'
        write ( *, '(a)' ) '  CFL condition failed.'
        write ( *, '(a)' ) '  0.5 <= K * dT / dX / dX = CFL.'
        stop
      end if

      ! set the initial condition
      allocate( h(1:x_num) )

      do j = 1, x_num
        h(j) = 50.0_DP
      end do

      ! set the bounday condition
      h(1) = 90.0_DP
      h(x_num) = 70.0_DP

      ! initialise the matrix to the initial condition
      allocate( hmat(1:x_num, 1:t_num) )

      do i = 1, x_num
        hmat(i, 1) = h(i)
      end do

      ! the main time integration loop
      allocate ( h_new(1:x_num) )

      do j = 2, t_num
        call fd1d_heat_explicit( x_num, x, t(j-1), dt, cfl, h, h_new )

        do i = 1, x_num
          hmat(i, j) = h_new(i)
          h(i) = h_new(i)
        end do
      end do

      ! write data to files
      call r8mat_write( 'h_test01.txt', x_num, t_num, hmat )
      call r8vec_write( 't_test01.txt', t_num, t )
      call r8vec_write( 'x_test01.txt', x_num, x )

    deallocate( h )
    deallocate( h_new )
    deallocate( hmat )
    deallocate( x )
    deallocate( t )

end program fd1d_heat_explicit_prb

