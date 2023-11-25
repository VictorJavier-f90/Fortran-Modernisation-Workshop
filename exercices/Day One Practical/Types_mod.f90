!     Last change:  AP   24 Jul 2017   11:45 pm
module Types_mod

  implicit none

  integer, parameter :: SP = SELECTED_REAL_KIND(6 ,37 ) ! Single Precision > 6  significant digits of precision and
                                                        !                    an exponent range of at least 37
  integer, parameter :: DP = SELECTED_REAL_KIND(13,300) ! Double Precision > 13 significant digits of precision and
                                                        !                    an exponent range of ar least 300
  integer, parameter :: SI = SELECTED_INT_KIND(4)       ! Single Integer > ranging from -10^4 to 10^4
  integer, parameter :: DI = SELECTED_INT_KIND(9)       ! Double Integer > ranging from -10^9 to 10^9

end module Types_mod
