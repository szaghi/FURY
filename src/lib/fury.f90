!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_qinteger, only : qinteger
use fury_qreal, only : qreal
use fury_units_si, only : initialize_si=>initialize, &
                          metre, unit_metre,         &
                          second, unit_second
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! FURY objects
! quantity with unit
public :: qinteger
public :: qreal
! SI units
public :: initialize_si
public :: metre, unit_metre
public :: second, unit_second
! PENF objects
! kinds
public :: R8P
public :: R4P
public :: R_P
public :: I8P
public :: I4P
public :: I2P
public :: I1P
public :: I_P
! number casting
public :: str, strz
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury
