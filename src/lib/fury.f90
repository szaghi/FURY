!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_qinteger, only : qinteger
use fury_qreal, only : qreal
use fury_units_si, only : initialize_si=>initialize, metre
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! quantity with unit
public :: qinteger
public :: qreal
! SI units
public :: initialize_si
public :: metre
! kinds
public :: R8P
public :: R4P
public :: R_P
public :: I8P
public :: I4P
public :: I2P
public :: I1P
public :: I_P
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury
