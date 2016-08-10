!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
! FURY objects
! quantity with unit
use fury_qinteger, only : qinteger
use fury_qreal, only : qreal
! reference base units
use fury_unit_length, only : unit_length
use fury_unit_time, only : unit_time
use fury_unit_unknown, only : unit_unknown
! reference derived units
use fury_unit_area, only : unit_area
use fury_unit_speed, only : unit_speed
! SI units
use fury_unit_metre, only : unit_metre
use fury_unit_second, only : unit_second
use fury_unit_metre_per_second, only : unit_metre_per_second
use fury_unit_metre_square, only : unit_metre_square
use fury_units_system_si, only : units_system_si

! PENF objects
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! FURY objects
! quantity with unit
public :: qinteger
public :: qreal
! reference base units
public :: unit_length, unit_time, unit_unknown
! reference derived units
public :: unit_area, unit_speed
! units system SI
public :: unit_metre, unit_second, unit_metre_per_second, unit_metre_square
public :: units_system_si

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
