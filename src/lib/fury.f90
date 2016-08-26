!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
! FURY objects
use fury_qreal
use fury_system_si
use fury_uom
use fury_unit_symbol

! PENF objects
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! FURY objects
public :: qreal
public :: system_si
public :: uom
public :: unit_symbol

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
