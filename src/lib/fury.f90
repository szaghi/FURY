!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
! FURY objects
use fury_qreal64, qreal64 => qreal
use fury_system_abstract64, system_abstract64 => system_abstract
use fury_system_si64, system_si64 => system_si
use fury_uom64, uom64 => uom
use fury_uom_converter64, converter64 => converter
use fury_uom_reference64, uom_reference64 => uom_reference

! PENF objects
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! FURY objects
public :: operator(*)
public :: qreal64
public :: system_si64
public :: converter64
public :: uom64
public :: uom_reference64

! PENF objects
! kinds
public :: R16P
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
