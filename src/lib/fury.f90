!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_mixed_kinds
use fury_qreal32, qreal32 => qreal
use fury_qreal64, qreal64 => qreal
use fury_system_abstract32, system_abstract32 => system_abstract
use fury_system_abstract64, system_abstract64 => system_abstract
use fury_system_si32, system_si32 => system_si
use fury_system_si64, system_si64 => system_si
use fury_uom32, uom32 => uom
use fury_uom64, uom64 => uom
use fury_uom_converter32, converter32 => converter
use fury_uom_converter64, converter64 => converter
use fury_uom_reference32, uom_reference32 => uom_reference
use fury_uom_reference64, uom_reference64 => uom_reference
use fury_uom_symbol32, uom_symbol32 => uom_symbol
use fury_uom_symbol64, uom_symbol64 => uom_symbol
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! FURY objects
public :: assignment(=)
public :: operator(*)
public :: qreal32, qreal64
public :: system_si32, system_si64
public :: converter32, converter64
public :: uom32, uom64
public :: uom_reference32, uom_reference64

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
