!< FURY exposition of units base SI.
module fury_units_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY exposition of units base SI.
!-----------------------------------------------------------------------------------------------------------------------------------
! base units
use fury_unit_ampere
use fury_unit_candela
use fury_unit_kelvin
use fury_unit_kilogram
use fury_unit_metre
use fury_unit_mole
use fury_unit_second
! derived units
use fury_unit_coulomb
use fury_unit_farad
use fury_unit_henry
use fury_unit_hertz
use fury_unit_joule
use fury_unit_lumen
use fury_unit_lux
use fury_unit_metre_per_second
use fury_unit_metre_square
use fury_unit_newton
use fury_unit_ohm
use fury_unit_pascal
use fury_unit_radian
use fury_unit_siemens
use fury_unit_steradian
use fury_unit_tesla
use fury_unit_volt
use fury_unit_watt
use fury_unit_weber
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! base units
public :: unit_ampere
public :: unit_candela
public :: unit_kelvin
public :: unit_kilogram
public :: unit_metre
public :: unit_mole
public :: unit_second
! derived units
public :: unit_coulomb
public :: unit_farad
public :: unit_henry
public :: unit_hertz
public :: unit_joule
public :: unit_lumen
public :: unit_lux
public :: unit_metre_per_second
public :: unit_metre_square
public :: unit_newton
public :: unit_ohm
public :: unit_pascal
public :: unit_radian
public :: unit_siemens
public :: unit_steradian
public :: unit_tesla
public :: unit_volt
public :: unit_watt
public :: unit_weber
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury_units_si
