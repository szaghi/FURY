!< FURY, Fortran Units (environment) for Reliable phYsical math.
module fury
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, Fortran Units (environment) for Reliable phYsical math.
!-----------------------------------------------------------------------------------------------------------------------------------
! FURY objects
! quantities
use fury_qinteger
use fury_qreal
! units reference
use fury_unit_unknown
use fury_units_base_reference
use fury_units_derived_reference
! units SI
use fury_units_base_si
use fury_units_derived_si
use fury_units_system_si

! PENF objects
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! FURY objects
! quantities
public :: qinteger
public :: qreal
! units reference
public :: unit_current
public :: unit_length
public :: unit_luminosity
public :: unit_mass
public :: unit_substance
public :: unit_temperature
public :: unit_time
public :: unit_angle
public :: unit_area
public :: unit_electrical_conductance
public :: unit_electric_capacitance
public :: unit_electric_charge
public :: unit_electric_resistance
public :: unit_energy
public :: unit_force
public :: unit_frequency
public :: unit_illuminance
public :: unit_inductance
public :: unit_luminous_flux
public :: unit_magnetic_flux_density
public :: unit_magnetic_flux
public :: unit_power
public :: unit_pressure
public :: unit_solid_angle
public :: unit_speed
public :: unit_voltage
public :: unit_unknown
! units SI
public :: unit_ampere
public :: unit_candela
public :: unit_kelvin
public :: unit_kilogram
public :: unit_metre
public :: unit_mole
public :: unit_second
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
