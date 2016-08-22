!< FURY exposition of units base reference.
module fury_units_reference
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY exposition of units base reference.
!-----------------------------------------------------------------------------------------------------------------------------------
! base units
use fury_unit_current
use fury_unit_length
use fury_unit_luminosity
use fury_unit_mass
use fury_unit_substance
use fury_unit_temperature
use fury_unit_time
! derived units
use fury_unit_angle
use fury_unit_area
use fury_unit_electric_capacitance
use fury_unit_electric_charge
use fury_unit_electric_conductance
use fury_unit_electric_resistance
use fury_unit_energy
use fury_unit_force
use fury_unit_frequency
use fury_unit_illuminance
use fury_unit_inductance
use fury_unit_luminous_flux
use fury_unit_magnetic_flux_density
use fury_unit_magnetic_flux
use fury_unit_power
use fury_unit_pressure
use fury_unit_solid_angle
use fury_unit_speed
use fury_unit_voltage
! unknown unit
use fury_unit_unknown
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! base units
public :: unit_current
public :: unit_length
public :: unit_luminosity
public :: unit_mass
public :: unit_substance
public :: unit_temperature
public :: unit_time
! derived units
public :: unit_angle
public :: unit_area
public :: unit_electric_capacitance
public :: unit_electric_charge
public :: unit_electric_conductance
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
! unknown unit
public :: unit_unknown
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury_units_reference
