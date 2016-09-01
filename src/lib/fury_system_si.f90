!< FURY definition of *International System of Units*.
module fury_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of *International System of Units*.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_prefixes
use fury_system_abstract
use fury_uom
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: system_si
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(system_abstract) :: system_si
  !< International System of Units.
  contains
    ! public deferred methods
    procedure, pass(self) :: initialize !< Initialize the units system.
endtype system_si
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine initialize(self, acronym)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the units system.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_si), intent(inout)         :: self    !< The units system.
  character(*),     intent(in),  optional :: acronym !< Units system acronym, e.g. "SI" for the International System.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%free
  self%acronym = 'SI' ; if (present(acronym)) self%acronym = acronym
  self%units_number = 40
  allocate(self%units(1:self%units_number))
  ! base units
  self%units(1) = 'A = ampere [current] {ampere}'
  self%units(2) = 'cd = candela [luminosity] {candela}'
  self%units(3) = 'K = kelvin [temperature] {kelvin}'
  self%units(4) = 'kg = kilogram [mass] {kilogram}'
  self%units(5) = 'm = metre = meter [length] {metre}'
  self%units(6) = 'mol = mole [substance] {mole}'
  self%units(7) = 's = sec = second [time] {second}'
  ! units derived
  self%units(8 ) = 's [time].A [current] (C[electric_charge]) {coulomb}'
  self%units(9 ) = 'kg-1 [mass-1].m-2 [length-2].s4 [time4].A2 [current2] (F[electric_capacitance]) {farad}'
  self%units(10) = 'kg [mass].m2 [length2].s-2 [time-2].A-2 [current-2] (H[inductance]) {henry}'
  self%units(11) = 's-1 [time-1] (Hz[frequency]) {hertz}'
  self%units(12) = 'kg [mass].m2 [length2].s-2 [time-2] (J[energy]) {joule}'
  self%units(13) = 'cd [luminous_flux] (lm [luminous_flux]) {lumen}'
  self%units(14) = 'm-2 [length-2].cd [luminosity] (lx[illuminance]) {lux}'
  self%units(15) = 'm [length].s-1 [time-1]{metre.second-1}'
  self%units(16) = 'm2 [length2]{metre2}'
  self%units(17) = 'kg [mass].m [length].s-2 [time-2] (N[force]) {newton}'
  self%units(18) = 'kg [mass].m2 [length2].s-3 [time-3].A-2 [current-2]{ohm}'
  self%units(19) = 'kg [mass].m-1 [length-1].s-2 [time-2] (Pa[pressure]) {pascal}'
  self%units(20) = 'm [length].m-1 [length-1]{radian}'
  self%units(21) = 'kg-1 [mass-1].m-2 [length-2].s3 [time3].A2 [current2] (S[electric_conductance]) {siemens}'
  self%units(22) = 'm2 [length2].m-2 [length-2]{steradian}'
  self%units(23) = 'kg [mass].s-2 [time-2].A-1 [current-1] (T[magnetic_flux_density]) {tesla}'
  self%units(24) = 'kg [mass].m2 [length2].s-3 [time-3].A-1 [current-1] (V[voltage]) {volt}'
  self%units(25) = 'kg [mass].m2 [length2].s-3 [time-3] (W[power]) {watt}'
  self%units(26) = 'kg [mass].m2 [length2].s-2 [time-2].A-1 [current-1] (Wb[magnetic_flux]) {weber}'
  ! information technology
  self%units(27) = 'bit [bit] {bit}'
  self%units(28) = 'byte = 8 * bit = B = octet [bit] {byte}'
  self%units(29) = 'bit [bit].s-1 [time-1](baud = Bd = bps) {baud}'
  ! common time
  self%units(30) = 'minute = 60 * s = min              [time](minute[time]){minute}'
  self%units(31) = 'hour = 3600 * s = 60 * minute = hr [time](hour[time])  {hour}'
  self%units(32) = 'day = 86400 * s = 1440 * minute = 24 * hour [time](day[time]){day}'
  self%units(33) = 'week = 604800 * s = 10080 * minute = 168 * hour = 7 * day [time](week[time]){week}'
  ! USCS units for conversions
  self%units(34) = 'in< = 0.0254 * m = inch> [length] {internaltion_inch}'
  self%units(35) = 'yd< = 0.9144 * m = yard> [length] {internaltion_yard}'
  self%units(36) = 'ft< = 0.3048 * m = foot> [length] {internaltion_foot}'
  self%units(37) = 'mi< = 1609.34 * m = mile> [length] {internaltion_mile}'
  self%units(38) = 'lb< = 0.453592 * kg = pound> [mass] {internaltion_pound}'
  self%units(39) = 'oz< = 0.0283495 * kg = ounce> [mass] {internaltion_ounce}'
  self%units(40) = 'gram[mass].cm[length].s-2[time-2] (dyne=dyn[force]) {dyne}'
  ! decimal prefixes
  self%decimal_prefixes_number = 20
  allocate(self%decimal_prefixes(self%decimal_prefixes_number))
  self%decimal_prefixes(1 ) = prefixes(aliases=['yocto', 'y    '         ], factor=1.e-24_R_P)
  self%decimal_prefixes(2 ) = prefixes(aliases=['zepto', 'z    '         ], factor=1.e-21_R_P)
  self%decimal_prefixes(3 ) = prefixes(aliases=['atto ', 'a    '         ], factor=1.e-18_R_P)
  self%decimal_prefixes(4 ) = prefixes(aliases=['femto', 'f    '         ], factor=1.e-15_R_P)
  self%decimal_prefixes(5 ) = prefixes(aliases=['pico ', 'p    '         ], factor=1.e-12_R_P)
  self%decimal_prefixes(6 ) = prefixes(aliases=['nano ', 'n    '         ], factor=1.e-9_R_P )
  self%decimal_prefixes(7 ) = prefixes(aliases=['micro', 'u    '         ], factor=1.e-6_R_P )
  self%decimal_prefixes(8 ) = prefixes(aliases=['milli', 'm    '         ], factor=1.e-3_R_P )
  self%decimal_prefixes(9 ) = prefixes(aliases=['centi', 'c    '         ], factor=1.e-2_R_P )
  self%decimal_prefixes(10) = prefixes(aliases=['deci ', 'd    '         ], factor=1.e-1_R_P )
  self%decimal_prefixes(11) = prefixes(aliases=['deca ', 'd    ', 'deka '], factor=1.e+1_R_P )
  self%decimal_prefixes(12) = prefixes(aliases=['hecto', 'h    '         ], factor=1.e2_R_P  )
  self%decimal_prefixes(13) = prefixes(aliases=['kilo ', 'k    '         ], factor=1.e3_R_P  )
  self%decimal_prefixes(14) = prefixes(aliases=['mega ', 'M    '         ], factor=1.e6_R_P  )
  self%decimal_prefixes(15) = prefixes(aliases=['giga ', 'G    '         ], factor=1.e9_R_P  )
  self%decimal_prefixes(16) = prefixes(aliases=['tera ', 'T    '         ], factor=1.e12_R_P )
  self%decimal_prefixes(17) = prefixes(aliases=['peta ', 'P    '         ], factor=1.e15_R_P )
  self%decimal_prefixes(18) = prefixes(aliases=['exa  ', 'E    '         ], factor=1.e18_R_P )
  self%decimal_prefixes(19) = prefixes(aliases=['zetta', 'Z    '         ], factor=1.e21_R_P )
  self%decimal_prefixes(20) = prefixes(aliases=['yotta', 'Y    '         ], factor=1.e24_R_P )
  ! binary prefixes
  self%binary_prefixes_number = 8
  allocate(self%binary_prefixes(self%binary_prefixes_number))
  self%binary_prefixes(1) = prefixes(aliases=['kibi', 'Ki  '], factor=2.e10_R_P)
  self%binary_prefixes(2) = prefixes(aliases=['mebi', 'Mi  '], factor=2.e20_R_P)
  self%binary_prefixes(3) = prefixes(aliases=['gibi', 'Gi  '], factor=2.e30_R_P)
  self%binary_prefixes(4) = prefixes(aliases=['tebi', 'Ti  '], factor=2.e40_R_P)
  self%binary_prefixes(5) = prefixes(aliases=['pebi', 'Pi  '], factor=2.e50_R_P)
  self%binary_prefixes(6) = prefixes(aliases=['exbi', 'Ei  '], factor=2.e60_R_P)
  self%binary_prefixes(7) = prefixes(aliases=['zebi', 'Zi  '], factor=2.e70_R_P)
  self%binary_prefixes(8) = prefixes(aliases=['yobi', 'Yi  '], factor=2.e80_R_P)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize
endmodule fury_system_si
