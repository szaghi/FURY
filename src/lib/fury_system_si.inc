!< FURY implementation of *International System of Units* with generic kind.

implicit none
private
public :: system_si

type, extends(system_abstract) :: system_si
  !< International System of Units.
  contains
    ! public deferred methods
    procedure, pass(self) :: initialize !< Initialize the units system.
endtype system_si
contains
  subroutine initialize(self, acronym)
  !< Initialize the units system.
  !<
  !< @todo Load from file.
  class(system_si), intent(inout)         :: self      !< The units system.
  character(*),     intent(in),  optional :: acronym   !< Units system acronym, e.g. "SI" for the International System.
  type(qreal)                             :: c         !< Speed of light.
  type(qreal)                             :: epsilon_0 !< Vacuum permittivity.
  type(qreal)                             :: mu_0      !< Vacuum permeability.
  type(qreal)                             :: Z_0       !< Vacuum impedance.

  call self%free
  self%acronym = 'SI' ; if (present(acronym)) self%acronym = acronym
  ! base units
  call self%add_unit('A = ampere [current] {ampere}')
  call self%add_unit('cd = candela [luminosity] {candela}')
  call self%add_unit('K = kelvin [temperature] {kelvin}')
  call self%add_unit('kg = kilogram [mass] {kilogram}')
  call self%add_unit('m = metre = meter [length] {metre}')
  call self%add_unit('mol = mole [substance] {mole}')
  call self%add_unit('s = sec = second [time] {second}')
  ! units derived
  call self%add_unit('celsius< = 273.15 + K> [temperature] (celsius[temperature]) {celsius}')
  call self%add_unit('s [time].A [current] (C[electric_charge]) {coulomb}')
  call self%add_unit('kg-1 [mass-1].m-2 [length-2].s4 [time4].A2 [current2] (F[electric_capacitance]) {farad}')
  call self%add_unit('kg [mass].m2 [length2].s-2 [time-2].A-2 [current-2] (H[inductance]) {henry}')
  call self%add_unit('Hz< = s-1 > [frequency] {hertz}')
  call self%add_unit('kg [mass].m2 [length2].s-2 [time-2] (J[energy]) {joule}')
  call self%add_unit('cd [luminosity].m2 [length2].m-2 [length-2] (lm [luminous_flux]) {lumen}')
  call self%add_unit('m-2 [length-2].cd [luminosity] (lx[illuminance]) {lux}')
  call self%add_unit('m [length].s-1 [time-1]{metre.second-1}')
  call self%add_unit('m [length].s-2 [time-2]{metre.second-2}')
  call self%add_unit('m2 [length2]{metre2}')
  call self%add_unit('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
  call self%add_unit('kg [mass].m2 [length2].s-3 [time-3].A-2 [current-2]{ohm}')
  call self%add_unit('kg [mass].m-1 [length-1].s-2 [time-2] (Pa[pressure]) {pascal}')
  call self%add_unit('m [length].m-1 [length-1]{radian}')
  call self%add_unit('kg-1 [mass-1].m-2 [length-2].s3 [time3].A2 [current2] (S[electric_conductance]) {siemens}')
  call self%add_unit('m2 [length2].m-2 [length-2]{steradian}')
  call self%add_unit('kg [mass].s-2 [time-2].A-1 [current-1] (T[magnetic_flux_density]) {tesla}')
  call self%add_unit('kg [mass].m2 [length2].s-3 [time-3].A-1 [current-1] (V[voltage]) {volt}')
  call self%add_unit('kg [mass].m2 [length2].s-3 [time-3] (W[power]) {watt}')
  call self%add_unit('kg [mass].m2 [length2].s-2 [time-2].A-1 [current-1] (Wb[magnetic_flux]) {weber}')
  ! information technology
  call self%add_unit('bit [bit] {bit}')
  call self%add_unit('byte = 8 * bit = B = octet [bit] {byte}')
  call self%add_unit('bit [bit].s-1 [time-1](baud = Bd = bps) {baud}')
  ! common time
  call self%add_unit('minute = 60 * s = min [time](minute[time]){minute}')
  call self%add_unit('hour = 3600 * s = 60 * minute = hr [time](hour[time])  {hour}')
  call self%add_unit('day = 86400 * s = 1440 * minute = 24 * hour [time](day[time]){day}')
  call self%add_unit('week = 604800 * s = 10080 * minute = 168 * hour = 7 * day [time](week[time]){week}')
  ! USCS units for conversions
  call self%add_unit('in< = 0.0254 * m = inch> [length] {inch}')
  call self%add_unit('yd< = 0.9144 * m = yard> [length] {yard}')
  call self%add_unit('ft< = 0.3048 * m = foot> [length] {foot}')
  call self%add_unit('mi< = 1609.344 * m = mile> [length] {mile}')
  call self%add_unit('lb< = 0.45359237 * kg = pound> [mass] {pound}')
  call self%add_unit('oz< = 0.028349523125 * kg = ounce> [mass] {ounce}')
  call self%add_unit('gram[mass].cm[length].s-2[time-2] (dyne=dyn[force]) {dyne}')
  ! decimal prefixes
  ! prefixes: the symbol first, the name last (used to name the prefixed units, e.g. kilometre)
  call self%add_prefix('1.e-24 * y = 1.e-24 * yocto')
  call self%add_prefix('1.e-21 * z = 1.e-21 * zepto')
  call self%add_prefix('1.e-18 * a = 1.e-18 * atto')
  call self%add_prefix('1.e-15 * f = 1.e-15 * femto')
  call self%add_prefix('1.e-12 * p = 1.e-12 * pico')
  call self%add_prefix('1.e-9 * n = 1.e-9 * nano')
  call self%add_prefix('1.e-6 * u = 1.e-6 * micro')
  call self%add_prefix('1.e-3 * m = 1.e-3 * milli')
  call self%add_prefix('1.e-2 * c = 1.e-2 * centi')
  call self%add_prefix('1.e-1 * d = 1.e-1 * deci')
  call self%add_prefix('1.e1 * da = 1.e1 * deka = 1.e1 * deca')
  call self%add_prefix('1.e2 * h = 1.e2 * hecto')
  call self%add_prefix('1.e3 * k = 1.e3 * kilo')
  call self%add_prefix('1.e6 * M = 1.e6 * mega')
  call self%add_prefix('1.e9 * G = 1.e9 * giga')
  call self%add_prefix('1.e12 * T = 1.e12 * tera')
  call self%add_prefix('1.e15 * P = 1.e15 * peta')
  call self%add_prefix('1.e18 * E = 1.e18 * exa')
  call self%add_prefix('1.e21 * Z = 1.e21 * zetta')
  call self%add_prefix('1.e24 * Y = 1.e24 * yotta')
  ! binary prefixes, 1024**n
  call self%add_prefix('1024. * Ki = 1024. * kibi')
  call self%add_prefix('1048576. * Mi = 1048576. * mebi')
  call self%add_prefix('1073741824. * Gi = 1073741824. * gibi')
  call self%add_prefix('1099511627776. * Ti = 1099511627776. * tebi')
  call self%add_prefix('1125899906842624. * Pi = 1125899906842624. * pebi')
  call self%add_prefix('1152921504606846976. * Ei = 1152921504606846976. * exbi')
  call self%add_prefix('1180591620717411303424. * Zi = 1180591620717411303424. * zebi')
  call self%add_prefix('1208925819614629174706176. * Yi = 1208925819614629174706176. * yobi')
  ! common constants
  ! constants: exact by the definition of the 2019 SI (c, e, h, k, NA, and so R = NA k), CODATA 2018 the others
  call self%add_constant(qreal(magnitude=4._RKP * atan(1._RKP), name='pi'))
  call self%add_constant(qreal(magnitude=299792458._RKP, unit=self%unit('metre.second-1'), name='speed_of_light'))
  call self%add_constant(qreal(magnitude=9.806650_RKP, unit=self%unit('metre.second-2'), name='gravity'))
  call self%add_constant(qreal(magnitude=1.602176634e-19_RKP, unit=self%unit('coulomb'), name='elementary_charge'))
  call self%add_constant(qreal(magnitude=9.1093837015e-31_RKP, unit=self%unit('kilogram'), name='electron_mass'))
  call self%add_constant(qreal(magnitude=1.67492749804e-27_RKP, unit=self%unit('kilogram'), name='neutron_mass'))
  call self%add_constant(qreal(magnitude=1.67262192369e-27_RKP, unit=self%unit('kilogram'), name='proton_mass'))
  call self%add_constant(qreal(magnitude=6.62607015e-34_RKP,                    &
                               unit=uom('kg [mass].m2 [length2].s-1 [time-1]'), &
                               name='planck_constant'))
  call self%add_constant(qreal(magnitude=6.67430e-11_RKP,                           &
                               unit=uom('m3 [length3].kg-1 [mass-1].s-2 [time-2]'), &
                               name='newton_gravitation_constant'))
  call self%add_constant(qreal(magnitude=8.31446261815324_RKP,                                                                 &
                               unit=uom('kg [mass].m2 [length2].s-2 [time-2].mol-1 [substance-1].K-1 [temperature-1]'), &
                               name='molar_gas_constant'))
  call self%add_constant(qreal(magnitude=6.02214076e23_RKP, unit=uom('mol-1 [substance-1]'), name='avogadro_number'))
  call self%add_constant(qreal(magnitude=1.380649e-23_RKP,                                         &
                               unit=uom('kg [mass].m2 [length2].s-2 [time-2].K-1 [temperature-1]'), &
                               name='boltzmann_constant'))
  ! derived constants
  c = self%const('speed_of_light')

  ! since the 2019 SI the vacuum permeability is measured, no more 4 pi 1e-7 exactly
  mu_0 = 1.25663706212e-6_RKP * uom('kg [mass].m [length].s-2 [time-2].A-2 [current-2]')
  call mu_0%set(name='vacuum_permeability')
  call self%add_constant(mu_0)

  epsilon_0 = (mu_0 * c**2)**(-1)
  call epsilon_0%set(name='vacuum_permittivity')
  call self%add_constant(epsilon_0)

  Z_0 = mu_0 * c
  call Z_0%set(name='vacuum_impedance')
  call self%add_constant(Z_0)
  endsubroutine initialize
