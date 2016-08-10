!< FURY definition of units symbols of *International System of Units*.
module fury_units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of units symbols of *International System of Units*.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
use fury_units_base_si
use fury_units_derived_si
use fury_units_system_abstract
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(units_system_abstract) :: units_system_si
  ! units base
  type(unit_ampere)   :: ampere   !< The ampere unit instance.
  type(unit_candela)  :: candela  !< The candela unit instance.
  type(unit_kelvin)   :: kelvin   !< The kelvin unit instance.
  type(unit_kilogram) :: kilogram !< The kilogram unit instance.
  type(unit_metre)    :: metre    !< The metre unit instance.
  type(unit_mole)     :: mole     !< The mole unit instance.
  type(unit_second)   :: second   !< The second unit instance.
  ! units derived
  type(unit_coulomb)          :: coulomb          !< The coulomb unit instance.
  type(unit_farad)            :: farad            !< The farad unit instance.
  type(unit_henry)            :: henry            !< The henry unit instance.
  type(unit_hertz)            :: hertz            !< The hertz unit instance.
  type(unit_joule)            :: joule            !< The joule unit instance.
  type(unit_lumen)            :: lumen            !< The lumen unit instance.
  type(unit_lux)              :: lux              !< The lux unit instance.
  type(unit_metre_per_second) :: metre_per_second !< The metre_per_second unit instance.
  type(unit_metre_square)     :: metre_square     !< The metre_square unit instance.
  type(unit_newton)           :: newton           !< The newton unit instance.
  type(unit_ohm)              :: ohm              !< The ohm unit instance.
  type(unit_pascal)           :: pascal           !< The pascal unit instance.
  type(unit_radian)           :: radian           !< The radian unit instance.
  type(unit_siemens)          :: siemens          !< The siemens unit instance.
  type(unit_steradian)        :: steradian        !< The steradian unit instance.
  type(unit_tesla)            :: tesla            !< The tesla unit instance.
  type(unit_volt)             :: volt             !< The volt unit instance.
  type(unit_watt)             :: watt             !< The watt unit instance.
  type(unit_weber)            :: weber            !< The weber unit instance.
  contains
    ! public deferred methods
    procedure, pass(self) :: associate_unit !< Associate unit by dimensionality.
    procedure, pass(self) :: initialize     !< Initialize the units system.
endtype units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine associate_unit(self, dimensionality, unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Associate unit by dimensionality.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(units_system_si), intent(in), target   :: self           !< The units system.
  character(*),           intent(in)           :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for m.
  class(unit_abstract),   intent(out), pointer :: unit           !< Unit of measure of quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(dimensionality)
  ! units base
  case('[current]')
    unit => self%ampere
  case('[luminosity]')
    unit => self%candela
  case('[temperature]')
    unit => self%kelvin
  case('[mass]')
    unit => self%kilogram
  case('[length]')
    unit => self%metre
  case('[substance]')
    unit => self%mole
  case('[time]')
    unit => self%second
  ! units derived
  case('[length]/[time]')
    unit => self%metre_per_second
  case('[length]*[length]')
    unit => self%metre_square
  case default
    unit => null()
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine associate_unit

  subroutine initialize(self, acronym, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the units system.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(units_system_si), intent(inout)         :: self    !< The units system.
  character(*),           intent(in),  optional :: acronym !< Units system acronym, e.g. "SI" for the International System.
  integer(I_P),           intent(out), optional :: error   !< Error code, 0 => no errors happen.
  integer(I_P)                                  :: error_  !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 0
  self%acronym = 'SI' ; if (present(acronym)) self%acronym = acronym
  ! units base
  call self%ampere%set(error=error_)
  call self%candela%set(error=error_)
  call self%kelvin%set(error=error_)
  call self%kilogram%set(error=error_)
  call self%metre%set(error=error_)
  call self%mole%set(error=error_)
  call self%second%set(error=error_)
  ! units derived
  call self%coulomb%set(error=error_)
  call self%farad%set(error=error_)
  call self%henry%set(error=error_)
  call self%hertz%set(error=error_)
  call self%joule%set(error=error_)
  call self%lumen%set(error=error_)
  call self%lux%set(error=error_)
  call self%metre_per_second%set(error=error_)
  call self%metre_square%set(error=error_)
  call self%newton%set(error=error_)
  call self%ohm%set(error=error_)
  call self%pascal%set(error=error_)
  call self%radian%set(error=error_)
  call self%siemens%set(error=error_)
  call self%steradian%set(error=error_)
  call self%tesla%set(error=error_)
  call self%volt%set(error=error_)
  call self%watt%set(error=error_)
  call self%weber%set(error=error_)
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize
endmodule fury_units_system_si
