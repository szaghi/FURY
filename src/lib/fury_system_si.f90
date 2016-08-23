!< FURY definition of *International System of Units*.
module fury_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of *International System of Units*.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_generic
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: system_si
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: system_si
  !< International System of Units.
  character(len=:),   allocatable :: acronym            !< Units system acronym, e.g. "SI" for the International System.
  type(unit_generic), allocatable :: units(:)           !< Defined units.
  integer(I_P)                    :: units_number=0_I_P !< Number of units.
  contains
    ! public deferred methods
    ! procedure, pass(self) :: add_unit   !< Add a new unit.
    procedure, pass(self) :: free       !< Free the units system.
    procedure, pass(self) :: initialize !< Initialize the units system.
    procedure, pass(self) :: list_units !< Return the list of defined units.
endtype system_si
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental subroutine free(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free the units system.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_si), intent(inout) :: self !< The units system.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%acronym)) deallocate(self%acronym)
  if (allocated(self%units)) deallocate(self%units)
  self%units_number = 0_I_P
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free

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
  self%units_number = 26
  allocate(self%units(1:self%units_number))
  ! base units
  self%units(1) = unit_generic(symbols='A [current]', name='ampere')
  self%units(2) = unit_generic(symbols='cd [luminosity]', name='candela')
  self%units(3) = unit_generic(symbols='K [temperature]', name='kelvin')
  self%units(4) = unit_generic(symbols='kg [mass]', name='kilogram')
  self%units(5) = unit_generic(symbols='m [length]', name='metre')
  self%units(6) = unit_generic(symbols='mol [substance]', name='mole')
  self%units(7) = unit_generic(symbols='s [time]', name='second')
  ! ! units derived
  self%units(8 ) = unit_generic(symbols='s [time].A [current]', name='coulomb')
  self%units(9 ) = unit_generic(symbols='kg-1 [mass-1].m-2 [length-2].s4 [time4].A2 [current2]', name='farad')
  self%units(10) = unit_generic(symbols='kg [mass].m2 [length2].s-2 [time-2].A-2 [current-2]', name='henry')
  self%units(11) = unit_generic(symbols='s-1 [time-1]', name='hertz')
  self%units(12) = unit_generic(symbols='kg [mass].m2 [length2].s-2 [time-2]', name='joule')
  self%units(13) = unit_generic(symbols='cd [luminous_flux]', name='lumen')
  self%units(14) = unit_generic(symbols='m-2 [length-2].cd [luminosity]', name='lux')
  self%units(15) = unit_generic(symbols='m [length].s-1 [time-1]', name='metre.second-1')
  self%units(16) = unit_generic(symbols='m2 [length2]', name='metre2')
  self%units(17) = unit_generic(symbols='kg [mass].m [length].s-2 [time-2]', name='newton')
  self%units(18) = unit_generic(symbols='kg [mass].m2 [length2].s-3 [time-3].A-2 [current-2]', name='ohm')
  self%units(19) = unit_generic(symbols='kg [mass].m-1 [length-1].s-2 [time-2]', name='pascal')
  self%units(20) = unit_generic(symbols='m [length].m-1 [length-1]', name='radian')
  self%units(21) = unit_generic(symbols='kg-1 [mass-1].m-2 [length-2].s3 [time3].A2 [current2]', name='siemens')
  self%units(22) = unit_generic(symbols='m2 [length2].m-2 [length-2]', name='steradian')
  self%units(23) = unit_generic(symbols='kg [mass].s-2 [time-2].A-1 [current-1]', name='tesla')
  self%units(24) = unit_generic(symbols='kg [mass].m2 [length2].s-3 [time-3].A-1 [current-1]', name='volt')
  self%units(25) = unit_generic(symbols='kg [mass].m2 [length2].s-3 [time-3]', name='watt')
  self%units(26) = unit_generic(symbols='kg [mass].m2 [length2].s-2 [time-2].A-1 [current-1]', name='weber')
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  pure function list_units(self, with_dimensions) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the list defined units.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_si), intent(in)           :: self            !< The unit.
  logical,          intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  character(len=:), allocatable          :: raw             !< Raw characters data.
  integer(I_P)                           :: u               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (self%units_number>0) then
    do u=1, self%units_number
      raw = raw//new_line('a')//' '//trim(str(n=u, no_sign=.true.))//'. '//self%units(u)%name//': '//&
        self%units(u)%stringify(with_dimensions=with_dimensions)
    enddo
    raw = raw(2:)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction list_units
endmodule fury_system_si
