!< FURY definition of unit of measure class.
module fury_uom
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of unit of measure class.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_reference
use fury_uom_symbol
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: uom
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: uom
  !< Unit of measure (UOM) class.
  !<
  !< The string format definition of a valid FURY unit is as following:
  !<
  !< `kg [mass].m-1 = metre-1 = meter-1 [length-1].s-2 = seconds-2 = sec-2 [time-2](Pa[pressure]){pascal}`
  !< `km = 1000 * m [length].h-1 = 3600 s-1 [time-1](km/h[speed]){km/h}`
  !<
  !< where
  !<
  !<+ the terms `[...]` define the *dimension* of each reference and are optional (the white spaces are ignored); moreover the
  !<  exponents of dimensions can be omitted: in this case they are inferred from the symbol reference exponents; in the case
  !<  they are explicitely written they must match the corresponding symbols ones or an error is raised;
  !<+ the term `(...)` defines a unit *alias* that is optional and must come always after unit reference definition;
  !<+ the term `{...}` defines the unit name that is optional and must be always the last term.
  !<
  !< Other valid string inputs of the same above *pressure* unit are:
  !<
  !< `kg.m-1.s-2` a unit without specified dimensions, alias and name;
  !< `kg.m-1.s-2(Pa)` a unit without specified dimensions and name, but with an alias without alias dimension;
  !< `kg.m-1.s-2(Pa[pressure])` a unit without specified dimensions and name, but with an alias with alias dimension;
  !< `kg.m-1.s-2{pascal}` a unit without specified dimensions and alias, but with a name;
  !< `kg.m-1.s-2(Pa){pascal}` a unit without specified dimensions, but with an alias without alias dimension and a name;
  !<
  !< @note It is better to avoid to uncomplete list of dimensions for references: define all dimensions for all references
  !< or avoid to define dimensions at all.
  type(uom_reference), allocatable, private :: references(:)           !< Reference units of the unit.
  type(uom_reference), allocatable, private :: alias                   !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal.
  integer(I_P),                     private :: references_number=0_I_P !< References number.
  character(len=:), allocatable             :: name                    !< Unit name.
  contains
    ! public methods
    procedure, pass(self) :: has_reference !< Check if the unit has a refence unit.
    procedure, pass(self) :: is_defined    !< Check if the unit is defined.
    procedure, pass(self) :: set           !< Set the unit.
    procedure, pass(self) :: stringify     !< Return a string representaion of the unit.
    procedure, pass(self) :: unset         !< unset the unit.
    ! public generic names
    generic :: assignment(=) => assign_string, &
                                assign_uom             !< Overloading `=` operator.
    generic :: operator(+) => add                      !< Overloading `+` operator.
    generic :: operator(/) => div                      !< Overloading `/` operator.
    generic :: operator(*) => mul                      !< Overloading `*` operator.
    generic :: operator(-) => sub                      !< Overloading `-` operator.
    generic :: operator(**) => pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P        !< Overloading `**` operator.
    generic :: operator(==) => is_equal                !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal            !< Overloading `/=` operator.
    generic :: operator(.compatible.) => is_compatible !< Definition of `.compatible.` operator.
    ! private methods
    procedure, pass(self), private :: add_reference            !< Add a refence unit to unit.
    ! procedure, pass(self) :: get_alias_scale          !< Get the scale factor from an alias.
    procedure, pass(self), private :: has_alias                !< Check if the unit has an alias.
    procedure, pass(self), private :: has_name                 !< Check if the unit has a name.
    procedure, pass(self), private :: has_reference_compatible !< Check if the unit has a compatible refence unit.
    procedure, pass(self), private :: is_compatible            !< Check if unit is compatible with another one.
    procedure, pass(self), private :: is_equal                 !< Check if unit is equal with another one.
    procedure, pass(self), private :: is_not_equal             !< Check if unit is not equal with another one.
    procedure, pass(self), private :: parse                    !< Parse unit definition from an input string.
    procedure, pass(self), private :: parse_alias              !< Parse unit alias from an input string.
    procedure, pass(self), private :: parse_name               !< Parse unit name from an input string.
    procedure, pass(self), private :: parse_references         !< Parse refence units from an input string.
    procedure, pass(self), private :: update_references_number !< Update refence units number counter.
    ! operators
    procedure, pass(lhs), private :: assign_string !< `uom = string` assignament.
    procedure, pass(lhs), private :: assign_uom    !< `uom = uom` assignament.
    procedure, pass(lhs), private :: add           !< `uom + uom` operator.
    procedure, pass(lhs), private :: div           !< `uom / uom` operator.
    procedure, pass(lhs), private :: mul           !< `uom * uom` operator.
    procedure, pass(lhs), private :: sub           !< `uom - uom` operator.
    procedure, pass(lhs), private :: pow_I8P       !< `uom ** integer(I8P)` operator.
    procedure, pass(lhs), private :: pow_I4P       !< `uom ** integer(I4P)` operator.
    procedure, pass(lhs), private :: pow_I2P       !< `uom ** integer(I2P)` operator.
    procedure, pass(lhs), private :: pow_I1P       !< `uom ** integer(I1P)` operator.
endtype uom

interface uom
  !< Ovearloading [[uom]] name with a set of creator functions.
  module procedure creator_from_string, creator_from_other_unit
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator_from_string(source, alias, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit from an input source string..
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*),        intent(in)           :: source !< Source input string definition of the unit.
  type(uom_reference), intent(in), optional :: alias  !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in), optional :: name   !< Unit name.
  type(uom)                                 :: unit   !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%parse(source=source)
  call unit%set(alias=alias, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string
!
  function creator_from_other_unit(source, alias, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit from another unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom),           intent(in)           :: source !< Source input unit.
  type(uom_reference), intent(in), optional :: alias  !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in), optional :: name   !< Unit name.
  type(uom)                                 :: unit   !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  unit = source
  call unit%set(alias=alias, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_other_unit

  subroutine raise_error_bad_input_alias(source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the bad input alias error.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: source !< Input source containing the unit definition with bad alias specifier.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: input source string "'//source//'" has bad alias specifier for the unit!'
  write(stderr, '(A)')'  the alias must enclosed into "()" brackets and must be placed after the references definition'
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_bad_input_alias

  subroutine raise_error_bad_input_name(source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the bad input name error.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: source !< Input source containing the unit definition with bad name specifier.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: input source string "'//source//'" has bad name specifier for the unit!'
  write(stderr, '(A)')'  the name must enclosed into "{}" brackets and must be placed as the last term of the input string'
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_bad_input_name

  subroutine raise_error_incompatibility(lhs, rhs, operation)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the incompatibility error.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom),    intent(in) :: lhs       !< Left hand side of the operator.
  type(uom),    intent(in) :: rhs       !< Rigth hand side of the operator.
  character(*), intent(in) :: operation !< Description of the operation errored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: left and right terms of "'//operation//'" have incompatible units!'
  write(stderr, '(A)')'  LHS: '//lhs%stringify(with_dimensions=.true.)
  write(stderr, '(A)')'  RHS: '//rhs%stringify(with_dimensions=.true.)
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_incompatibility

  subroutine raise_error_disequality(lhs, rhs, operation)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the disequality error.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom),    intent(in) :: lhs       !< Left hand side of the operator.
  type(uom),    intent(in) :: rhs       !< Rigth hand side of the operator.
  character(*), intent(in) :: operation !< Description of the operation errored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: left and right terms of "'//operation//'" have disequal units!'
  write(stderr, '(A)')'  LHS: '//lhs%stringify(with_dimensions=.true.)
  write(stderr, '(A)')'  RHS: '//rhs%stringify(with_dimensions=.true.)
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_disequality

  subroutine remove_reference(references, id)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a reference from a list of rerefences.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference), intent(inout), allocatable :: references(:)     !< References list.
  integer(I_P),        intent(in)                 :: id                !< Index of reference to remove.
  type(uom_reference), allocatable                :: references_tmp(:) !< References list temporary copy.
  integer(I_P)                                    :: references_number !< References number.
  integer(I_P)                                    :: i                 !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  references_number = size(references, dim=1)
  allocate(references_tmp(1:references_number-1))
  if (id==1) then
    do i=2, references_number
      references_tmp(i-1) = references(i)
    enddo
  elseif (id==references_number )  then
    do i=1, references_number-1
      references_tmp(i) = references(i)
    enddo
  else
    do i=1, id-1
      references_tmp(i) = references(i)
    enddo
    do i=id, references_number-1
      references_tmp(i) = references(i+1)
    enddo
  endif
  call move_alloc(from=references_tmp, to=references)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove_reference

  ! public methods
! function get_alias_scale(self, alias_unit) result(scale_factor)
! !---------------------------------------------------------------------------------------------------------------------------------
! !< Get the scale factor from an alias.
! !---------------------------------------------------------------------------------------------------------------------------------
! class(uom), intent(in) :: self         !< The uom.
! type(uom),  intent(in) :: alias_unit   !< Alias symbol queried.
! real(R_P)              :: scale_factor !< Symbol scale factor.
! integer(I_P)           :: r            !< Counter.
! !---------------------------------------------------------------------------------------------------------------------------------
!
! !---------------------------------------------------------------------------------------------------------------------------------
!     print*, 'cazzo ', self%stringify(), alias_unit%stringify(), self%references_number
! scale_factor = 1._R_P
! if (self%is_defined().and.alias_unit%is_defined().and.self%references_number==alias_unit%references_number) then
!   do r=1, self%references_number
!     if (alias_unit%references(r)%is_compatible(self%references(r))) then
!       scale_factor = scale_factor * alias_unit%references(r)%get_alias_scale(self%references(r)%symbol)
!     print*, 'cazzo ', scale_factor
!     else
!     endif
!   enddo
! endif
! !---------------------------------------------------------------------------------------------------------------------------------
! endfunction get_alias_scale

  elemental function has_reference(self, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a reference unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(in) :: self          !< The unit.
  type(uom_reference), intent(in) :: reference     !< Reference unit to check the presence of.
  logical                         :: has_reference !< reference unit presence status.
  integer(I_P)                    :: r             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_reference = .false.
  if (self%is_defined()) then
    do r=1, self%references_number
      has_reference = self%references(r) == reference
      if (has_reference) exit
    enddo
    if (.not.has_reference) then
      if (self%has_alias()) has_reference = self%alias == reference
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_reference

  elemental function is_defined(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has been defined, namely it has defined references.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self       !< The unit.
  logical                :: is_defined !< Definition status.
  integer                :: r          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%references)
  if (is_defined) then
    do r=1, self%references_number
      is_defined = self%references(r)%is_defined()
      if (.not.is_defined) exit
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_defined

  subroutine set(self, references, alias, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(inout)         :: self           !< The unit.
  type(uom_reference), intent(in),  optional :: references(1:) !< Unit references of the unit.
  type(uom_reference), intent(in),  optional :: alias          !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in),  optional :: name           !< Unit name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(references)) then
    if (allocated(self%references)) deallocate(self%references)
    self%references = references
  endif
  call self%update_references_number()
  if (present(alias)) then
    if (.not.allocated(self%alias)) allocate(self%alias)
    self%alias = alias
  endif
  if (present(name)) self%name = name
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  function stringify(self, with_dimensions, with_aliases, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)           :: self            !< The unit.
  logical,    intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,    intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,    intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(len=:), allocatable    :: raw             !< Raw characters data.
  integer(I_P)                     :: s               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    raw = ''
    do s=1, self%references_number
      raw = raw//'.'//self%references(s)%stringify(with_aliases=with_aliases, compact_reals=compact_reals)
    enddo
    raw = raw(2:)
    if (present(with_dimensions)) then
      if (with_dimensions) then
        raw = raw//' ['
        do s=1, self%references_number
          raw = raw//self%references(s)%dimensionality()//'.'
        enddo
        raw(len(raw):len(raw)) = ']'
      endif
    endif
    if (present(with_aliases)) then
      if (with_aliases.and.self%has_alias()) then
        raw = raw//' ('//self%alias%stringify(with_dimensions=with_dimensions, compact_reals=compact_reals)//')'
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(inout) :: self !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%references)) deallocate(self%references)
  if (allocated(self%alias)) deallocate(self%alias)
  self%references_number = 0_I_P
  if (allocated(self%name)) deallocate(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  subroutine add_reference(self, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add reference unit to unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(inout) :: self          !< The unit.
  type(uom_reference), intent(in)    :: reference     !< Unit reference to be added.
  type(uom_reference), allocatable   :: references(:) !< Reference unit(s) temporary array.
  integer(I_P)                       :: r             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined().and.(.not.self%has_reference(reference=reference))) then
    allocate(references(self%references_number+1))
    ! references(1:self%references_number) = self%references
    do r=1, self%references_number
      references(r) = self%references(r)
    enddo
    references(self%references_number+1) = reference
    call move_alloc(from=references, to=self%references)
    self%references_number = self%references_number + 1_I_P
  else
    allocate(self%references(1))
    self%references(1) = reference
    self%references_number = 1_I_P
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_reference

  elemental function has_alias(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has an alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self      !< The unit.
  logical                :: has_alias !< Alias presence status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_alias = allocated(self%alias)
  if (has_alias) has_alias = self%alias%is_defined()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_alias

  elemental function has_name(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a name.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self     !< The unit.
  logical                :: has_name !< Name presence status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_name = allocated(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_name

  elemental function has_reference_compatible(self, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a compatible reference unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(in) :: self                     !< The unit.
  type(uom_reference), intent(in) :: reference                !< Reference unit to check the presence of.
  logical                         :: has_reference_compatible !< reference unit presence status.
  integer(I_P)                    :: r                        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_reference_compatible = .false.
  if (self%is_defined()) then
    do r=1, self%references_number
      has_reference_compatible = self%references(r).compatible.reference
      if (has_reference_compatible) exit
    enddo
    if (.not.has_reference_compatible) then
      if (self%has_alias()) has_reference_compatible = self%alias.compatible.reference
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_reference_compatible

  elemental function is_compatible(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self          !< The unit.
  class(uom), intent(in) :: other         !< The other unit.
  logical                :: is_compatible !< Compatibility check result.
  integer(I_P)           :: r             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_compatible = .false.
  if (self%is_defined().and.other%is_defined()) then
    is_compatible = (self%references_number==other%references_number)
    if (is_compatible) then
      do r=1, self%references_number
        is_compatible = other%has_reference_compatible(reference=self%references(r))
        if (.not.is_compatible) exit
      enddo
    endif
    if (.not.is_compatible) then
      ! compare against alias
      if (self%has_alias().and.other%references_number==1) then
        is_compatible = other%references(1).compatible.self%alias
      elseif (other%has_alias().and.self%references_number==1) then
        is_compatible = self%references(1).compatible.other%alias
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  elemental function is_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is equal (has the same references units) with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self     !< The unit.
  class(uom), intent(in) :: other    !< The other unit.
  logical                :: is_equal !< Equality check result.
  integer(I_P)           :: r        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = .false.
  if (self%is_defined().and.other%is_defined()) then
    is_equal = (self%references_number==other%references_number)
    if (is_equal) then
      do r=1, self%references_number
        is_equal = other%has_reference(reference=self%references(r))
        if (.not.is_equal) exit
      enddo
    endif
    if (.not.is_equal) then
      ! compare against alias
      if (self%has_alias().and.other%references_number==1) then
        is_equal = other%references(1) == self%alias
      elseif (other%has_alias().and.self%references_number==1) then
        is_equal = self%references(1) == other%alias
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is not equal (has not the same references units) with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self         !< The unit.
  class(uom), intent(in) :: other        !< The other unit.
  logical                :: is_not_equal !< Disequality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit definition form an input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self       !< The unit.
  character(*), intent(in)    :: source     !< Input source string.
  type(string)                :: source_str !< Source input stringified.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%unset
  source_str = trim(adjustl(source))
  call self%parse_name(source=source_str)
  call self%parse_alias(source=source_str)
  call self%parse_references(source=source_str)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  subroutine parse_alias(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit alias form an input string and return also the source string without the alias data.
  !<
  !< @note It is assumed that the optional unit name has been already parsed and trimmed out form the input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self      !< The unit.
  type(string), intent(inout) :: source    !< Input source string.
  type(string), allocatable   :: tokens(:) !< String tokens.
  integer(I_P)                :: n1        !< Counter.
  integer(I_P)                :: n2        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n1 = source%count('(')
  n2 = source%count(')')
  if (n1==1.and.n2==1) then
    call source%split(sep='(', tokens=tokens)
    source = tokens(1)
    tokens(2) = tokens(2)%replace(old=')', new='')
    if (.not.allocated(self%alias)) allocate(self%alias)
    call self%alias%parse(source=tokens(2)%chars())
  elseif (n1>1.or.n2>1) then
    call raise_error_bad_input_alias(source=source%chars())
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_alias

  subroutine parse_name(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit name form an input string and return also the source string without the name data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self      !< The unit.
  type(string), intent(inout) :: source    !< Input source string.
  type(string), allocatable   :: tokens(:) !< String tokens.
  integer(I_P)                :: n1        !< Counter.
  integer(I_P)                :: n2        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n1 = source%count('{')
  n2 = source%count('}')
  if (n1==1.and.n2==1) then
    call source%split(sep='{', tokens=tokens)
    source = tokens(1)
    tokens(2) = tokens(2)%replace(old='}', new='')
    self%name = tokens(2)%chars()
  elseif (n1>1.or.n2>1) then
    call raise_error_bad_input_name(source=source%chars())
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_name

  subroutine parse_references(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse references units form an input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self          !< The unit.
  type(string), intent(in)    :: source        !< Input source string.
  type(string)                :: buffer        !< String buffer.
  type(string), allocatable   :: tokens(:)     !< String tokens.
  integer(I_P)                :: tokens_number !< Tokens number.
  integer(I_P)                :: t             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = source
  call buffer%split(tokens=tokens, sep='.')
  tokens_number = size(tokens, dim=1)
  if (allocated(self%references)) deallocate(self%references)
  allocate(self%references(1:tokens_number))
  do t=1, tokens_number
    call self%references(t)%parse(source=tokens(t)%chars())
  enddo
  call self%update_references_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_references

  subroutine update_references_number(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update references number counter.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(inout) :: self !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%references_number = 0_I_P
  if (self%is_defined()) self%references_number = size(self%references, dim=1)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine update_references_number

  ! operators
  subroutine assign_string(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom = string` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: lhs         !< Left hand side.
  character(*), intent(in)    :: rhs         !< Right hand side.
  type(uom)                   :: parsed_unit !< Unit arising from string input.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call parsed_unit%parse(source=rhs)
  if (.not.lhs%is_defined())  then
    lhs = parsed_unit
  else
    if (.not.lhs == parsed_unit) call raise_error_disequality(lhs=lhs, rhs=parsed_unit, operation='LHS = RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_string

  subroutine assign_uom(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom = uom` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(inout) :: lhs !< Left hand side.
  class(uom), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      if (allocated(lhs%references)) deallocate(lhs%references)
      ! lhs%references = rhs%references ! possible GNU bug
      allocate(lhs%references, source=rhs%references)
      lhs%references_number = rhs%references_number
      if (allocated(rhs%alias)) then
        if (.not.allocated(lhs%alias)) allocate(lhs%alias)
        lhs%alias = rhs%alias
      endif
      if (allocated(rhs%name)) lhs%name = rhs%name
    else
      if (.not.lhs==rhs) call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS = RHS')
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_uom

  function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom + uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  class(uom), intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs == rhs) then
    opr = lhs
  else
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS + RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom / uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)           :: lhs               !< Left hand side.
  class(uom), intent(in)           :: rhs               !< Right hand side.
  type(uom)                        :: opr               !< Operator result.
  type(uom_reference), allocatable :: lhs_references(:) !< Left hand side references.
  type(uom_reference), allocatable :: rhs_references(:) !< Right hand side references.
  integer(I_P)                     :: ls                !< Counter.
  integer(I_P)                     :: rs                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (lhs%is_defined().and.rhs%is_defined()) then
    ! lhs_references = lhs%references ! possible GNU bug
    ! rhs_references = rhs%references ! possible GNU bug
    allocate(lhs_references, source=lhs%references)
    allocate(rhs_references, source=rhs%references)
    do ls=1, size(lhs_references, dim=1)
      rs = 1
      remaining_rhs_references: do
        if (lhs_references(ls).compatible.rhs_references(rs)) then
          lhs_references(ls) = lhs_references(ls) / rhs_references(rs)
          ! pop up current reference from rhs references
          if (size(rhs_references, dim=1)>1) then
            call remove_reference(references=rhs_references, id=rs)
            rs = 1
          else
            deallocate(rhs_references)
          endif
        else
          ! check the next rhs references
          rs = rs + 1
        endif
        if (rs>=size(rhs_references, dim=1).or.(.not.allocated(rhs_references))) exit remaining_rhs_references
      enddo remaining_rhs_references
    enddo
    opr%references = lhs_references
    if (allocated(rhs_references)) then
      ! there are still rhs references not compatible with lhs ones that must be added
      do rs=1, size(rhs_references, dim=1)
        call opr%add_reference(reference=rhs_references(rs)**(-1))
      enddo
    endif
    if (lhs%has_name().and.rhs%has_name()) then
      opr%name = lhs%name//'/'//rhs%name
    endif
  endif
  call opr%update_references_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)           :: lhs               !< Left hand side.
  class(uom), intent(in)           :: rhs               !< Right hand side.
  type(uom)                        :: opr               !< Operator result.
  type(uom_reference), allocatable :: lhs_references(:) !< Left hand side references.
  type(uom_reference), allocatable :: rhs_references(:) !< Right hand side references.
  integer(I_P)                     :: ls                !< Counter.
  integer(I_P)                     :: rs                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (lhs%is_defined().and.rhs%is_defined()) then
    ! lhs_references = lhs%references ! possible GNU bug
    ! rhs_references = rhs%references ! possible GNU bug
    allocate(lhs_references, source=lhs%references)
    allocate(rhs_references, source=rhs%references)
    do ls=1, size(lhs_references, dim=1)
      rs = 1
      remaining_rhs_references: do
        if (lhs_references(ls).compatible.rhs_references(rs)) then
          lhs_references(ls) = lhs_references(ls) * rhs_references(rs)
          ! pop up current reference from rhs references
          if (size(rhs_references, dim=1)>1) then
            call remove_reference(references=rhs_references, id=rs)
            rs = 1
          else
            deallocate(rhs_references)
          endif
        else
          ! check the next rhs references
          rs = rs + 1
        endif
        if (rs>=size(rhs_references, dim=1).or.(.not.allocated(rhs_references))) exit remaining_rhs_references
      enddo remaining_rhs_references
    enddo
    opr%references = lhs_references
    if (allocated(rhs_references)) then
      ! there are still rhs references not compatible with lhs ones that must be added
      do rs=1, size(rhs_references, dim=1)
        call opr%add_reference(reference=rhs_references(rs))
      enddo
    endif
    if (lhs%has_name().and.rhs%has_name()) then
      opr%name = lhs%name//'*'//rhs%name
    endif
  endif
  call opr%update_references_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  function sub(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom - uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  class(uom), intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs==rhs) then
    opr = lhs
  else
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS - RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub

  function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I8P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I4P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I2P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I1P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_uom
