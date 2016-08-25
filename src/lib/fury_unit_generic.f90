!< FURY definition of *generic* unit class.
module fury_unit_generic
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of *generic* unit class.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_unit_symbol
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_generic
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: unit_generic
  !< Generic prototype of *unit*.
  !<
  !< The string format definition of a valid FURY unit is as following:
  !<
  !< `kg [mass].m-1 [length-1].s-2 [time-2](Pa[pressure]){pascal}`
  !<
  !< where
  !<
  !<+ the terms `[...]` define the *dimension* of each symbol and are optional (the white spaces are ignored); moreover ther
  !<  exponent of dimensions can be omitted: in this case they are inferred from the symbols exponents; in the case they are
  !<  explicitely written they must match the corresponding symbols ones or an error is raised;
  !<+ the term `(...)` defines a symbol *alias* that is optional and must come always after symbols definition;
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
  !< @note It is better to avoid to uncomplete list of dimensions for symbols: define all dimensions for all symbols or avoid to
  !< define dimensions at all.
  type(unit_symbol), allocatable :: symbols(:)           !< Symbol(s) of the unit.
  type(unit_symbol), allocatable :: alias                !< Alias symbol of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(len=:),  allocatable :: name                 !< Unit name.
  integer(I_P), private          :: symbols_number=0_I_P !< Symbols number.
  contains
    ! public methods
    procedure, pass(self) :: add_symbol          !< Add a symbol to unit.
    procedure, pass(self) :: add_symbols         !< Add symbols to unit.
    procedure, pass(self) :: are_symbols_defined !< Check if the symbols have been defined.
    procedure, pass(self) :: has_alias           !< Check if the unit has an alias.
    procedure, pass(self) :: has_name            !< Check if the unit has a name.
    procedure, pass(self) :: has_symbol          !< Check if the unit has a symbol.
    procedure, pass(self) :: is_compatible       !< Check if unit is compatible with another one.
    procedure, pass(self) :: parse               !< Parse unit definition from an input string.
    procedure, pass(self) :: set                 !< Set the unit.
    procedure, pass(self) :: stringify           !< Return a string representaion of the unit.
    procedure, pass(self) :: unset               !< Set the unit.
    ! public generic names
    generic :: assignment(=) => assign_string, &
                                assign_unit_generic !< Overloading `=` operator.
    generic :: operator(+) => add                   !< Overloading `+` operator.
    generic :: operator(/) => div                   !< Overloading `/` operator.
    generic :: operator(*) => mul                   !< Overloading `*` operator.
    generic :: operator(-) => sub                   !< Overloading `-` operator.
    generic :: operator(**) => pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P     !< Overloading `**` operator.
    generic :: operator(==) => is_equal             !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal         !< Overloading `/=` operator.
    ! private methods
    procedure, pass(self), private :: is_equal              !< Check if unit is equal with another one.
    procedure, pass(self), private :: is_not_equal          !< Check if unit is not equal with another one.
    procedure, pass(self), private :: parse_alias           !< Parse unit alias from an input string.
    procedure, pass(self), private :: parse_name            !< Parse unit name from an input string.
    procedure, pass(self), private :: parse_symbols         !< Parse unit symbols from an input string.
    procedure, pass(self), private :: update_symbols_number !< Update symbols number counter.
    ! operators
    procedure, pass(lhs), private :: assign_string       !< `unit_generic = string` assignament.
    procedure, pass(lhs), private :: assign_unit_generic !< `unit_generic = unit_generic` assignament.
    procedure, pass(lhs), private :: add                 !< `unit_generic + unit_generic` operator.
    procedure, pass(lhs), private :: div                 !< `unit_generic / unit_generic` operator.
    procedure, pass(lhs), private :: mul                 !< `unit_generic * unit_generic` operator.
    procedure, pass(lhs), private :: sub                 !< `unit_generic - unit_generic` operator.
    procedure, pass(lhs), private :: pow_I8P             !< `unit_generic ** integer(I8P)` operator.
    procedure, pass(lhs), private :: pow_I4P             !< `unit_generic ** integer(I4P)` operator.
    procedure, pass(lhs), private :: pow_I2P             !< `unit_generic ** integer(I2P)` operator.
    procedure, pass(lhs), private :: pow_I1P             !< `unit_generic ** integer(I1P)` operator.
endtype unit_generic

interface unit_generic
  !< Ovearloading [[unit_generic]] name with a set of creator functions.
  module procedure creator_from_string, creator_from_other_unit
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator_from_string(source, alias, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit from an input source string..
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*),      intent(in)           :: source !< Source input string definition of the unit.
  type(unit_symbol), intent(in), optional :: alias  !< Alias symbol of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),      intent(in), optional :: name   !< Unit name.
  type(unit_generic)                      :: unit   !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%parse(source=source)
  call unit%set(alias=alias, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string

  function creator_from_other_unit(source, alias, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit from another unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(unit_generic), intent(in)           :: source !< Source input unit.
  type(unit_symbol),  intent(in), optional :: alias  !< Alias symbol of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),       intent(in), optional :: name   !< Unit name.
  type(unit_generic)                       :: unit   !< The unit.
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
  write(stderr, '(A)')'  the alias must enclosed into "()" brackets and must be placed after the symbols definition'
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
  type(unit_generic), intent(in) :: lhs       !< Left hand side of the operator.
  type(unit_generic), intent(in) :: rhs       !< Rigth hand side of the operator.
  character(*),       intent(in) :: operation !< Description of the operation errored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: left and right terms of "'//operation//'" have incompatible units!'
  write(stderr, '(A)')'  LHS: '//lhs%stringify(with_dimensions=.true.)
  write(stderr, '(A)')'  RHS: '//rhs%stringify(with_dimensions=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_incompatibility

  subroutine raise_error_disequality(lhs, rhs, operation)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the disequality error.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(unit_generic), intent(in) :: lhs       !< Left hand side of the operator.
  type(unit_generic), intent(in) :: rhs       !< Rigth hand side of the operator.
  character(*),       intent(in) :: operation !< Description of the operation errored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: left and right terms of "'//operation//'" have disequal units!'
  write(stderr, '(A)')'  LHS: '//lhs%stringify(with_dimensions=.true.)
  write(stderr, '(A)')'  RHS: '//rhs%stringify(with_dimensions=.true.)
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_disequality

  ! public methods
  subroutine add_symbol(self, symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add symbols to unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self       !< The unit.
  type(unit_symbol),   intent(in)    :: symbol     !< Unit symbol.
  type(unit_symbol), allocatable     :: symbols(:) !< Litteral symbol(s), e.g. "m.s-1" for metres/seconds, array var.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%are_symbols_defined().and.(.not.self%has_symbol(symbol=symbol))) then
    allocate(symbols(self%symbols_number+1))
    symbols(1:self%symbols_number) = self%symbols
    symbols(self%symbols_number+1) = symbol
    call move_alloc(from=symbols, to=self%symbols)
    self%symbols_number = self%symbols_number + 1_I_P
  else
    allocate(self%symbols(1))
    self%symbols(1) = symbol
    self%symbols_number = 1_I_P
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_symbol

  subroutine add_symbols(self, symbols)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add symbols to unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self        !< The unit.
  type(unit_symbol),   intent(in)    :: symbols(1:) !< Litteral symbol(s), e.g. "m.s-1" for metres/second.
  integer(I_P)                       :: s           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%are_symbols_defined()) then
    do s=1, size(symbols, dim=1)
      call self%add_symbol(symbol=symbols(s))
    enddo
  else
    self%symbols = symbols
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_symbols

  elemental function are_symbols_defined(self) result(are_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbols have been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self        !< The unit.
  logical                         :: are_defined !< Symbols definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  are_defined = allocated(self%symbols)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction are_symbols_defined

  elemental function has_alias(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has an alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self      !< The unit.
  logical                         :: has_alias !< Alias presence status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_alias = allocated(self%alias)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_alias

  elemental function has_name(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a name.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self     !< The unit.
  logical                         :: has_name !< Name presence status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_name = allocated(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_name

  elemental function has_symbol(self, symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self       !< The unit.
  type(unit_symbol),   intent(in) :: symbol     !< Symbol to check the presence of.
  logical                         :: has_symbol !< Symbol presence status.
  integer(I_P)                    :: s          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_symbol = .false.
  if (self%are_symbols_defined()) then
    do s=1, self%symbols_number
      has_symbol = self%symbols(s) == symbol
      if (has_symbol) exit
    enddo
    if (.not.has_symbol) then
      if (self%has_alias()) has_symbol = self%alias == symbol
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_symbol

  elemental function is_compatible(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self          !< The unit.
  class(unit_generic), intent(in) :: other         !< The other unit.
  logical                         :: is_compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_compatible = .true.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit definition form an input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self       !< The unit.
  character(*),        intent(in)    :: source     !< Input source string.
  type(string)                       :: source_str !< Source input stringified.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  source_str = trim(adjustl(source))
  call self%parse_name(source=source_str)
  call self%parse_alias(source=source_str)
  call self%parse_symbols(source=source_str)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  subroutine set(self, symbols, alias, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout)         :: self        !< The unit.
  type(unit_symbol),   intent(in),  optional :: symbols(1:) !< Symbol(s) of the unit.
  type(unit_symbol),   intent(in),  optional :: alias       !< Alias symbol of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in),  optional :: name        !< Unit name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(symbols)) self%symbols = symbols
  call self%update_symbols_number()
  if (present(alias)) then
    if (.not.allocated(self%alias)) allocate(self%alias)
    self%alias = alias
  endif
  if (present(name)) self%name = name
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  function stringify(self, with_dimensions, with_alias) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in)           :: self            !< The unit.
  logical,             intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,             intent(in), optional :: with_alias      !< Flag to activate alias printing.
  character(len=:), allocatable             :: raw             !< Raw characters data.
  integer(I_P)                              :: s               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%are_symbols_defined()) then
    raw = ''
    do s=1, self%symbols_number
      raw = raw//'.'//self%symbols(s)%stringify()
    enddo
    raw = raw(2:)
    if (present(with_dimensions)) then
      if (with_dimensions) then
        raw = raw//' ['
        do s=1, self%symbols_number
          raw = raw//self%symbols(s)%dimensionality()//'.'
        enddo
        raw(len(raw):len(raw)) = ']'
      endif
    endif
    if (present(with_alias)) then
      if (with_alias.and.self%has_alias()) then
        raw = raw//' ('//self%alias%stringify(with_dimension=with_dimensions)//')'
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%symbols)) deallocate(self%symbols)
  if (allocated(self%alias)) deallocate(self%alias)
  if (allocated(self%name)) deallocate(self%name)
  self%symbols_number = 0_I_P
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  elemental function is_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is equal (has the same symbols) with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self     !< The unit.
  class(unit_generic), intent(in) :: other    !< The other unit.
  logical                         :: is_equal !< Equality check result.
  integer(I_P)                    :: s        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = .false.
  if (self%are_symbols_defined().and.other%are_symbols_defined()) then
    is_equal = (self%symbols_number==other%symbols_number)
    if (is_equal) then
      do s=1, self%symbols_number
        is_equal = other%has_symbol(symbol=self%symbols(s))
        if (.not.is_equal) exit
      enddo
    endif
    if (.not.is_equal) then
      ! compare against alias
      if (self%has_alias().and.other%symbols_number==1) then
        is_equal = other%symbols(1) == self%alias
      elseif (other%has_alias().and.self%symbols_number==1) then
        is_equal = self%symbols(1) == other%alias
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is not equal (has not the same symbols) with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: self         !< The unit.
  class(unit_generic), intent(in) :: other        !< The other unit.
  logical                         :: is_not_equal !< Disequality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  subroutine parse_alias(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit alias form an input string and return also the source string without the alias data.
  !<
  !< @note It is assumed that the optional unit name has been already parsed and trimmed out form the input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self      !< The unit.
  type(string),        intent(inout) :: source    !< Input source string.
  type(string), allocatable          :: tokens(:) !< String tokens.
  integer(I_P)                       :: n1        !< Counter.
  integer(I_P)                       :: n2        !< Counter.
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
  class(unit_generic), intent(inout) :: self      !< The unit.
  type(string),        intent(inout) :: source    !< Input source string.
  type(string), allocatable          :: tokens(:) !< String tokens.
  integer(I_P)                       :: n1        !< Counter.
  integer(I_P)                       :: n2        !< Counter.
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

  subroutine parse_symbols(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit symbols form an input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self   !< The unit.
  type(string),        intent(in)    :: source !< Input source string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%symbols = parse_unit_symbols(symbols=source%chars())
  call self%update_symbols_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_symbols

  subroutine update_symbols_number(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update symbols number counter.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%symbols_number = 0_I_P
  if (self%are_symbols_defined()) self%symbols_number = size(self%symbols, dim=1)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine update_symbols_number

  ! operators
  subroutine assign_string(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic = string` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: lhs         !< Left hand side.
  character(*),        intent(in)    :: rhs         !< Right hand side.
  type(unit_generic)                 :: parsed_unit !< Unit arising from string input.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call parsed_unit%parse(source=rhs)
  if (.not.lhs%are_symbols_defined())  then
    lhs = parsed_unit
  else
    if (.not.lhs == parsed_unit) call raise_error_disequality(lhs=lhs, rhs=parsed_unit, operation='LHS = RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_string

  subroutine assign_unit_generic(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic = unit_generic` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: lhs !< Left hand side.
  class(unit_generic), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%are_symbols_defined())  then
    if (.not.lhs%are_symbols_defined())  then
      lhs%symbols = rhs%symbols
      if (allocated(rhs%alias)) then
        if (.not.allocated(lhs%alias)) allocate(lhs%alias)
        lhs%alias = rhs%alias
      endif
      if (allocated(rhs%name)) lhs%name = rhs%name
      lhs%symbols_number = rhs%symbols_number
    else
      if (.not.lhs==rhs) call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS = RHS')
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_unit_generic

  function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic + unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs !< Left hand side.
  class(unit_generic), intent(in) :: rhs !< Right hand side.
  type(unit_generic)              :: opr !< Operator result.
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
  !< `unit_generic / unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs            !< Left hand side.
  class(unit_generic), intent(in) :: rhs            !< Right hand side.
  type(unit_generic)              :: opr            !< Operator result.
  type(unit_symbol), allocatable  :: lhs_symbols(:) !< Left hand side symbols.
  type(unit_symbol), allocatable  :: rhs_symbols(:) !< Right hand side symbols.
  integer(I_P)                    :: ls             !< Counter.
  integer(I_P)                    :: rs             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (lhs%are_symbols_defined().and.rhs%are_symbols_defined()) then
    lhs_symbols = lhs%symbols
    rhs_symbols = rhs%symbols
    do ls=1, size(lhs_symbols, dim=1)
      rs = 1
      remaining_rhs_symbols: do
        if (lhs_symbols(ls)%is_compatible(other=rhs_symbols(rs))) then
          lhs_symbols(ls) = lhs_symbols(ls) / rhs_symbols(rs)
          ! pop up current symbol from rhs symbols
          if (size(rhs_symbols, dim=1)>1) then
            if (rs==1) then
              rhs_symbols = rhs_symbols(rs+1:)
            elseif (rs==size(rhs_symbols, dim=1))  then
              rhs_symbols = rhs_symbols(1:rs-1)
            else
              rhs_symbols = [rhs_symbols(1:rs-1), rhs_symbols(rs+1:)]
            endif
            rs = 1
          else
            deallocate(rhs_symbols)
          endif
        else
          ! check the next rhs symbols
          rs = rs + 1
        endif
        if (rs>=size(rhs_symbols, dim=1).or.(.not.allocated(rhs_symbols))) exit remaining_rhs_symbols
      enddo remaining_rhs_symbols
    enddo
    opr%symbols = lhs_symbols
    if (allocated(rhs_symbols)) then
      ! there are still rhs symbols not compatible with lhs ones that must be added
      do rs=1, size(rhs_symbols, dim=1)
        call opr%add_symbol(symbol=rhs_symbols(rs)**(-1))
      enddo
    endif
    if (lhs%has_name().and.rhs%has_name()) then
      opr%name = lhs%name//'/'//rhs%name
    endif
  endif
  call opr%update_symbols_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic * unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs            !< Left hand side.
  class(unit_generic), intent(in) :: rhs            !< Right hand side.
  type(unit_generic)              :: opr            !< Operator result.
  type(unit_symbol), allocatable  :: lhs_symbols(:) !< Left hand side symbols.
  type(unit_symbol), allocatable  :: rhs_symbols(:) !< Right hand side symbols.
  integer(I_P)                    :: ls             !< Counter.
  integer(I_P)                    :: rs             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (lhs%are_symbols_defined().and.rhs%are_symbols_defined()) then
    lhs_symbols = lhs%symbols
    rhs_symbols = rhs%symbols
    do ls=1, size(lhs_symbols, dim=1)
      rs = 1
      remaining_rhs_symbols: do
        if (lhs_symbols(ls)%is_compatible(other=rhs_symbols(rs))) then
          lhs_symbols(ls) = lhs_symbols(ls) * rhs_symbols(rs)
          ! pop up current symbol from rhs symbols
          if (size(rhs_symbols, dim=1)>1) then
            if (rs==1) then
              rhs_symbols = rhs_symbols(rs+1:)
            elseif (rs==size(rhs_symbols, dim=1))  then
              rhs_symbols = rhs_symbols(1:rs-1)
            else
              rhs_symbols = [rhs_symbols(1:rs-1), rhs_symbols(rs+1:)]
            endif
            rs = 1
          else
            deallocate(rhs_symbols)
          endif
        else
          ! check the next rhs symbols
          rs = rs + 1
        endif
        if (rs>=size(rhs_symbols, dim=1).or.(.not.allocated(rhs_symbols))) exit remaining_rhs_symbols
      enddo remaining_rhs_symbols
    enddo
    opr%symbols = lhs_symbols
    if (allocated(rhs_symbols)) then
      ! there are still rhs symbols not compatible with lhs ones that must be added
      do rs=1, size(rhs_symbols, dim=1)
        call opr%add_symbol(symbol=rhs_symbols(rs))
      enddo
    endif
    if (lhs%has_name().and.rhs%has_name()) then
      opr%name = lhs%name//'*'//rhs%name
    endif
  endif
  call opr%update_symbols_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  function sub(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic - unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in)  :: lhs !< Left hand side.
  class(unit_generic), intent(in)  :: rhs !< Right hand side.
  class(unit_generic), allocatable :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs==rhs) then
    allocate(opr, source=lhs)
  else
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS - RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub

  function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs !< Left hand side.
  integer(I8P),        intent(in) :: rhs !< Right hand side.
  type(unit_generic)              :: opr !< Operator result.
  integer(I_P)                    :: s   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%are_symbols_defined()) then
    do s=1, size(opr%symbols, dim=1)
      opr%symbols(s) = opr%symbols(s) ** rhs
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs !< Left hand side.
  integer(I4P),        intent(in) :: rhs !< Right hand side.
  type(unit_generic)              :: opr !< Operator result.
  integer(I_P)                    :: s   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%are_symbols_defined()) then
    do s=1, size(opr%symbols, dim=1)
      opr%symbols(s) = opr%symbols(s) ** rhs
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs !< Left hand side.
  integer(I2P),        intent(in) :: rhs !< Right hand side.
  type(unit_generic)              :: opr !< Operator result.
  integer(I_P)                    :: s   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%are_symbols_defined()) then
    do s=1, size(opr%symbols, dim=1)
      opr%symbols(s) = opr%symbols(s) ** rhs
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in) :: lhs !< Left hand side.
  integer(I1P),        intent(in) :: rhs !< Right hand side.
  type(unit_generic)              :: opr !< Operator result.
  integer(I_P)                    :: s   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%are_symbols_defined()) then
    do s=1, size(opr%symbols, dim=1)
      opr%symbols(s) = opr%symbols(s) ** rhs
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_unit_generic
