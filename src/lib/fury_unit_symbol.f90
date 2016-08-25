!< FURY class definition of unit's symbol.
module fury_unit_symbol
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of unit's symbol.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: parse_unit_symbols
public :: unit_symbol
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: unit_symbol
  !< Unit's symbol class.
  !<
  !< Provide math operations on symbols necessary to build complex units.
  !<
  !< The symbol is constituted by 2 required and 1 optional members, namely the *litteral* symbol and its exponent and the
  !< (optional) dimension tag (that obviuosly has the same exponent of the symbol). These 3 members allow the definition of
  !< 3 different kind of comparisons:
  !<
  !<+ *compatibility*: 2 symbols are defined *compatible* if they have the same litteral symbol (and dimension if defined)
  !<                   unregarded their exponent values; this allows for the *compatible* math operation, e.g. `m2 => m3 / m`
  !<                   where `m3` and `m` are *compatible* in the sense that they can be
  !<                   divided/multiplied, whereas `m / s` is not a compatible operation rather a mixing rule that is handled
  !<                   by the [[unit]] class;
  !<+ *equality*: 2 symbols are defined *equals* if they have the same litteral symbol (and dimension if defined) and also the
  !<              same exponent value; this allows addition/subtraction operations handled by the [[unit]] class.
  !<+ *dimensionality*: 2 symbols are defined *equals in dimension* if they have dimension and also the same exponent value,
  !<                    unregarded their litteral symbols; this allows for units conversions;
  !<
  !< The string format definition of a valid FURY unit symbol is as following:
  !<
  !< `kg [mass]`
  !<
  !< where
  !<
  !<+ `kg` is the first mandatory term that defines the litteral symbol;
  !<+ `[mass]` is the second optional term that defines the symbol dimension;
  !<
  !< These 2 terms can be separated by any white spaces number (even zero), but the dimension must be enclosed into `[]` brackets.
  character(len=:), allocatable :: symbol                !< Litteral symbol, e.g. "m" for metres.
  integer(I_P)                  :: symbol_exponent=1_I_P !< Exponent of the symbol, e.g. "1" for metres, namely "m1".
  character(len=:), allocatable :: dimension             !< Dimensions of the symbol, e.g. "length" for metres.
  contains
    ! public methods
    procedure, pass(self) :: dimensionality           !< Return a string representation of the symbol dimensionality.
    procedure, pass(self) :: has_dimension            !< Check if the symbol dimension has been defined.
    procedure, pass(self) :: has_symbol               !< Check if the symbol has been defined.
    procedure, pass(self) :: is_compatible            !< Check if the symbol is compatible with another one.
    procedure, pass(self) :: is_dimension_equal       !< Check if the symbol dimension is equal with another one.
    procedure, pass(self) :: parse                    !< Parse symbol from string.
    procedure, pass(self) :: set                      !< Set symbol.
    procedure, pass(self) :: stringify                !< Return a string representaion of the symbol.
    procedure, pass(self) :: unset                    !< Unset symbol.
    ! public generic names
    generic :: assignment(=) => assign_string, &
                                assign_unit_symbol !< Overloading `=` assignament.
    generic :: operator(/) => div                  !< Overloading `/` operator.
    generic :: operator(*) => mul                  !< Overloading `*` operator.
    generic :: operator(**) => pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P    !< Overloading `**` operator.
    generic :: operator(==) => is_equal            !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal        !< Overloading `/=` operator.
    ! private methods
    procedure, pass(self), private :: is_equal           !< Check if the symbol is equal with another one.
    procedure, pass(self), private :: is_not_equal       !< Check if the symbol is not equal with another one.
    procedure, pass(lhs),  private :: assign_string      !< `unit_symbol = string` assignament.
    procedure, pass(lhs),  private :: assign_unit_symbol !< `unit_symbol = unit_symbol` assignament.
    procedure, pass(lhs),  private :: div                !< `unit_symbol / unit_symbol` operator.
    procedure, pass(lhs),  private :: mul                !< `unit_symbol * unit_symbol` operator.
    procedure, pass(lhs),  private :: pow_I8P            !< `unit_symbol ** integer(I8P)` operator.
    procedure, pass(lhs),  private :: pow_I4P            !< `unit_symbol ** integer(I4P)` operator.
    procedure, pass(lhs),  private :: pow_I2P            !< `unit_symbol ** integer(I2P)` operator.
    procedure, pass(lhs),  private :: pow_I1P            !< `unit_symbol ** integer(I1P)` operator.
endtype unit_symbol

interface unit_symbol
  !< Ovearloading [[unit_symbol]] name with a creator function.
  module procedure creator_from_string
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non type bound procedures
  function parse_unit_symbols(symbols) result(symbols_array)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse symbols string returning an array of (base) unit symbols.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in)       :: symbols          !< Litteral symbol(s) of the unit, e.g. "m.s-1" for metres/second.
  type(unit_symbol), allocatable :: symbols_array(:) !< Litteral symbol(s) of the unit, e.g. "m.s-1" for metres/seconds, array var.
  type(string)                   :: buffer           !< String buffer.
  type(string), allocatable      :: sym_tokens(:)    !< Symbols string tokens.
  integer(I_P)                   :: symbols_number   !< Symbols number.
  integer(I_P)                   :: t                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = trim(adjustl(symbols))
  call buffer%split(tokens=sym_tokens, sep='.')
  symbols_number = size(sym_tokens, dim=1)
  allocate(symbols_array(1:symbols_number))
  do t=1, symbols_number
    call symbols_array(t)%parse(source=sym_tokens(t)%chars())
  enddo
  ! reduce to unique symbols list
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction parse_unit_symbols

  ! private non type bound procedures
  function creator_from_string(source) result(symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit_symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: source !< Source input string definition of the symbol.
  type(unit_symbol)        :: symbol !< The symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call symbol%parse(source=source)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string

  ! public methods
  pure function dimensionality(self) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the symbol dimensionality.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self !< The symbol.
  character(len=:), allocatable  :: raw  !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%has_dimension()) then
    raw = self%dimension
    if (self%symbol_exponent<0) then
      raw = raw//trim(str(n=self%symbol_exponent))
    elseif (self%symbol_exponent/=1_I_P) then
      raw = raw//trim(str(n=self%symbol_exponent, no_sign=.true.))
    endif
  else
    raw = ''
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction dimensionality

  elemental function has_dimension(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbol dimension has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self       !< The symbol.
  logical                        :: is_defined !< Symbol dimension definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%dimension)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_dimension

  elemental function has_symbol(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbol has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self       !< The symbol.
  logical                        :: is_defined !< Symbol definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_symbol

  elemental function is_compatible(self, other) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbol is compatible with another one.
  !<
  !< Two symbols are defined *compatible* if they have the same litteral symbol, unregarded their exponent values.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self       !< The symbol.
  type(unit_symbol),  intent(in) :: other      !< The other symbol.
  logical                        :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  compatible = .false.
  if (self%has_symbol().and.other%has_symbol()) then
    compatible = (self%symbol==other%symbol)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  elemental function is_dimension_equal(self, other) result(equal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbol dimension is equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self  !< The symbol.
  type(unit_symbol),  intent(in) :: other !< The other symbol.
  logical                        :: equal !< Equality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  equal = .false.
  if (self%has_symbol().and.other%has_symbol().and.&
      self%has_dimension().and.other%has_dimension()) &
    equal = ((self%dimension==other%dimension).and.(self%symbol_exponent==other%symbol_exponent))
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_dimension_equal

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse symbol from string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(inout) :: self               !< The symbol.
  character(*),       intent(in)    :: source             !< Source input string definition of symbol.
  character(len=:), allocatable     :: buffer             !< String buffer.
  character(len=:), allocatable     :: dimension          !< Dimension buffer.
  integer(I_P)                      :: dimension_exponent !< Dimension exponent.
  integer(I_P)                      :: e                  !< Counter.
  integer(I_P)                      :: d(2)               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dimension_exponent = 0
  buffer = trim(adjustl(source))
  d(1) = index(string=buffer, substring='[')
  d(2) = index(string=buffer, substring=']')
  if ((d(1)>0).and.(d(2)>0).and.(d(2)>d(1)+1)) then
    dimension = trim(adjustl(buffer(d(1)+1:d(2)-1)))
    buffer = trim(adjustl(buffer(1:d(1)-1)))
    e = scan(string=dimension, set='-123456789')
    if (e>0) then
      self%dimension = trim(adjustl(dimension(1:e-1)))
      dimension_exponent = cton(str=dimension(e:), knd=1_I_P)
    else
      self%dimension = trim(adjustl(dimension))
    endif
  endif
  e = scan(string=buffer, set='-123456789')
  if (e>0) then
    self%symbol = trim(adjustl(buffer(1:e-1)))
    self%symbol_exponent = cton(str=buffer(e:), knd=1_I_P)
  else
    self%symbol = trim(adjustl(buffer))
  endif
  if (dimension_exponent/=0.and.dimension_exponent/=self%symbol_exponent) then
    write(stderr, '(A)')'error: parse string definition "'//trim(adjustl(source))//'" failed! '//&
    ' the exponent of the symbol and the one of the dimension (if passed) must be the same!'
    stop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  elemental subroutine set(self, symbol, symbol_exponent, dimension)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(inout)        :: self            !< The symbol.
  character(*),       intent(in), optional :: symbol          !< Litteral symbol of the unit, e.g. "m" for metres.
  integer(I_P),       intent(in), optional :: symbol_exponent !< Exponent of the symbol, e.g. "1" for metres, namely "m1".
  character(*),       intent(in), optional :: dimension       !< Dimensions of the symbol, e.g. "length" for metres.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(symbol)) self%symbol = symbol
  if (present(symbol_exponent)) self%symbol_exponent = symbol_exponent
  if (present(dimension)) self%dimension = dimension
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self, with_dimension) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in)           :: self           !< The symbol.
  logical,            intent(in), optional :: with_dimension !< Flag to activate dimension printing.
  character(len=:), allocatable            :: raw            !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%has_symbol()) then
    raw = self%symbol
    if (self%symbol_exponent<0) then
      raw = raw//trim(str(n=self%symbol_exponent))
    elseif (self%symbol_exponent/=1_I_P) then
      raw = raw//trim(str(n=self%symbol_exponent, no_sign=.true.))
    endif
    if (present(with_dimension)) then
      if (with_dimension) raw = raw//' ['//self%dimensionality()//']'
    endif
  else
    raw = ''
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(inout) :: self !< The symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%symbol)) deallocate(self%symbol)
  self%symbol_exponent = 1_I_P
  if (allocated(self%dimension)) deallocate(self%dimension)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  elemental function is_equal(self, other) result(equal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbol is equal with another one.
  !<
  !< Two symbols are defined *equal* if they have the same litteral symbol and the same exponent value.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self  !< The symbol.
  type(unit_symbol),  intent(in) :: other !< The other symbol.
  logical                        :: equal !< Equality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  equal = .false.
  if (self%has_symbol().and.other%has_symbol()) then
    equal = ((self%symbol==other%symbol).and.(self%symbol_exponent==other%symbol_exponent))
    if (equal.and.self%has_dimension().and.other%has_dimension()) equal = (self%dimension==other%dimension)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other) result(not_equal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the symbol is not equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: self      !< The symbol.
  type(unit_symbol),  intent(in) :: other     !< The other symbol.
  logical                        :: not_equal !< Disequality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  ! operators
  subroutine assign_string(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol = string` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(inout) :: lhs           !< Left hand side.
  character(*),       intent(in)    :: rhs           !< Right hand side.
  type(unit_symbol)                 :: parsed_symbol !< Symbol arising from string input.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call parsed_symbol%parse(source=rhs)
  if (parsed_symbol%has_symbol()) lhs = parsed_symbol
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_string

  pure subroutine assign_unit_symbol(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol = unit_symbol` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(inout) :: lhs !< Left hand side.
  type(unit_symbol),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%has_symbol())  then
    lhs%symbol = rhs%symbol
    lhs%symbol_exponent = rhs%symbol_exponent
    if (rhs%has_dimension()) lhs%dimension = rhs%dimension
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_unit_symbol

  pure function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol / unit_symbol` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: lhs !< Left hand side.
  type(unit_symbol),  intent(in) :: rhs !< Right hand side.
  type(unit_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_compatible(other=rhs)) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent - rhs%symbol_exponent
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  pure function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol * unit_symbol` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: lhs !< Left hand side.
  type(unit_symbol),  intent(in) :: rhs !< Right hand side.
  type(unit_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_compatible(other=rhs)) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent + rhs%symbol_exponent
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  pure function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: lhs !< Left hand side.
  integer(I8P),       intent(in) :: rhs !< Right hand side.
  type(unit_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  pure function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: lhs !< Left hand side.
  integer(I4P),       intent(in) :: rhs !< Right hand side.
  type(unit_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  pure function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: lhs !< Left hand side.
  integer(I2P),       intent(in) :: rhs !< Right hand side.
  type(unit_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  pure function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_symbol ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_symbol), intent(in) :: lhs !< Left hand side.
  integer(I1P),       intent(in) :: rhs !< Right hand side.
  type(unit_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_unit_symbol
