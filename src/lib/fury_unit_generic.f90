!< FURY definition of *generic* unit class.
module fury_unit_generic
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of *generic* unit class.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_unit_symbol
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_generic
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: unit_generic
  !< Generic prototype of *unit*.
  type(unit_symbol), allocatable :: symbols(:) !< Symbol(s) of the unit.
  character(len=:),  allocatable :: name       !< Unit name.
  contains
    ! public methods
    procedure, pass(self) :: add_symbol          !< Add a symbol to unit.
    procedure, pass(self) :: add_symbols         !< Add symbols to unit.
    procedure, pass(self) :: are_symbols_defined !< Check if the symbols have been defined.
    procedure, pass(self) :: has_name            !< Check if the unit has a name.
    procedure, pass(self) :: has_symbol          !< Check if the unit has a symbol.
    procedure, pass(self) :: is_compatible       !< Check if unit is compatible with another one.
    procedure, pass(self) :: is_equal            !< Check if unit is equal with another one.
    procedure, pass(self) :: set                 !< Set the unit.
    procedure, pass(self) :: stringify           !< Return a string representaion of the unit.
    procedure, pass(self) :: unset               !< Set the unit.
    ! public generic names
    generic :: assignment(=) => assign_unit_generic !< Overloading `=` operator.
    generic :: operator(+) => add                   !< Overloading `+` operator.
    generic :: operator(/) => div                   !< Overloading `/` operator.
    generic :: operator(*) => mul                   !< Overloading `*` operator.
    generic :: operator(-) => sub                   !< Overloading `-` operator.
    ! private methods
    procedure, pass(lhs), private :: assign_unit_generic !< `unit_generic = unit_generic` assignament.
    procedure, pass(lhs), private :: add                 !< `unit_generic + unit_generic` operator.
    procedure, pass(lhs), private :: div                 !< `unit_generic / unit_generic` operator.
    procedure, pass(lhs), private :: mul                 !< `unit_generic * unit_generic` operator.
    procedure, pass(lhs), private :: sub                 !< `unit_generic - unit_generic` operator.
endtype unit_generic

interface unit_generic
  !< Ovearloading [[unit_generic]] name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator(symbols, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in), optional :: symbols !< Symbol(s) of the unit.
  character(*), intent(in), optional :: name    !< Unit name.
  type(unit_generic)                 :: unit    !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(symbols=symbols, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

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
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_disequality

  ! public methods
  subroutine add_symbol(self, symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add symbols to unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self           !< The unit.
  type(unit_symbol),   intent(in)    :: symbol         !< Unit symbol.
  type(unit_symbol), allocatable     :: symbols(:)     !< Litteral symbol(s), e.g. "m.s-1" for metres/seconds, array var.
  integer(I_P)                       :: symbols_number !< Symbols number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%are_symbols_defined()) then
    symbols_number = size(self%symbols, dim=1)
    allocate(symbols(symbols_number+1))
    symbols(1:symbols_number) = self%symbols
    symbols(symbols_number+1) = symbol
    call move_alloc(from=symbols, to=self%symbols)
  else
    allocate(self%symbols(1))
    self%symbols(1) = symbol
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_symbol

  subroutine add_symbols(self, symbols)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add symbols to unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout) :: self             !< The unit.
  character(*),        intent(in)    :: symbols          !< Litteral symbol(s), e.g. "m.s-1" for metres/second.
  type(unit_symbol), allocatable     :: symbols_array(:) !< Litteral symbol(s), e.g. "m.s-1" for metres/seconds, array var.
  integer(I_P)                       :: s                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%are_symbols_defined()) then
    symbols_array = parse_unit_symbols(symbols=symbols)
    do s=1, size(symbols_array, dim=1)
      if (.not.self%has_symbol(symbol=symbols_array(s))) call self%add_symbol(symbol=symbols_array(s))
    enddo
  else
    self%symbols = parse_unit_symbols(symbols=symbols)
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
    do s=1, size(self%symbols, dim=1)
      has_symbol = self%symbols(s)%is_equal(other=symbol)
      if (has_symbol) exit
    enddo
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
  is_equal = self%is_compatible(other=other)
  if (is_equal.and.self%are_symbols_defined().and.other%are_symbols_defined()) then
    is_equal = (size(self%symbols, dim=1)==size(other%symbols, dim=1))
    if (is_equal) then
      do s=1, size(self%symbols, dim=1)
        is_equal = other%has_symbol(symbol=self%symbols(s))
        if (.not.is_equal) exit
      enddo
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  subroutine set(self, symbols, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout)         :: self    !< The unit.
  character(*),        intent(in),  optional :: symbols !< Symbol(s) of the unit.
  character(*),        intent(in),  optional :: name    !< Unit name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(symbols)) call self%add_symbols(symbols=symbols)
  if (present(name)) self%name = name
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self, with_dimensions) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in)           :: self            !< The unit.
  logical,             intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  character(len=:), allocatable             :: raw             !< Raw characters data.
  integer(I_P)                              :: s               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%are_symbols_defined()) then
    raw = ''
    do s=1, size(self%symbols, dim=1)
      raw = raw//'.'//self%symbols(s)%stringify()
    enddo
    raw = raw(2:)
    if (present(with_dimensions)) then
      if (with_dimensions) then
        raw = raw//' ['
        do s=1, size(self%symbols, dim=1)
          raw = raw//self%symbols(s)%dimensionality()//'.'
        enddo
        raw(len(raw):len(raw)) = ']'
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
  if (allocated(self%name)) deallocate(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
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
      if (allocated(rhs%name)) lhs%name = rhs%name
    else
      if (.not.lhs%is_equal(other=rhs)) call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS = RHS')
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_unit_generic

  function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic + unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in)  :: lhs !< Left hand side.
  class(unit_generic), intent(in)  :: rhs !< Right hand side.
  class(unit_generic), allocatable :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_equal(other=rhs)) then
    allocate(opr, source=lhs)
  else
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS + RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic / unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in)  :: lhs            !< Left hand side.
  class(unit_generic), intent(in)  :: rhs            !< Right hand side.
  class(unit_generic), allocatable :: opr            !< Operator result.
  type(unit_symbol), allocatable   :: lhs_symbols(:) !< Left hand side symbols.
  type(unit_symbol), allocatable   :: rhs_symbols(:) !< Right hand side symbols.
  integer(I_P)                     :: ls             !< Counter.
  integer(I_P)                     :: rs             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(opr, source=lhs)
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
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `unit_generic * unit_generic` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(in)  :: lhs            !< Left hand side.
  class(unit_generic), intent(in)  :: rhs            !< Right hand side.
  class(unit_generic), allocatable :: opr            !< Operator result.
  type(unit_symbol), allocatable   :: lhs_symbols(:) !< Left hand side symbols.
  type(unit_symbol), allocatable   :: rhs_symbols(:) !< Right hand side symbols.
  integer(I_P)                     :: ls             !< Counter.
  integer(I_P)                     :: rs             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(opr, source=lhs)
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
  if (lhs%is_equal(other=rhs)) then
    allocate(opr, source=lhs)
  else
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS - RHS')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_unit_generic
