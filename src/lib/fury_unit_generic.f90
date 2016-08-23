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
  real(R_P)                      :: scale_factor=1._R_P !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  type(unit_symbol), allocatable :: symbols(:)          !< Litteral symbol(s) of the unit, e.g. "m.s-1" for metres/seconds.
  character(len=:),  allocatable :: alias               !< Alias symbol, e.g. "N" for Netown that is "kg.m.s-2".
  contains
    ! public methods
    procedure, pass(self) :: add_symbol          !< Add a symbol to unit.
    procedure, pass(self) :: add_symbols         !< Add symbols to unit.
    procedure, pass(self) :: are_symbols_defined !< Check if the symbols have been defined.
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
  !< Ovearloading `unit_generic` name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator(scale_factor, symbols, alias) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),    intent(in), optional :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*), intent(in), optional :: symbols      !< Litteral symbol(s) of the unit, e.g. "m.s-1" for metres/seconds.
  character(*), intent(in), optional :: alias        !< Alias symbol, e.g. "N" for Netown that is "kg.m.s-2".
  type(unit_generic)                 :: unit         !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(scale_factor=scale_factor, symbols=symbols, alias=alias)
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
  write(stderr, '(A)')'  LHS: '//lhs%stringify()
  write(stderr, '(A)')'  RHS: '//rhs%stringify()
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
  write(stderr, '(A)')'  LHS: '//lhs%stringify()
  write(stderr, '(A)')'  RHS: '//rhs%stringify()
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

  subroutine set(self, scale_factor, symbols, alias, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_generic), intent(inout)         :: self         !< The unit.
  real(R_P),           intent(in),  optional :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*),        intent(in),  optional :: symbols      !< Litteral symbol(s) of the unit, e.g. "m.s-1" for metres/second.
  character(*),        intent(in),  optional :: alias        !< Alias symbol, e.g. "N" for Netown that is "kg.m.s-2".
  integer(I_P),        intent(out), optional :: error        !< Error code, 0 => no errors happen.
  integer(I_P)                               :: error_       !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 1
  if (present(scale_factor)) self%scale_factor = scale_factor
  if (present(symbols)) call self%add_symbols(symbols=symbols)
  if (present(alias)) self%alias = alias
  error_ = 0
  if (present(error)) error = error_
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
      lhs%scale_factor = rhs%scale_factor
      lhs%symbols = rhs%symbols
      if (allocated(rhs%alias)) lhs%alias = rhs%alias
    else
      if (.not.lhs%is_equal(other=rhs)) call raise_error_disequality(lhs=lhs, rhs=rhs, operation='L = R')
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
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='L + R')
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
      remaining_denominator: do
        if (lhs_symbols(ls)%is_compatible(other=rhs_symbols(rs))) then
          lhs_symbols(ls) = lhs_symbols(ls) / rhs_symbols(rs)
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
          rs = rs + 1
        endif
        if (rs>=size(rhs_symbols, dim=1).or.(.not.allocated(rhs_symbols))) exit
      enddo remaining_denominator
    enddo
    opr%symbols = lhs_symbols
    if (allocated(rhs_symbols)) then
      do rs=1, size(rhs_symbols, dim=1)
        call opr%add_symbol(symbol=rhs_symbols(rs)**(-1))
      enddo
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
      remaining_denominator: do
        if (lhs_symbols(ls)%is_compatible(other=rhs_symbols(rs))) then
          lhs_symbols(ls) = lhs_symbols(ls) * rhs_symbols(rs)
          if (rs==1) then
            rhs_symbols = rhs_symbols(rs+1:)
          elseif (rs==size(rhs_symbols, dim=1))  then
            rhs_symbols = rhs_symbols(1:rs-1)
          else
            rhs_symbols = [rhs_symbols(1:rs-1), rhs_symbols(rs+1:)]
          endif
          rs = 1
        else
          rs = rs + 1
        endif
        if (rs==size(rhs_symbols, dim=1)) exit
      enddo remaining_denominator
    enddo
    opr%symbols = lhs_symbols
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
    call raise_error_disequality(lhs=lhs, rhs=rhs, operation='L - R')
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_unit_generic
