!< FURY, definition of operators for mixed kinds math.
module fury_mixed_kinds
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, definition of operators for mixed kinds math.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_qreal32, qreal32 => qreal
use fury_qreal64, qreal64 => qreal
! use fury_system_abstract32, system_abstract32 => system_abstract
! use fury_system_abstract64, system_abstract64 => system_abstract
! use fury_system_si32, system_si32 => system_si
! use fury_system_si64, system_si64 => system_si
use fury_uom32, uom32 => uom
use fury_uom64, uom64 => uom
use fury_uom_converter32, converter32 => converter
use fury_uom_converter64, converter64 => converter
use fury_uom_reference32, uom_reference32 => uom_reference
use fury_uom_reference64, uom_reference64 => uom_reference
use fury_uom_symbol32, uom_symbol32 => uom_symbol
use fury_uom_symbol64, uom_symbol64 => uom_symbol
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: assignment(=)
!-----------------------------------------------------------------------------------------------------------------------------------

interface assignment(=)
  module procedure qreal32_assign_qreal64, qreal64_assign_qreal32
  module procedure uom32_assign_uom64, uom64_assign_uom32
  module procedure uom_reference32_assign_uom_reference64, uom_reference64_assign_uom_reference32
  module procedure uom_symbol32_assign_uom_symbol64, uom_symbol64_assign_uom_symbol32
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine qreal32_assign_qreal64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 = qreal64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal32), intent(inout) :: lhs !< Left hand side.
  class(qreal64), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_unit_defined()) then
    lhs%magnitude = rhs%magnitude
    if (allocated(rhs%name)) lhs%name = rhs%name
    if (.not.lhs%is_unit_defined()) then
      call lhs%allocate_unit
      lhs%unit = rhs%unit
    else
      write(stderr, "(A)") 'error: cannot assign between "'//&
                           lhs%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true., compact_reals=.true.)//&
                           '" and "'//&
                           rhs%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true., compact_reals=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine qreal32_assign_qreal64

  subroutine qreal64_assign_qreal32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 = qreal32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal64), intent(inout) :: lhs !< Left hand side.
  class(qreal32), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_unit_defined()) then
    lhs%magnitude = rhs%magnitude
    if (allocated(rhs%name)) lhs%name = rhs%name
    if (.not.lhs%is_unit_defined()) then
      call lhs%allocate_unit
      lhs%unit = rhs%unit
    else
      write(stderr, "(A)") 'error: cannot assign between "'//&
                           lhs%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true., compact_reals=.true.)//&
                           '" and "'//&
                           rhs%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true., compact_reals=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine qreal64_assign_qreal32

  subroutine uom32_assign_uom64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom32 = uom64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom32), intent(inout)        :: lhs               !< Left hand side.
  class(uom64), intent(in)           :: rhs               !< Right hand side.
  type(uom_reference64), allocatable :: rhs_references(:) !< RHS references.
  type(uom_reference64)              :: rhs_alias         !< RHS alias.
  type(uom_reference32), allocatable :: lhs_references(:) !< LHS references.
  type(uom_reference32)              :: lhs_alias         !< LHS alias.
  integer(I_P)                       :: r                 !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      if (allocated(rhs%name)) call lhs%set(name=rhs%name)
      call rhs%get_references(references=rhs_references)
      call rhs%get_alias(alias=rhs_alias)
      if (allocated(rhs_references)) then
        allocate(lhs_references(1:size(rhs_references, dim=1)))
        do r=1, size(rhs_references, dim=1)
          lhs_references(r) = rhs_references(r)
        enddo
        call lhs%set(references=lhs_references)
      endif
      if (rhs_alias%is_defined()) then
        lhs_alias = rhs_alias
        call lhs%set(alias=lhs_alias)
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//&
                           lhs%stringify(with_dimensions=.true., with_aliases=.true.)//'" and "'//&
                           rhs%stringify(with_dimensions=.true., with_aliases=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom32_assign_uom64

  subroutine uom64_assign_uom32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom64 = uom32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom64), intent(inout)        :: lhs               !< Left hand side.
  class(uom32), intent(in)           :: rhs               !< Right hand side.
  type(uom_reference32), allocatable :: rhs_references(:) !< RHS references.
  type(uom_reference32)              :: rhs_alias         !< RHS alias.
  type(uom_reference64), allocatable :: lhs_references(:) !< LHS references.
  type(uom_reference64)              :: lhs_alias         !< LHS alias.
  integer(I_P)                       :: r                 !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      if (allocated(rhs%name)) call lhs%set(name=rhs%name)
      call rhs%get_references(references=rhs_references)
      call rhs%get_alias(alias=rhs_alias)
      if (allocated(rhs_references)) then
        allocate(lhs_references(1:size(rhs_references, dim=1)))
        do r=1, size(rhs_references, dim=1)
          lhs_references(r) = rhs_references(r)
        enddo
        call lhs%set(references=lhs_references)
      endif
      if (rhs_alias%is_defined()) then
        lhs_alias = rhs_alias
        call lhs%set(alias=lhs_alias)
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//&
                           lhs%stringify(with_dimensions=.true., with_aliases=.true.)//'" and "'//&
                           rhs%stringify(with_dimensions=.true., with_aliases=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom64_assign_uom32

  subroutine uom_reference32_assign_uom_reference64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference32 = uom_reference64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference32), intent(inout) :: lhs            !< Left hand side.
  type(uom_reference64), intent(in)     :: rhs            !< Right hand side.
  type(uom_symbol64), allocatable       :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol64)                    :: rhs_dimensions !< RHS dimensions.
  type(uom_symbol32), allocatable       :: lhs_aliases(:) !< LHS uom symbol aliases.
  type(uom_symbol32)                    :: lhs_dimensions !< LHS dimensions.
  integer(I_P)                          :: a              !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      call rhs%get_aliases(aliases=rhs_aliases)
      call rhs%get_dimensions(dimensions=rhs_dimensions)
      if (allocated(rhs_aliases)) then
        allocate(lhs_aliases(1:size(rhs_aliases, dim=1)))
        do a=1, size(rhs_aliases, dim=1)
          lhs_aliases(a) = rhs_aliases(a)
        enddo
        call lhs%set(aliases=lhs_aliases)
      endif
      if (rhs_dimensions%is_defined()) then
        lhs_dimensions = rhs_dimensions
        call lhs%set(dimensions=lhs_dimensions)
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//&
                           lhs%stringify(with_dimensions=.true., with_aliases=.true.)//'" and "'//&
                           rhs%stringify(with_dimensions=.true., with_aliases=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_reference32_assign_uom_reference64

  subroutine uom_reference64_assign_uom_reference32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference64 = uom_reference32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference64), intent(inout) :: lhs            !< Left hand side.
  type(uom_reference32), intent(in)     :: rhs            !< Right hand side.
  type(uom_symbol32), allocatable       :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol32)                    :: rhs_dimensions !< RHS dimensions.
  type(uom_symbol64), allocatable       :: lhs_aliases(:) !< LHS uom symbol aliases.
  type(uom_symbol64)                    :: lhs_dimensions !< LHS dimensions.
  integer(I_P)                          :: a              !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      call rhs%get_aliases(aliases=rhs_aliases)
      call rhs%get_dimensions(dimensions=rhs_dimensions)
      if (allocated(rhs_aliases)) then
        allocate(lhs_aliases(1:size(rhs_aliases, dim=1)))
        do a=1, size(rhs_aliases, dim=1)
          lhs_aliases(a) = rhs_aliases(a)
        enddo
        call lhs%set(aliases=lhs_aliases)
      endif
      if (rhs_dimensions%is_defined()) then
        lhs_dimensions = rhs_dimensions
        call lhs%set(dimensions=lhs_dimensions)
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//&
                           lhs%stringify(with_dimensions=.true., with_aliases=.true.)//'" and "'//&
                           rhs%stringify(with_dimensions=.true., with_aliases=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_reference64_assign_uom_reference32

  subroutine uom_symbol32_assign_uom_symbol64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol32 = uom_symbol64` assignment.
  !<
  !< @TODO Handle convert_
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol32), intent(inout) :: lhs       !< Left hand side.
  class(uom_symbol64), intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                       :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R8P)                          :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R8P)                          :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                       :: symbol_   !< literal symbol, e.g. "m" for metres.
  ! class(converter64), allocatable    :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      ! convert_ = rhs%get_convert()
      call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R4P), offset_=real(offset_, R4P))
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol32_assign_uom_symbol64

  subroutine uom_symbol64_assign_uom_symbol32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol64 = uom_symbol32` assignment.
  !<
  !< @TODO Handle convert_
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol64), intent(inout) :: lhs       !< Left hand side.
  class(uom_symbol32), intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                       :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R4P)                          :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R4P)                          :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                       :: symbol_   !< literal symbol, e.g. "m" for metres.
  ! class(converter64), allocatable    :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      ! convert_ = rhs%get_convert()
      call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R8P), offset_=real(offset_, R8P))
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol64_assign_uom_symbol32
endmodule fury_mixed_kinds
