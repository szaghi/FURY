!< FURY, definition of operators for mixed kinds math.
module fury_mixed_kinds
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY, definition of operators for mixed kinds math.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_qreal32, qreal32 => qreal
use fury_qreal64, qreal64 => qreal
use fury_qreal128, qreal128 => qreal
use fury_uom32, uom32 => uom
use fury_uom64, uom64 => uom
use fury_uom128, uom128 => uom
use fury_uom_converter
use fury_uom_reference32, uom_reference32 => uom_reference
use fury_uom_reference64, uom_reference64 => uom_reference
use fury_uom_reference128, uom_reference128 => uom_reference
use fury_uom_symbol32, uom_symbol32 => uom_symbol
use fury_uom_symbol64, uom_symbol64 => uom_symbol
use fury_uom_symbol128, uom_symbol128 => uom_symbol
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: assignment(=)
public :: operator(+)
public :: operator(/)
public :: operator(*)
public :: operator(-)
public :: operator(==)
public :: operator(/=)
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface assignment(=)
  ! qreal
  module procedure qreal32_assign_qreal64, qreal64_assign_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_assign_qreal128, qreal128_assign_qreal32
  module procedure qreal64_assign_qreal128, qreal128_assign_qreal64
#endif
  ! uom
  module procedure uom32_assign_uom64, uom64_assign_uom32
#ifdef _R16P_SUPPORTED
  module procedure uom32_assign_uom128, uom128_assign_uom32
  module procedure uom64_assign_uom128, uom128_assign_uom64
#endif
  ! uom_reference
  module procedure uom_reference32_assign_uom_reference64, uom_reference64_assign_uom_reference32
#ifdef _R16P_SUPPORTED
  module procedure uom_reference32_assign_uom_reference128, uom_reference128_assign_uom_reference32
  module procedure uom_reference64_assign_uom_reference128, uom_reference128_assign_uom_reference64
#endif
  ! uom_symbol
  module procedure uom_symbol32_assign_uom_symbol64, uom_symbol64_assign_uom_symbol32
#ifdef _R16P_SUPPORTED
  module procedure uom_symbol32_assign_uom_symbol128, uom_symbol128_assign_uom_symbol32
  module procedure uom_symbol64_assign_uom_symbol128, uom_symbol128_assign_uom_symbol64
#endif
endinterface

interface operator(+)
  module procedure qreal32_add_qreal64, qreal64_add_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_add_qreal128, qreal128_add_qreal32
  module procedure qreal64_add_qreal128, qreal128_add_qreal64
#endif
endinterface

interface operator(/)
  module procedure qreal32_div_qreal64, qreal64_div_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_div_qreal128, qreal128_div_qreal32
  module procedure qreal64_div_qreal128, qreal128_div_qreal64
#endif
endinterface

interface operator(*)
  module procedure qreal32_mul_qreal64, qreal64_mul_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_mul_qreal128, qreal128_mul_qreal32
  module procedure qreal64_mul_qreal128, qreal128_mul_qreal64
#endif
endinterface

interface operator(-)
  module procedure qreal32_sub_qreal64, qreal64_sub_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_sub_qreal128, qreal128_sub_qreal32
  module procedure qreal64_sub_qreal128, qreal128_sub_qreal64
#endif
endinterface

interface operator(==)
  module procedure qreal32_eq_qreal64, qreal64_eq_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_eq_qreal128, qreal128_eq_qreal32
  module procedure qreal64_eq_qreal128, qreal128_eq_qreal64
#endif
endinterface

interface operator(/=)
  module procedure qreal32_not_eq_qreal64, qreal64_not_eq_qreal32
#ifdef _R16P_SUPPORTED
  module procedure qreal32_not_eq_qreal128, qreal128_not_eq_qreal32
  module procedure qreal64_not_eq_qreal128, qreal128_not_eq_qreal64
#endif
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! assignment(=)
  subroutine qreal32_assign_qreal64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 = qreal64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(inout) :: lhs !< Left hand side.
  type(qreal64), intent(in)    :: rhs !< Right hand side.
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
  type(qreal64), intent(inout) :: lhs !< Left hand side.
  type(qreal32), intent(in)    :: rhs !< Right hand side.
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

  subroutine qreal32_assign_qreal128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 = qreal128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(inout) :: lhs !< Left hand side.
  type(qreal128), intent(in)    :: rhs !< Right hand side.
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
  endsubroutine qreal32_assign_qreal128

  subroutine qreal128_assign_qreal32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 = qreal32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(inout) :: lhs !< Left hand side.
  type(qreal32),  intent(in)    :: rhs !< Right hand side.
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
  endsubroutine qreal128_assign_qreal32

  subroutine qreal64_assign_qreal128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 = qreal128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(inout) :: lhs !< Left hand side.
  type(qreal128), intent(in)    :: rhs !< Right hand side.
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
  endsubroutine qreal64_assign_qreal128

  subroutine qreal128_assign_qreal64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 = qreal64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(inout) :: lhs !< Left hand side.
  type(qreal64),  intent(in)    :: rhs !< Right hand side.
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
  endsubroutine qreal128_assign_qreal64

  subroutine uom32_assign_uom64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom32 = uom64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom32), intent(inout)         :: lhs               !< Left hand side.
  type(uom64), intent(in)            :: rhs               !< Right hand side.
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
  type(uom64), intent(inout)         :: lhs               !< Left hand side.
  type(uom32), intent(in)            :: rhs               !< Right hand side.
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

  subroutine uom32_assign_uom128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom32 = uom128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom32),  intent(inout)         :: lhs               !< Left hand side.
  type(uom128), intent(in)            :: rhs               !< Right hand side.
  type(uom_reference128), allocatable :: rhs_references(:) !< RHS references.
  type(uom_reference128)              :: rhs_alias         !< RHS alias.
  type(uom_reference32), allocatable  :: lhs_references(:) !< LHS references.
  type(uom_reference32)               :: lhs_alias         !< LHS alias.
  integer(I_P)                        :: r                 !< Counter.
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
  endsubroutine uom32_assign_uom128

  subroutine uom128_assign_uom32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom128 = uom32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom128), intent(inout)         :: lhs               !< Left hand side.
  type(uom32),  intent(in)            :: rhs               !< Right hand side.
  type(uom_reference32), allocatable  :: rhs_references(:) !< RHS references.
  type(uom_reference32)               :: rhs_alias         !< RHS alias.
  type(uom_reference128), allocatable :: lhs_references(:) !< LHS references.
  type(uom_reference128)              :: lhs_alias         !< LHS alias.
  integer(I_P)                        :: r                 !< Counter.
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
  endsubroutine uom128_assign_uom32

  subroutine uom64_assign_uom128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom64 = uom128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom64),  intent(inout)         :: lhs               !< Left hand side.
  type(uom128), intent(in)            :: rhs               !< Right hand side.
  type(uom_reference128), allocatable :: rhs_references(:) !< RHS references.
  type(uom_reference128)              :: rhs_alias         !< RHS alias.
  type(uom_reference64), allocatable  :: lhs_references(:) !< LHS references.
  type(uom_reference64)               :: lhs_alias         !< LHS alias.
  integer(I_P)                        :: r                 !< Counter.
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
  endsubroutine uom64_assign_uom128

  subroutine uom128_assign_uom64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom128 = uom64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom128), intent(inout)         :: lhs               !< Left hand side.
  type(uom64),  intent(in)            :: rhs               !< Right hand side.
  type(uom_reference64), allocatable  :: rhs_references(:) !< RHS references.
  type(uom_reference64)               :: rhs_alias         !< RHS alias.
  type(uom_reference128), allocatable :: lhs_references(:) !< LHS references.
  type(uom_reference128)              :: lhs_alias         !< LHS alias.
  integer(I_P)                        :: r                 !< Counter.
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
  endsubroutine uom128_assign_uom64

  subroutine uom_reference32_assign_uom_reference64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference32 = uom_reference64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference32), intent(inout) :: lhs            !< Left hand side.
  type(uom_reference64), intent(in)    :: rhs            !< Right hand side.
  type(uom_symbol64), allocatable      :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol64)                   :: rhs_dimensions !< RHS dimensions.
  type(uom_symbol32), allocatable      :: lhs_aliases(:) !< LHS uom symbol aliases.
  type(uom_symbol32)                   :: lhs_dimensions !< LHS dimensions.
  integer(I_P)                         :: a              !< Counter.
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
  type(uom_reference64), intent(inout) :: lhs            !< Left hand side.
  type(uom_reference32), intent(in)    :: rhs            !< Right hand side.
  type(uom_symbol32), allocatable      :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol32)                   :: rhs_dimensions !< RHS dimensions.
  type(uom_symbol64), allocatable      :: lhs_aliases(:) !< LHS uom symbol aliases.
  type(uom_symbol64)                   :: lhs_dimensions !< LHS dimensions.
  integer(I_P)                         :: a              !< Counter.
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

  subroutine uom_reference32_assign_uom_reference128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference32 = uom_reference128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference32),  intent(inout) :: lhs            !< Left hand side.
  type(uom_reference128), intent(in)    :: rhs            !< Right hand side.
  type(uom_symbol128), allocatable      :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol128)                   :: rhs_dimensions !< RHS dimensions.
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
  endsubroutine uom_reference32_assign_uom_reference128

  subroutine uom_reference128_assign_uom_reference32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference128 = uom_reference32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference128), intent(inout) :: lhs            !< Left hand side.
  type(uom_reference32),  intent(in)    :: rhs            !< Right hand side.
  type(uom_symbol32), allocatable       :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol32)                    :: rhs_dimensions !< RHS dimensions.
  type(uom_symbol128), allocatable      :: lhs_aliases(:) !< LHS uom symbol aliases.
  type(uom_symbol128)                   :: lhs_dimensions !< LHS dimensions.
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
  endsubroutine uom_reference128_assign_uom_reference32

  subroutine uom_reference64_assign_uom_reference128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference64 = uom_reference128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference64),  intent(inout) :: lhs            !< Left hand side.
  type(uom_reference128), intent(in)    :: rhs            !< Right hand side.
  type(uom_symbol128), allocatable      :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol128)                   :: rhs_dimensions !< RHS dimensions.
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
  endsubroutine uom_reference64_assign_uom_reference128

  subroutine uom_reference128_assign_uom_reference64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference128 = uom_reference64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference128), intent(inout) :: lhs            !< Left hand side.
  type(uom_reference64),  intent(in)    :: rhs            !< Right hand side.
  type(uom_symbol64), allocatable       :: rhs_aliases(:) !< RHS uom symbol aliases.
  type(uom_symbol64)                    :: rhs_dimensions !< RHS dimensions.
  type(uom_symbol128), allocatable      :: lhs_aliases(:) !< LHS uom symbol aliases.
  type(uom_symbol128)                   :: lhs_dimensions !< LHS dimensions.
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
  endsubroutine uom_reference128_assign_uom_reference64

  subroutine uom_symbol32_assign_uom_symbol64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol32 = uom_symbol64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_symbol32), intent(inout) :: lhs       !< Left hand side.
  type(uom_symbol64), intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                      :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R8P)                         :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R8P)                         :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                      :: symbol_   !< literal symbol, e.g. "m" for metres.
  class(uom_converter), allocatable :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      call rhs%get_convert(convert_=convert_)
      if (allocated(convert_)) then
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_,           &
                     factor_=real(factor_, R4P), offset_=real(offset_, R4P), &
                     convert_=convert_)
      else
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R4P), offset_=real(offset_, R4P))
      endif
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
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_symbol64), intent(inout) :: lhs       !< Left hand side.
  type(uom_symbol32), intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                      :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R4P)                         :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R4P)                         :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                      :: symbol_   !< literal symbol, e.g. "m" for metres.
  class(uom_converter), allocatable :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      call rhs%get_convert(convert_=convert_)
      if (allocated(convert_)) then
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_,           &
                     factor_=real(factor_, R8P), offset_=real(offset_, R8P), &
                     convert_=convert_)
      else
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R8P), offset_=real(offset_, R8P))
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol64_assign_uom_symbol32

  subroutine uom_symbol32_assign_uom_symbol128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol32 = uom_symbol128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_symbol32),  intent(inout) :: lhs       !< Left hand side.
  type(uom_symbol128), intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                       :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R16P)                         :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R16P)                         :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                       :: symbol_   !< literal symbol, e.g. "m" for metres.
  class(uom_converter), allocatable  :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      call rhs%get_convert(convert_=convert_)
      if (allocated(convert_)) then
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_,           &
                     factor_=real(factor_, R4P), offset_=real(offset_, R4P), &
                     convert_=convert_)
      else
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R4P), offset_=real(offset_, R4P))
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol32_assign_uom_symbol128

  subroutine uom_symbol128_assign_uom_symbol32(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol128 = uom_symbol32` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_symbol128), intent(inout) :: lhs       !< Left hand side.
  type(uom_symbol32),  intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                       :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R4P)                          :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R4P)                          :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                       :: symbol_   !< literal symbol, e.g. "m" for metres.
  class(uom_converter), allocatable  :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      call rhs%get_convert(convert_=convert_)
      if (allocated(convert_)) then
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_,             &
                     factor_=real(factor_, R16P), offset_=real(offset_, R16P), &
                     convert_=convert_)
      else
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R16P), offset_=real(offset_, R16P))
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol128_assign_uom_symbol32

  subroutine uom_symbol64_assign_uom_symbol128(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol64 = uom_symbol128` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_symbol64),  intent(inout) :: lhs       !< Left hand side.
  type(uom_symbol128), intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                       :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R16P)                         :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R16P)                         :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                       :: symbol_   !< literal symbol, e.g. "m" for metres.
  class(uom_converter), allocatable  :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      call rhs%get_convert(convert_=convert_)
      if (allocated(convert_)) then
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_,           &
                     factor_=real(factor_, R8P), offset_=real(offset_, R8P), &
                     convert_=convert_)
      else
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R8P), offset_=real(offset_, R8P))
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol64_assign_uom_symbol128

  subroutine uom_symbol128_assign_uom_symbol64(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol128 = uom_symbol64` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_symbol128), intent(inout) :: lhs       !< Left hand side.
  type(uom_symbol64),  intent(in)    :: rhs       !< Right hand side.
  integer(I_P)                       :: exponent_ !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(R8P)                          :: factor_   !< Symbol multiplicative scale factor (used only for converters).
  real(R8P)                          :: offset_   !< Symbol additive offset (used only for converters).
  type(string)                       :: symbol_   !< literal symbol, e.g. "m" for metres.
  class(uom_converter), allocatable  :: convert_  !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      exponent_ = rhs%get_exponent()
      factor_ = rhs%get_factor()
      offset_ = rhs%get_offset()
      symbol_ = rhs%get_symbol()
      call rhs%get_convert(convert_=convert_)
      if (allocated(convert_)) then
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_,             &
                     factor_=real(factor_, R16P), offset_=real(offset_, R16P), &
                     convert_=convert_)
      else
        call lhs%set(symbol_=symbol_%chars(), exponent_=exponent_, factor_=real(factor_, R16P), offset_=real(offset_, R16P))
      endif
    else
      write(stderr, "(A)") 'error: cannot assign between "'//lhs%stringify()//'" and "'//rhs%stringify()//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine uom_symbol128_assign_uom_symbol64

  ! operator(+)
  function qreal32_add_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 + qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(in) :: lhs !< Left hand side.
  type(qreal64), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr = opr + rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_add_qreal64

  function qreal64_add_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 + qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64), intent(in) :: lhs !< Left hand side.
  type(qreal32), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr = lhs + opr
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_add_qreal32

  function qreal32_add_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 + qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr = opr + rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_add_qreal128

  function qreal128_add_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 + qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal32),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr = lhs + opr
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_add_qreal32

  function qreal64_add_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 + qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr = opr + rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_add_qreal128

  function qreal128_add_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 + qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal64),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr = lhs + opr
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_add_qreal64

  ! operator(/)
  function qreal32_div_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 / qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(in) :: lhs !< Left hand side.
  type(qreal64), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_div_qreal64

  function qreal64_div_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 / qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64), intent(in) :: lhs !< Left hand side.
  type(qreal32), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs / tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_div_qreal32

  function qreal32_div_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 / qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_div_qreal128

  function qreal128_div_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 / qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal32),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs / tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_div_qreal32

  function qreal64_div_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 / qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_div_qreal128

  function qreal128_div_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 / qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal64),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs / tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_div_qreal64

  ! operator(*)
  function qreal32_mul_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 * qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(in) :: lhs !< Left hand side.
  type(qreal64), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_mul_qreal64

  function qreal64_mul_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 * qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64), intent(in) :: lhs !< Left hand side.
  type(qreal32), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs * tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_mul_qreal32

  function qreal32_mul_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 * qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_mul_qreal128

  function qreal128_mul_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 * qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal32),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs * tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_mul_qreal32

  function qreal64_mul_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 * qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_mul_qreal128

  function qreal128_mul_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 * qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal64),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs * tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_mul_qreal64

  ! operator(-)
  function qreal32_sub_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 - qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(in) :: lhs !< Left hand side.
  type(qreal64), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr = opr - rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_sub_qreal64

  function qreal64_sub_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 - qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64), intent(in) :: lhs !< Left hand side.
  type(qreal32), intent(in) :: rhs !< Right hand side.
  type(qreal64)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr = lhs - opr
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_sub_qreal32

  function qreal32_sub_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 - qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr = opr - rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_sub_qreal128

  function qreal128_sub_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 - qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal32),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr = lhs - opr
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_sub_qreal32

  function qreal64_sub_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 - qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr = opr - rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_sub_qreal128

  function qreal128_sub_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 - qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal64),  intent(in) :: rhs !< Right hand side.
  type(qreal128)             :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr = lhs - opr
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_sub_qreal64

  ! operator(==)
  function qreal32_eq_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 == qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(in) :: lhs !< Left hand side.
  type(qreal64), intent(in) :: rhs !< Right hand side.
  logical                   :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp == rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_eq_qreal64

  function qreal64_eq_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 == qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64), intent(in) :: lhs !< Left hand side.
  type(qreal32), intent(in) :: rhs !< Right hand side.
  logical                   :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs == tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_eq_qreal32

  function qreal32_eq_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 == qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp == rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_eq_qreal128

  function qreal128_eq_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 == qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal32),  intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs == tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_eq_qreal32

  function qreal64_eq_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 == qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp == rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_eq_qreal128

  function qreal128_eq_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 == qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal64),  intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs == tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_eq_qreal64

  ! operator(/=)
  function qreal32_not_eq_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 /= qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32), intent(in) :: lhs !< Left hand side.
  type(qreal64), intent(in) :: rhs !< Right hand side.
  logical                   :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp /= rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_not_eq_qreal64

  function qreal64_not_eq_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 /= qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64), intent(in) :: lhs !< Left hand side.
  type(qreal32), intent(in) :: rhs !< Right hand side.
  logical                   :: opr !< Operator result.
  type(qreal64)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs /= tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_not_eq_qreal32

  function qreal32_not_eq_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal32 /= qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal32),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp /= rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal32_not_eq_qreal128

  function qreal128_not_eq_qreal32(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 /= qreal32` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal32),  intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs /= tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_not_eq_qreal32

  function qreal64_not_eq_qreal128(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal64 /= qreal128` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal64),  intent(in) :: lhs !< Left hand side.
  type(qreal128), intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = lhs
  opr = tmp /= rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal64_not_eq_qreal128

  function qreal128_not_eq_qreal64(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal128 /= qreal64` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal128), intent(in) :: lhs !< Left hand side.
  type(qreal64),  intent(in) :: rhs !< Right hand side.
  logical                    :: opr !< Operator result.
  type(qreal128)             :: tmp !< Temporary buffer with high precision.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tmp = rhs
  opr = lhs /= tmp
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction qreal128_not_eq_qreal64
endmodule fury_mixed_kinds
