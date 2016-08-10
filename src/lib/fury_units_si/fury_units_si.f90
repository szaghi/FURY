!< FURY definition of units symbols of *International System of Units*.
module fury_units_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of units symbols of *International System of Units*.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_metre
use fury_unit_second
use penf
! use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: initialize
public :: metre
public :: second
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
logical                      :: initialized=.false. !< Flag to check is units definitions are initialized.
type(unit_metre),  protected :: metre               !< The metre unit instance.
type(unit_second), protected :: second              !< The second unit instance.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine initialize(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize units definition.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I_P), intent(out), optional :: error  !< Error code, 0 => no errors happen.
  integer(I_P)                        :: error_ !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 0
  if (.not.initialized) then
    call metre%set(scale_factor=1._R_P, symbol='m', error=error_)
    call second%set(scale_factor=1._R_P, symbol='s', error=error_)
  endif
  initialized = .true.
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize
endmodule fury_units_si
