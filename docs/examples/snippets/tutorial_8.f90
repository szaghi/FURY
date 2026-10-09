module dbm_converter
!< Tutorial 8: a user-supplied converter, from dBm (decibel-milliwatts) to mW and back.
use fury
implicit none
private
public :: dbm_to_mw

type, extends(uom_converter) :: dbm_to_mw
  contains
    procedure, nopass    :: convert_float128
    procedure, nopass    :: convert_float64
    procedure, nopass    :: convert_float32
    procedure, pass(lhs) :: assign_converter
endtype dbm_to_mw

contains
  pure function convert_float64(magnitude, inverse) result(converted)
  !< P[mW] = 10**(P[dBm]/10), inverse P[dBm] = 10*log10(P[mW]).
  real(R8P), intent(in)           :: magnitude
  logical,   intent(in), optional :: inverse
  real(R8P)                       :: converted
  logical                         :: inverse_

  inverse_ = .false. ; if (present(inverse)) inverse_ = inverse
  if (inverse_) then
    converted = 10._R8P * log10(magnitude)
  else
    converted = 10._R8P ** (magnitude / 10._R8P)
  endif
  endfunction convert_float64

  pure function convert_float32(magnitude, inverse) result(converted)
  real(R4P), intent(in)           :: magnitude
  logical,   intent(in), optional :: inverse
  real(R4P)                       :: converted

  converted = real(convert_float64(real(magnitude, R8P), inverse), R4P)
  endfunction convert_float32

  pure function convert_float128(magnitude, inverse) result(converted)
  real(R16P), intent(in)           :: magnitude
  logical,    intent(in), optional :: inverse
  real(R16P)                       :: converted
  logical                          :: inverse_

  inverse_ = .false. ; if (present(inverse)) inverse_ = inverse
  if (inverse_) then
    converted = 10._R16P * log10(magnitude)
  else
    converted = 10._R16P ** (magnitude / 10._R16P)
  endif
  endfunction convert_float128

  pure subroutine assign_converter(lhs, rhs)
  class(dbm_to_mw),     intent(inout) :: lhs
  class(uom_converter), intent(in)    :: rhs

  select type(rhs)
  class is (dbm_to_mw)
    lhs = rhs
  endselect
  endsubroutine assign_converter
endmodule dbm_converter

program tutorial_8
use dbm_converter
use fury
implicit none
type(uom64)     :: dBm, mW
type(qreal64)   :: power, converted
type(dbm_to_mw) :: converter

dBm = uom64('dBm = @user mW')
mW  = uom64('mW')
call dBm%set_alias_conversion(reference_index=1, alias_index=2, convert=converter)

power = 20._R8P * dBm
converted = power%to(mW)
print '(A)', power%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F5.1)')
call power%unset
power = converted%to(dBm)
print '(A)', converted%stringify(format='(F5.1)')//' = '//power%stringify(format='(F4.1)')
endprogram tutorial_8
