!< FURY test of [[qreal]].
module dBm_to_mW_converter
!-----------------------------------------------------------------------------------------------------------------------------------
!< Define the converter (user-supplied) from dBm to mW.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: dBm_to_mW
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(converter64) :: dBm_to_mW
  !< Converter (user-supplied) from dBm to mW.
  contains
    procedure, nopass :: convert
endtype dBm_to_mW
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  pure function convert(magnitude, inverse) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< User-supplied conversion from dBm to mW.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)           :: magnitude !< Magnitude (of the quantity) to be converted.
  logical,   intent(in), optional :: inverse   !< Activate inverse conversion.
  real(R8P)                       :: converted !< Converted magnitude.
  logical                         :: inverse_  !< Activate inverse conversion, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  inverse_ = .false. ; if (present(inverse)) inverse_ = inverse
  if (inverse_) then
    converted = 10._R8P * log10(magnitude)
  else
    converted = 10._R8P**(magnitude / 10._R8P)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction convert
endmodule dBm_to_mW_converter

program fury_test_qreal_conversions_complex
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use dBm_to_mW_converter
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64)     :: dBm            !< dBm unit.
type(uom64)     :: mW             !< mW unit.
type(uom64)     :: kelvin         !< Kelvin unit.
type(uom64)     :: celsius        !< Celsius unit.
type(qreal64)   :: q1             !< A quantity.
type(qreal64)   :: q2             !< A quantity.
type(qreal64)   :: q3             !< A quantity.
type(dBm_to_mW) :: dBm2mW         !< Converter from dBm to mW.
logical         :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

dBm = uom64('dBm = @user mW')
mW = uom64('mW')

call dBm%set_alias_conversion(reference_index=1, alias_index=2, convert=dBm2mW)

q1 = 10. * dBm
q2 = q1%to(unit=mW)
test_passed(1) = q2%stringify(format='(F4.1)')=='10.0 mW'
print "(A,L1)", '10.0 dBm = '//q2%stringify(format='(F4.1)')//', is correct? ', test_passed(1)

call q1%unset
call q2%unset
q1 = 10. * mW
q2 = q1%to(unit=dBm)
test_passed(2) = q2%stringify(format='(F4.1)')=='10.0 dBm'
print "(A,L1)", '10.0 mW = '//q2%stringify(format='(F4.1)')//', is correct? ', test_passed(2)

kelvin = uom64('K')
celsius = uom64('degC<=273.15 + K=celsius>')

call q1%unset
call q2%unset
q1 = 2 * kelvin
q2 = 1 * celsius
q3 = q1 - q2%to(kelvin)
test_passed(3) = q3%stringify(format='(F7.2)')=='-272.15 K'
print "(A,L1)", '2 K - 1 celsius = '//q3%stringify(format='(F7.2)')//', is correct? ', test_passed(3)

call q3%unset
q3 = q2 - q1%to(celsius)
test_passed(4) = q3%stringify(format='(F6.2)')=='272.15 degC'
print "(A,L1)", '1 celsius - 2 K = '//q3%stringify(format='(F6.2)')//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_conversions_complex
