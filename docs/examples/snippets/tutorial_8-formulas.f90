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
