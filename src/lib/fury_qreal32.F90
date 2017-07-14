!< FURY class definition of real (with float32 kind) quantity with associated unit of measure.

module fury_qreal32
!< FURY class definition of real (with float32 kind) quantity with associated unit of measure.
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom32
use penf, RKP => R4P
use stringifor

#include "fury_qreal.inc"
endmodule fury_qreal32
