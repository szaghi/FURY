program tutorial_7
!< Tutorial 7: real kinds, 32, 64 and 128 bits quantities.
use fury
implicit none
type(uom32)    :: metre32
type(uom64)    :: metre64
type(uom128)   :: metre128
type(qreal32)  :: third32
type(qreal64)  :: third64
type(qreal128) :: third128, sum128
type(qreal64)  :: narrowed

!region kinds
metre32  = uom32('m [length]')
metre64  = uom64('m [length]')
metre128 = uom128('m [length]')
third32  = qreal32(1._R4P / 3._R4P, metre32)
third64  = qreal64(1._R8P / 3._R8P, metre64)
third128 = qreal128(1._R16P / 3._R16P, metre128)
print '(A)', third32%stringify()
print '(A)', third64%stringify()
print '(A)', third128%stringify()
!endregion kinds

!region mixed
sum128 = third128 + third64
narrowed = third128
print '(A)', sum128%stringify()
print '(A)', narrowed%stringify()
!endregion mixed
!run tutorial_7 tutorial_7
endprogram tutorial_7
