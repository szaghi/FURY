program cookbook_attach
!< Cookbook: three ways to attach a unit to a number.
use fury
implicit none
type(system_si64) :: SI
type(uom64)       :: metre
type(qreal64)     :: a, b, c

call SI%initialize
metre = uom64('m [length]')
!region attach
a = qreal64(magnitude=2._R8P, unit=metre, name='width')  ! the constructor, optionally named
b = 2._R8P * metre                                       ! a number times a unit
c = 2 * SI%qunit('metre')                                ! a number times a unit quantity of a system
!endregion attach
print '(A)', a%stringify(format='(F3.1)', with_name=.true.)
print '(A)', b%stringify(format='(F3.1)')
print '(A)', c%stringify(format='(F3.1)')
!run cookbook_attach cookbook_attach
endprogram cookbook_attach
