!< FURY test of [[qreal]].
program fury_test_bolt
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : real64
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom)   :: meter               !< Meter unit.
type(uom)   :: second              !< Second unit.
type(qreal) :: distance_to_arrival !< Distance.
type(qreal) :: time_to_arrival     !< Time.
type(qreal) :: mean_velocity       !< Bolt's speed.
logical     :: test_passed(1)      !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

meter = uom('m = meter = metre [length] {meter}')
second = uom('s = sec = second [time] {second}')

distance_to_arrival = qreal(100._real64, meter)
time_to_arrival = qreal(9.58_real64, second)

mean_velocity = distance_to_arrival / time_to_arrival
test_passed(1) = mean_velocity%stringify()=='+0.104384133611691E+002 m.s-1'
print "(A,L1)", 'Bolt''s record speed: '//mean_velocity%stringify(with_dimensions=.true.)//', is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_bolt
