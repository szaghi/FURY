!< FURY test of [[qreal]].
program fury_test_bolt64
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : real64
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64)   :: meter               !< Meter unit.
type(uom64)   :: second              !< Second unit.
type(qreal64) :: distance_to_arrival !< Distance.
type(qreal64) :: time_to_arrival     !< Time.
type(qreal64) :: mean_velocity       !< Bolt's speed.
logical       :: test_passed(1)      !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

meter = uom64('m = meter = metre [length] {meter}')
second = uom64('s = sec = second [time] {second}')

distance_to_arrival = qreal64(100._real64, meter)
time_to_arrival = qreal64(9.58_real64, second)

mean_velocity = distance_to_arrival / time_to_arrival
test_passed(1) = mean_velocity%stringify()=='+0.104384133611691E+002 m.s-1'
print "(A,L1)", 'Bolt''s record speed: '//mean_velocity%stringify(with_dimensions=.true.)//', is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_bolt64
