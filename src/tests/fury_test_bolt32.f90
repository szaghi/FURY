!< FURY test of [[qreal]].
program fury_test_bolt32
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : real32
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom32)   :: meter               !< Meter unit.
type(uom32)   :: second              !< Second unit.
type(qreal32) :: distance_to_arrival !< Distance.
type(qreal32) :: time_to_arrival     !< Time.
type(qreal32) :: mean_velocity       !< Bolt's speed.
logical       :: test_passed(1)      !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

meter = uom32('m = meter = metre [length] {meter}')
second = uom32('s = sec = second [time] {second}')

distance_to_arrival = qreal32(100._real32, meter)
time_to_arrival = qreal32(9.58_real32, second)

mean_velocity = distance_to_arrival / time_to_arrival
test_passed(1) = mean_velocity%stringify()=='+0.104384E+02 m.s-1'
print "(A,L1)", 'Bolt''s record speed: '//mean_velocity%stringify(with_dimensions=.true.)//', is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_bolt32
