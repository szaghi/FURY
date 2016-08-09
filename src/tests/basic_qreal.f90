!< FURY basic qreal definition/creation test.
program basic_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY basic qreal definition/creation test.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal) :: aquantity       !< A quantity.
type(qreal) :: anotherquantity !< Another quantity.
logical     :: test_passed(3)  !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call initialize_si

aquantity = qreal(magnitude=1.2_R_P, unit=metre)
test_passed(1) = aquantity%magnitude<1.21_R_P

call aquantity%set(magnitude=2.2_R_P, unit=metre)
test_passed(2) = aquantity%magnitude<2.21_R_P

aquantity = qreal(magnitude=1._R_P, unit=metre)
anotherquantity = qreal(magnitude=1._R_P, unit=metre)
anotherquantity = aquantity + anotherquantity
test_passed(3) = anotherquantity%magnitude==2._R_P

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram basic_qreal
