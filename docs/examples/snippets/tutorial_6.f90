program tutorial_6
!< Tutorial 6: the SI system, units, prefixes and constants by name.
use fury
implicit none
type(system_si64) :: SI
type(uom64)       :: metre, newton, kilometre, minute
type(qreal64)     :: distance, light, kilobyte, g, weight, converted
character(16)     :: query(6)
integer           :: i

call SI%initialize

metre  = SI%unit('metre')
newton = SI%unit('N')
print '(A)', metre%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)
print '(A)', newton%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)

query = [character(16) :: 'm', 'meter', 'second', 'sec', 'km', 'kilometre']
do i=1, size(query)
  call metre%unset
  metre = SI%unit(trim(query(i)))
  print '(A)', query(i)//' -> '//metre%stringify(with_name=.true.)
enddo

kilometre = SI%unit('km')
minute    = SI%unit('min')
distance  = 42.195_R8P * kilometre
converted = distance%to(SI%unit('m'))
print '(A)', distance%stringify(format='(F6.3)')//' = '//converted%stringify(format='(F7.1)')

kilobyte = 64 * SI%qunit('KiB')
call converted%unset
converted = kilobyte%to(SI%unit('byte'))
print '(A)', kilobyte%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F7.1)')

light = SI%const('speed_of_light')
g     = SI%const('gravity')
weight = 75._R8P * SI%unit('kg') * g
print '(A)', light%stringify(compact_reals=.true., with_name=.true.)
print '(A)', 'weight: '//weight%stringify(format='(F5.1)', with_dimensions=.true.)
endprogram tutorial_6
