query = [character(16) :: 'm', 'meter', 'second', 'sec', 'km', 'kilometre']
do i=1, size(query)
  call metre%unset
  metre = SI%unit(trim(query(i)))
  print '(A)', query(i)//' -> '//metre%stringify(with_name=.true.)
enddo
