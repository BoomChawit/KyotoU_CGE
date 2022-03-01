program boom

  implicit none
  real(8):: NPV,r,m,e = 1d-12
  integer :: x,i
  character(len=21) :: string1 = "Rates of interest (%)"
  character(len=22) :: string2 = "Net present values ($)"
  !Announce the variable
  
  write(*,'(4X,A,10X,A)') string1, string2
  write(*,'(A)') repeat ('=',55)
  !write to be the same as the sampel
  
  do x = 0,25,5 !to select 0,10,15,20,24 to print
     if (x /= 5) then
        r = real(x)/100
        NPV = y(r) !print NPV for each percent
        write(*,100) r*100, NPV
        100 FORMAT (13X,F5.2,24X,F10.2) !make a format
     end if
  end do
  write(*,'(A)') repeat ('=',55)
  
  m = 0 !guess any value
  do i = 1,10000 !do like midterm
     m = m - y(m)/((y(m+e)-y(m))/e)
     if (y(m) .le. 0) then !obtain value
        write(*,200) m*100 
        200 FORMAT ("The internal rate of return is ",F8.5, " %")
        stop
     end if
  end do

contains
  function y(x)                               
    real(8):: y, x                       
    y = -300000 + (150000/(1+x)) + (150000/(1+x)**2) + (160000/(1+x)**3)      
  end function y
  

end program boom
