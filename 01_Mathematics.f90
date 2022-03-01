program abc

  implicit none
  real :: x=0.0, y=0.0, r=0.0, s=0.0
  integer :: i=1
  !Construct or announce vaules for the program 

  write (*,*) "Solution of infinitely nested square root"
  write (*,*) "Start of iteration"
  do i = 1,1000   !Loop for calculate the infinite root
     y = x
     x = (1+x)**(0.5)
     r = floor(y*1000000) !Put floor for checking the 10^-6 precision
     s = floor(x*1000000) !Put floor for checking the 10^-6 precision 
     write (*,*)  i , "nested root : x = ", x
     if (r==s) then   !Check if the precision of 10^-6 has reached
        write (*,*) "End of iteration"
        write (*,*) "The value is ", x
        stop
     end if
  end do

end program abc

