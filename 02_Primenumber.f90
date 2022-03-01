program abc

  implicit none
  integer:: lower, upper, i, j, a, t=0
  !announces variables that will be use in this program. a is for telling if it is prime number of not (a=0 is prime number; a=1 is not prime number. t is for counting the number of prime numbers.
  
  write (*,*) "Find prime number between two integers (minimum = 2 and maximum = 32767)"
  write (*,*) "Input the lower limit"
  read (*,*) lower
  write (*,*) "Input the upper limit"
  read (*,*) upper
  !read the values of lower and upper limit.
  
  if (lower < 2 .OR. lower>32767) then
     stop
  else if (upper < 2 .OR. upper > 32767) then
     stop
  end if
  !check if it is under the program's condition (2 to 32767).
  
  write (*,*) "The following numbers between ", lower, "  and ", upper, " are prime"
  
  do i = lower, upper !loop of every number. Check one by one.
     a = 0 !assume that it is a prime number and try to use loop to check.
     do j = 2,i-1 !loop of division to check if it can be divided.
        if (mod(i,j) == 0)then !if it can be divided, it is not a prime number
           a = 1 
        end if
     end do
     if (a == 0) then !if it is a prime number, then write it
        write (*,*) i
        t = t+1 !to count the number of prime number
     end if
  end do

  write (*,*) "A total of ", t," prime number were found in this range."
  
end program abc

        
     
