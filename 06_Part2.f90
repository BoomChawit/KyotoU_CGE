program Boom
  implicit none
  integer :: i,j,m,n
  real :: s,r
  real(8):: x=0.0,f=0.0

  write(*,*) "12 Decimals iteration for correct solution is"
  do i = 1,100 !hundred terms are enough (I think), if not we can later add more terms
     x = 1 + real(i)
     s = real(i) - 1
     do j = 1,i-2
        x = 1+s*(x**0.5)
        s = s - 1
     end do
     !until here everything is same as the 1.f90 program
     !now I will explain my idea little by little
     !we cannot multiply 10**12 to any value because of the program limitation
     !also we cannot directly multiply by 10**6 and 10**6. Here is a trick
     !we then have to first multiply by 10**6 and minus by its floor
     !after we get the other value, we now can multiply by another 10**6 
     !lastly compare both value and we finish
     f = (x**0.5)*1000000 - floor(x**0.5*1000000)
     m = n
     n = floor(f*1000000)
     write(*,*) x**0.5
     if (m==n .and. m .ne. 0)then
        write(*,*) i
        write(*,*) "As seen from above, the last two terms are equal up until the 12th decimal place"
        stop
     end if
  end do

end program boom
