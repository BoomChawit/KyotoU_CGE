program Boom

  implicit none
  integer :: i, j
  real :: x,s,r
  !i is for that we are going to print i terms
  !j is for finding a value of each term
  !x is the value of each term
  !s is multiple to find value of each term
  !r is an input (how many terms we will print?)

  write(*,*) "Input a finite number of radical"
  read (*,*) r !obtain an input
  do i = 1,int(r) !loop for each term from 1 to the input
     x = 1 + real(i) !as the equation the first vaule to be computed is this x
     s = real(i) - 1 !as the equation the first multiple to be computed is this s 
     do j = 1,i-2 !another loop for finding the value for each term
        ! here, we need to start from 1 to i-2 because for some reason the first two terms are confusing to compute 
        x = 1+s*(x**0.5) !follow the equation
        s = s - 1 !as seen s is decreasing each term
     end do
     write (*,*) x**0.5 !do not forget to put square root of x before printing for each term
  end do

end program boom
