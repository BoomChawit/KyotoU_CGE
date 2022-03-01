program boom

  implicit none 
  integer :: a,i,g,f
  real(8) :: x,e = 1D-12,xold,TOL
  ! announce variable
  ! a is for input
  ! i is for loop
  ! f and g are the final loop and range in loop
  ! x is the printed number
  ! xold is for comparing to x
  ! TOL is the max range that xold and x differ
  ! e is a very little real number
  
  write (*,*) "Assign the numerical tolerance (e.g. 1D-6)"
  read(*,*) TOL !real tolerance
  write(*,*) "Input the guessed value"
  read(*,*) a !read the guessed value
  
  x = real(a) !get the first value of x and change to real number
  if (a .le. 0) then !if x is negative then get g and f for loop
     g = -1
     f = -10000
  else !if x is non negative then also get g and f for loop
     g = 1
     f = 10000
  end if

  write(*,*)"i = Iteration x = Variable r = Residual"
  do i = 0,f,g !loop for print
     write (*,*) "i =" ,i, "x = " ,x ,"r =" ,-r(x) !print each component
     ! we will introduce r(x) as function written below
     xold = x !store x as xold
     x = x - (r(x)*e/(r(x+e)-r(x))) !compute new x
     if (abs(x - xold) .le. TOL ) then !compare x and xold
        write(*,*) "The numeridal solution with the precision up to", TOL ,"is",xold
        stop
     end if
  end do

contains
  function r(x) !create function of r(x)
    real(8):: r, x !announce variable in function
    r =  2**x - x**2 !what function will do
  end function r

end program boom
