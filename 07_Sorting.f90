program boom

  implicit none
  integer :: l,h
  integer :: n
  integer, dimension(:), allocatable :: a,b
  integer :: i,j,k,r
  ! announce variables for the program
  ! l is lower limit and h is upper limit
  ! n is number of integer
  ! a is the array to be allocated
  ! b is another array that equals to a
  ! i is loop for getting n random numbers
  ! j and k are for loop for each selection sort
  ! r is for allocation or swapping

!  integer, parameter:: n =5
!  integer, dimension(n) :: a = (/6,4,3,3,7/),b !these two lines are for testing as sample output

  write(*,*) "Enter the number of integer"
  read(*,*) n
  write(*,*) "Enter the lower limit"
  read(*,*) l
  write(*,*) "Enter the upper limit"
  read(*,*) h
  write(*,*) "----------------------------"
  allocate(a(n),b(n))
  Call SRAND (time())
  ! get number of random integer, lower limit and upper limit. Also set up seed random

  do i = 1,n ! This loop is for putting random number to each position in array a
     a(i) = NINT((h-l)*RAND()+l) ! Random number in the range of lower and upper limit
  end do

  write(*,*) "Unsorted list |", a ! print to see what a is 
  write(*,*) "----------------------------"
  
  ! From now, it will be 3 selection sorts. The way to obtain loops was observing the pattern
  b=a ! We need to store a as b because we need a for another selection sort
  write(*,*) "< Selection sort >"
  write(*,*) "----------------------------"
  do j = 1,n-1 ! We need two loops here to get the order of the sort
     do k = j+1,n ! Obtained by observing the pattern
        write(*,*) "compare",j,"&", k ,"|", a
        if (a(k)<a(j)) then ! use if to compare the value of two position in array a
           r = a(j) ! if it satisfies the condition, do these replacements
           a(j)=a(k)
           a(k)=r
        end if
     end do
     write(*,*) "----------------------------"
  end do
  write(*,*) "Sorted list |", a ! Obtain sorted value
  write(*,*) " "
  
  a = b ! To begin new sort, we need get the original a which we stored as b
  write(*,*) "< Bubble sort >"
  write(*,*) "---------------------------"
  do j = n,2,-1 ! Again, the pattern is obtained from observations
     do k = 1,j-1
        write(*,*) "compare", k,"&",k+1,"|",a
        if (a(k+1) < a(k)) then ! Compare by IF
           r = a(k) ! If it satisfies the condition, then replace
           a(k) = a(k+1)
           a(k+1) = r
        end if
     end do
     write(*,*) "----------------------------"
  end do
  write(*,*) "Sorted list |",a ! Obtained sort value
  write(*,*) " "
  
  a = b ! Set to the original a again
  write(*,*) "< Insertion sort >"
  write(*,*) "---------------------------"
  do j = 1,n-1 ! Again, these were obtained by observation of pattern
     do k = j,1,-1
        write(*,*) "compare", k, "&",k+1,"|",a
        if (a(k+1) < a(k)) then ! Compare by IF
           r = a(k) ! If it satisfies the condition, then replace!
           a(k) = a(k+1)
           a(k+1) = r
        end if
     end do
     write(*,*) "----------------------------"
  end do
  write(*,*) "Sorted list |",a ! Obtain sorted value

  ! We need r when we swap two values because we need to store the first value before replacing the second value to the first value. Then, use the store value to replace the second value.

end program boom
     
  
  
