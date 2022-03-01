program boom
  implicit none
  integer:: n, i, j
  real(8), dimension(:,:), allocatable:: Matrix
  real(8) :: S=16,t,A,x_c,y_c
  Call Read_data(Matrix, n)
!  write(*,*) n
!   do i=1, n                                   
!      write(*,*) Matrix(i,:)
!   end do
!   write(*,*)

! Format
100 format (A10,A10,A10,A10)
200 format (A10,F10.3,F10.3,F10.3)

 ! Start to write  
  write(*,100) "case","A","x_c","y_c"
  t = 0;x_c=0;y_c=0
  
  ! The first case
  ! Obtain the first area which is only a circle
  do i = 1,n
     if (Matrix(i,1)**2 + Matrix(i,2)**2 .le. 4) then
        t = t+1 ! This is a count of the number
        x_c = x_c + Matrix(i,2) ! This is finding the center of mass of x
        y_c = y_c + Matrix(i,1) ! This is finding the center of mass of y ! We will use this in other cases too
     end if
  end do
  A = S*t/(real(n)) ! obtain A
  x_c = S*x_c/(real(n)) ! obtain Mx
  y_c = S*y_c/(real(n)) ! Obtain My
  write(*,200) "I",A,x_c/A,y_c/A ! Print case, A, x_c = Mx/A , y_c = My/A 
  ! This will be applied to all cases
  
  ! The second case
  ! The second area is two circles, so we have the if condition below
  t = 0;x_c=0;y_c=0
  do i = 1,n
     if (Matrix(i,1)**2 + Matrix(i,2)**2 .le. 4 .and. Matrix(i,1)**2 + Matrix(i,2)**2 .ge. 1) then
        t = t+1
        x_c = x_c + Matrix(i,2)
        y_c = y_c + Matrix(i,1)
     end if
  end do
  A = S*t/(real(n))
  x_c = S*x_c/(real(n))
  y_c = S*y_c/(real(n))
  write(*,200) "II",A,x_c/A,y_c/A

  ! The third case
  ! The third case is two circles and one line, so we have two if conditions first and then we use the area below the line
  t = 0;x_c=0;y_c=0
  do i = 1,n
     if (Matrix(i,1)**2 + Matrix(i,2)**2 .le. 4 .and. Matrix(i,1)**2 + Matrix(i,2)**2 .ge. 1) then
        if (Matrix(i,1) + Matrix(i,2) .le. 2) then
           t = t+1
           x_c = x_c + Matrix(i,2)
           y_c = y_c + Matrix(i,1)
        end if
     end if
  end do
  A = S*t/(real(n))
  x_c = S*x_c/(real(n))
  y_c = S*y_c/(real(n))
  write(*,200) "III",A,x_c/A,y_c/A
  
  ! The fourth case
  ! The fourth case is the two circles but the smaller circle is shifted and a line
  t = 0;x_c=0;y_c=0
  do i = 1,n
     if (Matrix(i,1)**2 + Matrix(i,2)**2 .le. 4 .and. Matrix(i,1)**2 + (Matrix(i,2)+1)**2 .ge. 1) then
        if (Matrix(i,1) + Matrix(i,2) .le. 2) then
           t = t+1
           x_c = x_c + Matrix(i,1)
           y_c = y_c + Matrix(i,2)
        end if
     end if
  end do
  A = S*t/(real(n))
  x_c = S*x_c/(real(n))
  y_c = S*y_c/(real(n))
  write(*,200) "IV",A,x_c/A,y_c/A
  
  ! To read data
contains
  Subroutine Read_data(A, n)
    real(8), dimension(:,:), allocatable,intent(out):: A
    integer, intent(out):: n
    integer:: status=0,i
    real(8) :: r
    
    open(10, file = "random_coord.txt")
    n = 0
    do while (status==0) ! Find the number of rows                            
       read(10,*, IOSTAT=status) r
       n=n+1
    end do
    n=n-1
    rewind(10)

    allocate(A(n,2))
    do i = 1,n; ! Read and scale
       read(10,*) A(i,:)
       A(i,1) = A(i,1)*4 - 2
       A(i,2) = A(i,2)*4 - 2
    end do   
    close(10)
  end Subroutine Read_data
     
end program boom
