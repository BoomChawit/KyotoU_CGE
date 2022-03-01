program boom

  implicit none 
  integer, parameter :: n=4
  integer :: i,j
  real(8) :: b(n),A(n,n),c(1,n),d,t=1d-3
  real(8) :: x(n)=(/0,0,0,0/),RMSE,y(n)=(/ 0,0,0,0/)
  real(8) :: E(n,n),F(n),z(n)=(/ 0,0,0,0 /)
  !Anounce the variable that will be used
  !The first line is for swapping
  !The second line is for Jacobi method
  !The third line is for Gauss-Seidol method

 ! write(*,*) "123456789012345678901234567890123456789012345678901234567890"
  !The above is just to check the digits
  
  write(*,*) "Matrix and vector before pivoting"
  
  call Get_data
  10 format (A2,4F10.3,A5,F10.3)
  do i=1,4
     write(*, 10) "|",A(i,1:4),"|",b(i)
  end do
  !write the matrix
  !Then we try to swap the matrix by using pattern and loops
  do i=1,4
     do j=i+1,4
        if (abs(A(j,i)) > abs(A(i,i))) then
           c(1,:) = A(i,:)
           A(i,:) = A(j,:)
           A(j,:) = c(1,:)
           d = b(i)
           b(i) = b(j)
           b(j) = d
        end if
     end do
  end do
  !Finish swapping then we have E and F equal to A,b to be used in both Jacobi and Gauss-Seidol
  E = A
  F = b
  write(*,*) "Matrix and vector after pivoting"
  do i=1,4
     write(*,10) "|",A(i,1:4),"|",b(i)
  end do
  write(*,*) " "
  write(*,*) "Jacobi method with the convergence tolerance 0.100E-02"
  write(*,'(A)') repeat('-',65)
  20 format (A10,A10,A10,A10,A10,A11)
  write(*,20) "Iteration","x1","x2","x3","x4","RMSE"
  write(*,'(A)') repeat('-',65)
  30 format (I10,F10.3,F10.3,F10.3,F10.3,E15.3)
  !Start doing Jacobi method to finds the answer
  do i=0,100
     write(*,30) i,y(1),y(2),y(3),y(4),RMSE
     x(1) = y(1)
     x(2) = y(2)
     x(3) = y(3)
     x(4) = y(4)
     y(1) = (b(1) - A(1,2)*x(2) - A(1,3)*x(3) - A(1,4)*x(4))/A(1,1) !Here is Jacobi's method
     y(2) = (b(2) - A(2,1)*x(1) - A(2,3)*x(3) - A(2,4)*x(4))/A(2,2)
     y(3) = (b(3) - A(3,1)*x(1) - A(3,2)*x(2) - A(3,4)*x(4))/A(3,3)
     y(4) = (b(4) - A(4,1)*x(1) - A(4,2)*x(2) - A(4,3)*x(3))/A(4,4)
     RMSE = sqrt(0.25*((x(1)-y(1))**2+(x(2)-y(2))**2+(x(3)-y(3))**2+(x(4)-y(4))**2)) !Find RMSE
     if (RMSE .le. t) then !if RMSE satisfies the tolerance, then we exit the loop
        write(*,30) i+1,y(1),y(2),y(3),y(4),RMSE
        exit
     end if
  end do
  write(*,'(A)') repeat('-',65)
  
  !To begin Gauss-Seidol method, we need to get matrix A,b,x,y,RMSE again
  A = E
  b = F
  x = z
  y = z
  RMSE = 0
  write(*,*) ""
  write(*,*) "Gauss-Seidel method with the convergence tolerance 0.100E-02"
  write(*,'(A)') repeat('-',65)
  write(*,20) "Iteration","x1","x2","x3","x4","RMSE"
  write(*,'(A)') repeat('-',65)
  !Here we start doing Gauss-Seidol method
  do i=0,100
     write(*,30) i,y(1),y(2),y(3),y(4),RMSE
     x(1) = y(1) !Store in x in order to obtain RMSE
     x(2) = y(2)
     x(3) = y(3)
     x(4) = y(4)
     y(1) = (b(1) - A(1,2)*x(2) - A(1,3)*x(3) - A(1,4)*x(4))/A(1,1) !Here is how Gauss-Seidol works
     y(2) = (b(2) - A(2,1)*y(1) - A(2,3)*x(3) - A(2,4)*x(4))/A(2,2)
     y(3) = (b(3) - A(3,1)*y(1) - A(3,2)*y(2) - A(3,4)*x(4))/A(3,3)
     y(4) = (b(4) - A(4,1)*y(1) - A(4,2)*y(2) - A(4,3)*y(3))/A(4,4)
     RMSE = sqrt(0.25*((x(1)-y(1))**2+(x(2)-y(2))**2+(x(3)-y(3))**2+(x(4)-y(4))**2)) !Obtain RMSE
     if (RMSE .le. t) then !If RMSE satisfies the tolerance, then we exit the loop
        write(*,30) i+1,y(1),y(2),y(3),y(4),RMSE
        exit
     end if
  end do
  write(*,'(A)') repeat('-',65)

!Now is creating the matrix
contains
subroutine Get_data
  b = (/ 9.,7.5,-18.,10. /)
  A(1,:) = (/ 2.0, 0.0, 2.0, -3.0 /)
  A(2,:) = (/ 0.25, 0.0, 0.25, 6.0 /)
  A(3,:) = (/ -5.0, 1.0, -1.5, 2.0 /) 
  A(4,:) = (/ 1.0, 1.0, 1.0, 1.0 /)
end subroutine Get_data  

end program boom
