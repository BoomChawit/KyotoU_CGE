program boom
  implicit none
  integer, parameter :: n=4
  integer :: i,j,k
  real(8) :: b(n),A(n,n),c(1,n),d,t=1d-3
  real(8) :: x(n),RMSE=0,y(n)
  real(8) :: E(n,n),F(n),z(n)
  !Anounce the variable that will be used                                                                                  
  !The first line is for swapping                                                                                          
  !The second line is for Jacobi method                                                                                    
  !The third line is for Gauss-Seidol method  
  
  !obtain a guess matrix
  do i = 1,n
     write(*,*) "please enter a guess number",i;read(*,*) z(i)
  end do
  write(*,'(A12,4f10.3)') "Guess is", z(1:n)
  x = z !z is to be used later, so we have to store z
  y = z                                      

 ! write(*,*) "123456789012345678901234567890123456789012345678901234567890"    
  !The above is just to check the digits                                        
  write(*,*) "Matrix and vector before pivoting"

  call Get_data
10 format (A2,4F10.3,A5,F10.3)
  do i=1,n
     write(*, 10) "|",A(i,1:n),"|",b(i)
  end do
  !write the matrix                                                             
  !Then we try to swap the matrix by using pattern and loops                    
  do i=1,n
     do j=i+1,n
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
                                                                 
  E = A
  F = b
  write(*,*) "Matrix and vector after pivoting"
  do i=1,n
     write(*,10) "|",A(i,1:n),"|",b(i)
  end do
  write(*,*) " "
  write(*,*) "Jacobi method with the convergence tolerance 0.100E-02"
  write(*,'(A)') repeat('-',65)
20 format (A10,A10,A10,A10,A10,A11)
  write(*,20) "Iteration","x1","x2","x3","x4","RMSE"
  write(*,'(A)') repeat('-',65)
30 format (I10,F10.3,F10.3,F10.3,F10.3,E15.3)

  !Start doing Jacobi method to finds the answer
40 format (A10,F10.3,F10.3,F10.3,F10.3,A11)                                                        
  write(*,40) "0",x(1:n), "-"
  do i=1,100
     RMSE = 0
     y = x
     do j = 1,n
        x(j) = b(j)/A(j,j) !Start with b(j)/A(j,j)
        do k = 1,n !Obtained from the pattern
           if (j .ne. k) then !Obtained from the pattern
           x(j) = x(j) - (A(j,k)*y(k))/A(j,j) !Here we used y(k) because we use old values
        end if
        end do           
        RMSE = RMSE + (x(j)-y(j))**2 !RMSE sums
     end do
     RMSE = sqrt(RMSE/n) !real RMSE
     write(*,30) i,x(1:n),RMSE
     if (RMSE .le. t) then !if RMSE satisfies the tolerance, then we exit the loop                   
        exit
     end if
  end do
  write(*,'(A)') repeat('-',65)

  !We have to recreate all variables
  A = E
  b = F
  x = z
  y = z
  RMSE = 0
  write(*,*) " "
  write(*,*) "Gauss-Seidol method with the convergence tolerance 0.100E-02"
  write(*,'(A)') repeat('-',65)
  write(*,20) "Iteration","x1","x2","x3","x4","RMSE"
  write(*,'(A)') repeat('-',65)
  
  !Start doing Gauss-Seidol method to finds the answer                                                                      
  write(*,40) "0",x(1:n), "-"
  do i=1,100
     RMSE = 0
     y = x
     do j = 1,n
        x(j) = b(j)/A(j,j) !Again, start from b(j)/A(j,j) from the pattern
        do k = 1,n
           if (j .ne. k) then !obtained by seeing the pattern
           x(j) = x(j) - (A(j,k)*x(k))/A(j,j) !The only different is x(k) here because Gauss-Seidol used the new value
        end if
        end do
        RMSE = RMSE + (x(j)-y(j))**2
     end do
     RMSE = sqrt(RMSE/n)
     write(*,30) i,x(1:n),RMSE
     if (RMSE .le. t) then !if RMSE satisfies the tolerance, then we exit the loop                                         
        !write(*,30) i+1,x(1:n),RMSE
        exit
     end if
  end do
  write(*,'(A)') repeat("-",65)
!create matrix
contains
subroutine Get_data
  b = (/ 9.,7.5,-18.,10. /)
  A(1,:) = (/ 2.0, 0.0, 2.0, -3.0 /)
  A(2,:) = (/ 0.25, 0.0, 0.25, 6.0 /)
  A(3,:) = (/ -5.0, 1.0, -1.5, 2.0 /)
  A(4,:) = (/ 1.0, 1.0, 1.0, 1.0 /)
end subroutine Get_data

end program boom
