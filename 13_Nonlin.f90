program nonlinear

  implicit none
  integer, parameter :: m=3
  real(8) :: S(m), H(m,m)
  real(8), parameter :: CTOL = 1D-5

  Call Guess(S) !Call to subroutine Guess
  Call Solve(S) !Call to subroutine Solve
  
contains
  
  Subroutine Guess(x)
    real(8), intent(out):: x(m)
    integer :: i
    write(*,*) "Nonlinear system of 3 equations"
    do i = 1,m ! Read guess values
       write(*,*) "Guess value number",m
       read(*,*) x(i)
    end do
  end subroutine Guess

  Subroutine Solve(x) ! Solve the system by guess value
    real(8), intent(out) :: x(m)
    integer,parameter :: IMAX = 50
    integer :: k,n
    real(8) :: err, R(m),H(m,m)
    write(*,'(A10,A12,A12,A12,A20)') "Iteration", "x1","x2","x3","Norm of residuals"
    write(*,'(A)') repeat('-',66)
    do k = 0,IMAX ! Iteration which must be less than 50
       R = Residual(x)
       H = Hessian(x)
       err = magnitude(R)
       write(*,'(I10,F12.3,F12.3,F12.3,E20.3)') k,x,err ! Print the computed one
       x = x - matmul(Minverse(H),R) ! computed x for the next iteration
       if (err .le. CTOL) then ! Stop when 0
          write(*,'(A)') repeat('-',66)
          stop
       end if
    end do
  End Subroutine Solve

  function Residual(x) ! System to solve
    real(8) :: Residual(m),x(m)
    Residual(1)=-x(1)+2*x(2)+x(3)-2 
    Residual(2)=2*(x(1)+3)**2+5*x(2)**2-(x(3)+1)**2 
    Residual(3)=(x(1)-5)*(x(2)+2)+3
  end Function Residual

  Function Hessian(x) ! Hessian matrix
    real(8):: Hessian(m,m), x(m), x_(m), v(m), v_(m)
    real(8), parameter:: TOL=0.1*CTOL  
    integer:: i,j
    v = Residual(x)
    do i = 1,m
       do j =1,m
          x_ = x
          x_(j) = x(j) + TOL
          v_ = Residual(x_)
          Hessian(i,j) = (v_(i) - v(i))/ TOL
       end do
    end do
  end function Hessian
  
   function Minverse(A) ! Inverse obtained from the last assignment
    real(8), dimension(:,:):: A 
    ! A can be either static/dynamic array                                                                                            
    real(8), dimension(:,:), allocatable:: Minverse, B, V 
    real(8), dimension(:), allocatable:: temp,temp2; real(8), parameter:: TOL=1D-6
    real(8):: pivot;  integer:: i, j, k, n

    !===== Initialization =====                                                                                                       
    n = size(A)**0.5 ! row number of a symmetric matrix                                                                               
    allocate(Minverse(n,n), B(n,n), V(n,n), temp(n),temp2(n))
    B = A ! keep original A by copying it to B                                                                                        
    V(:,:) = 0.
    do i=1, n
       V(i,i) = 1.
    end do

    !===== Gauss-Jordan elimination =====                                                                                            
    do i=1, n
       do j=i+1, n
          if (abs(B(j,i)) > abs(B(i,i))) then ! Pivoting                                                                             
             temp = B(i,:);B(i,:) = B(j,:);B(j,:) = temp !Swapping B                                                                 
             temp2 = V(i,:);V(i,:) = V(j,:);v(j,:) = temp2 !Swapping V                                                             
          end if
       end do
       pivot = B(i,i)
       if (abs(pivot)<TOL) then !Check if it is singular or not                                                                     
          write(*,*) "The matrix is singular!";write(*,* ) "Program is terminated."; stop
       end if
       B(i,:) = B(i,:)/pivot;V(i,:) = V(i,:)/pivot
       do k=1, n
          pivot = B(k,i)
          if (k .ne. i) then ! Guass elimination                                                                                  
             B(k,:) = B(k,:) - B(i,:)*pivot;V(k,:) = V(k,:) - V(i,:)*pivot
          end if
       end do
    end do
    Minverse=V
    Deallocate (B,V,temp)
  end Function Minverse

  function Magnitude(x) !Find magnitude                                                                                                           
    real(8) :: Magnitude,x(3)
    integer:: i
    magnitude = sqrt(dot_product(x,x)) !Here it maps to function dot_product                                                                   
  end function Magnitude
    
end program nonlinear
 
