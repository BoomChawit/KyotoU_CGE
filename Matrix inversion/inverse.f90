program boom
  implicit none
  integer:: n, i, j; real(8), dimension(:,:), allocatable:: Matrix,Inv_Matrix, Identity
10 format(E12.3, $)

  !===== [1] Read file =====
  Call Read_data(Matrix, n) ! Call the matrix
  write(*,'(A)') "The matrix before inversion"
  do i=1, n; do j=1,n  ! Write to show the matrix
     write(*,10) Matrix(i,j);end do;write(*,*)
  end do

  !===== [2] Process data ===== 
  allocate(Inv_Matrix(n,n), Identity(n,n))  ! Allocate
  Inv_Matrix = Minverse(Matrix) ! Do process, send to function
  Identity = matmul(Matrix, Inv_Matrix) ! Multiply to check both matrix
  
  !===== [3] Display and write data =====  
  write(*,'(A)') "The matrix after inversion" 
  do i=1,n; do j =1,n ! Write to show the inverse matrix
     write(*,10) Inv_Matrix(i,j);end do;write(*,*)
  end do
  write(*,'(A)') "Check the identity matrix"
  do i=1,n; do j = 1,n ! Write to check the identity
     write(*,2) Identity(i,j);end do;write(*,*)
  end do
2 format (F12.3,$)  
  open (2, file = "invmat.dat") ! Write file
  do i=1, n; do j=1, n
     write(2,10) Inv_Matrix(i,j);end do; write(2,*)
  end do
  close(2)

  !===== [4] Subroutine =====
contains
  Subroutine Read_data(A, n)
    real(8), dimension(:,:), allocatable,intent(out):: A
    integer, intent(out):: n; integer:: status=0,i
    character(50):: matrixfile; real(8) :: r
    
    write(*,*) "Enter the file name of matrix";read(*,*) matrixfile
    open(10, file = matrixfile)
    n = 0
    do while (status==0) ! Find the number of rows
       read(10,*, IOSTAT=status) r;n=n+1 
    end do
    n=n-1; rewind(10)

    allocate(A(n,n))
    do i = 1,n;read(10,*) A(i,:);end do ! Read the matrix
    close(10)    
  end Subroutine Read_data

  !===== [5] Function =====
  
  function Minverse(A)
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
  
end program boom

