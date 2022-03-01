program boom

  implicit none 
  character (50) :: matrixfile 
  integer :: status = 0, n=0, i,j
  real (8):: r
  real(8), dimension(:,:), allocatable :: Matrix, Inv_Matrix, Identity
  10 format(E12.3,$)
  
  !===== [1] Read file =====
  write(*,*) "Enter the file name of matrix"
  read(*,*) matrixfile
  open(1, file=matrixfile)
  allocate(Matrix(n,n,)); rewind(1
  write(*,'(A') "The matrix before inversion"
  do i = 1,n
     read(1,*) Matrix(i,:)
     do j = 1,n
        write(*,10) Matrix(i,j)
     end do
  end do
  close(1)

  !===== [2] Process data =====
  allocate(Inv_Matrix(n,n), Identity(n,n))
  Inv_Matrix = Minverse(Matrix)
  Identity = matmul(Matrix, Inv_Matrix)

  !===== [3] Display data ======

  !===== [4] Write file =====

contains
  !======[5] function =====
  
end program boom
