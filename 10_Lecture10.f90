program finding_divisors

  implicit none
  integer :: i,j,n,k, d(10)
  
  open (1, file = "divisors")
  write(*,*) "Enter a positive integer:"
  read(*,*) n
  write(1,*) "Here are the divisors of ",n," :"
  k=0
  do i = 1,n
     if (mod(n,i)==0) then
        k = k+1
        d(k) = i
     end if
     if (k == 10) then
        write(1, '(10I6)') d(:)
        k = 0
     end if
  end do
  write(1, '(10I6)') d(:)
  close(1)

  write (*,*) "The divisors are listed in the file 'divisors'. Bye."

end program finding_divisors
