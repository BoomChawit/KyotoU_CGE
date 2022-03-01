program boom

  implicit none 
  integer :: i,n
  real(8):: k,m,x0,v0,t0,tf
  real(8):: t,x,v,a,xmax,vmax,amax
  real(8):: phi,omega
  real(8):: delt

  write(*,*) "Simple harmonic oscillator"
  write(*,'(A)') repeat('-',70)

  call Get_Data
!  write(*,*) k
!  write(*,*) m
!  write(*,*) x0
!  write(*,*) v0
!  write(*,*) t0,tf
  
  ! Task 1
  omega = (k/m)**(0.5)
  phi = atan(v0/(omega*x0))
  xmax = x0/cos(phi)
  vmax = xmax*omega
  amax = xmax*omega**2
  t = tf-t0
  ! Calculation for each term
  x = xmax*cos(omega*t - phi)
  v = -vmax*sin(omega*t-phi)
  a = -amax*cos(omega*t - phi)

  write(*,*) "1) Exact solution"
  write(*,'(A)') repeat('=',70)
10 format (A10,A10,A10,A10,A10,A10,A10) ! Format to print
  write(*,10) "t","x","v","a","xmax","vmax","amax"
  write(*,10) "(s)","(m)","(m/s)","(m/s^2)","(m)","(m/s)","(m/s^2)"
  write(*,'(A)') repeat('-',70)
20 format (F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2) ! Format to print values
  write(*,20) t,x,v,a,abs(xmax),abs(vmax),abs(amax)
  write(*,'(A)') repeat('-',70)
  write(*,*) " "

  ! Task 2
  write(*,*) "2) Numerical Solution"
  write(*,*) "2.1) Forward-Euler scheme"
  write(*,'(A)') repeat("=",70)
  write(*,10) "t","x","v","a","xmax","vmax","amax"
  write(*,10) "(s)","(m)","(m/s)","(m/s^2)","(m)","(m/s)","(m/s^2)"
  write(*,'(A)') repeat('-',70)
  x = x0;v = v0;a=-(omega**2)*x ! initial values
  xmax = 0;vmax = 0;amax = 0 ! Set max is zero to obtain the maximum later
  n = 1
  delt = t/n
  do i = 0,int(t)-1,int(delt) ! use loop to find the values for each n
     ! Do accordin to the formula
     x = x+v*delt
     v = v+a*delt
     a = -(omega**2)*x
     ! Find the maximum length
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
30 format (I10,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2)
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)

  ! Repeat like the above but change the loop
  x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 10
  delt = t/n
  do i = int(t0),int(tf)-1,int(delt) ! The loop changes according to n
     x = x+v*delt
     v = v+a*delt
     a = -(omega**2)*x
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax) 

  ! The rest are the same but different n
   x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 100
  delt = t/n
  do i = int(t0),int(tf/delt)-1,1
     x = x+v*delt
     v = v+a*delt
     a = -(omega**2)*x
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)

  x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 1000
  delt = t/n
  do i = int(t0),int(tf/delt)-1,1
     x = x+v*delt
     v = v+a*delt
     a = -(omega**2)*x
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)
  write(*,'(A)') repeat('-',70)
  write(*,*) ""

  ! Task 2.2
  write(*,*) "2.2) Backward-Euler scheme"
  write(*,'(A)') repeat('=',70)
  write(*,10) "t","x","v","a","xmax","vmax","amax"
  write(*,10) "(s)","(m)","(m/s)","(m/s^2)","(m)","(m/s)","(m/s^2)"
  write(*,'(A)') repeat('-',70)
  x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 1
  delt = t/n
  do i = int(t0),int(tf/delt)-1,1
     a = -(omega**2)*x
     v = v + a*delt
     x = x + v*delt
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)

  
  x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 10
  delt = t/n
  do i = int(t0),int(tf/delt)-1,1
     a = -(omega**2)*x
     v = v + a*delt
     x = x + v*delt
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)

  
  x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 100
  delt = t/n
  do i = int(t0),int(tf/delt)-1,1
     a = -(omega**2)*x
     v = v + a*delt
     x = x + v*delt
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)

  
  x = x0;v = v0;a=-(omega**2)*x
  xmax=0;vmax=0;amax=0
  n = 1000
  delt = t/n
  do i = int(t0),int(tf/delt)-1,1
     a = -(omega**2)*x
     v = v + a*delt
     x = x + v*delt
     if (abs(x)>abs(xmax)) then
        xmax = x
     end if
     if (abs(v)>abs(vmax)) then
        vmax = v
     end if
     if (abs(a)>abs(amax)) then
        amax = a
     end if
  end do
  write(*,30) n,x,v,a,abs(xmax),abs(vmax),abs(amax)
  write(*,'(A)') repeat('-',70)

! Obtain the data from the file
contains
  subroutine Get_Data
    open(1, file = 'input.txt')                                               
    read(1,*) k
    read(1,*) m
    read(1,*) x0
    read(1,*) v0
    read(1,*) t0,tf
    close(1)                                                        
  end subroutine Get_data

end program boom
