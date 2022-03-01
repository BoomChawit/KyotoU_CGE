program boom
  implicit none
  real(8) ::p=-1.0,s,area=0.0,mx=0.0,my=0.0
  integer ::i,n 
  do n = 1,10000
     if (n==5 .or. n==20 .or. n==100 .or. n==10000)then
        s = 2/real(n)
        do i = 1,n+1 
           area = area + y(p)
           mx = mx + (y(p)**2) 
           my = my + y(p)*p
           p = p + s
        end do
        write(*,*) "---------------------------------------"
        write(*,*) "Area=", area*s 
        write(*,*) "Centroid X=", my/area
        write(*,*) "Centroid Y=", mx/(2*area)
        area = 0.0
        mx = 0.0
        my = 0.0
        p = -1.0
     end if
  end do
  
  write(*,*) "---------------------------------------"
  contains
  function y(x) 
    real(8):: y, x
    if (x<0) then
       y = (1-x**2)**(0.5)
    else
       y = 1-x
    end if
  end function y
 
 end program boom
