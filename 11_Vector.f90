program boom
  
  implicit none
  integer :: i
  real(8):: a(7,3), surfacearea,area1,area2,volume
  real(8), dimension(3) :: v12,v13,v56,v57, v14,v24,v34, v45,v46,v47
  ! Declare the variables that will be used
  ! i is for loop to print
  ! a is an array with 7 rows and 3 columns that contains all 7 points
  ! area1 is surface area for a big pyramid and area2 is for a small pyramid
  ! so surfacearea is area1 - area2 or the surface area of the polygon
  ! the third line variables are used to compute the area and volumn

  write(*,*) "Coordinates of vertices"
  write(*,'(A)') repeat('-',24)

  call Get_Data
  do i = 1,7 
     write(*,'(A,I1, 3f10.3)') "P",i,a(i,:)
  end do
  ! Here we obtain the data from subroutine Get_Data below and display the data

  write(*,*) ""

  write(*,*) "Vectors of edges and their magnitudes"
  write(*,'(A)') repeat('-',35)
10 format (A3,3F10.3,A13,F8.3)
! But format to show the output

  ! We show the output as the plan. "difference" and "Magnitude" are the function that will be explained below
  write(*,10) "v12", difference(a(1,:),a(2,:)),"|v12| = ",Magnitude(difference(a(1,:),a(2,:)))
  write(*,10) "v32", difference(a(3,:),a(2,:)),"|v32| = ",Magnitude(difference(a(3,:),a(2,:)))
  write(*,10) "v42", difference(a(4,:),a(2,:)),"|v42| = ",Magnitude(difference(a(4,:),a(2,:)))
  write(*,10) "v13", difference(a(1,:),a(3,:)),"|v13| = ",Magnitude(difference(a(1,:),a(3,:)))
  write(*,10) "v34", difference(a(3,:),a(4,:)),"|v34| = ",Magnitude(difference(a(3,:),a(4,:)))
  write(*,10) "v41", difference(a(4,:),a(1,:)),"|v41| = ",Magnitude(difference(a(4,:),a(1,:)))
  write(*,10) "v56", difference(a(5,:),a(6,:)),"|v56| = ",Magnitude(difference(a(5,:),a(6,:)))
  write(*,10) "v67", difference(a(6,:),a(7,:)),"|v67| = ",Magnitude(difference(a(6,:),a(7,:)))
  write(*,10) "v75", difference(a(7,:),a(5,:)),"|v75| = ",Magnitude(difference(a(7,:),a(5,:)))
  write(*,10) "v47", difference(a(4,:),a(7,:)),"|v47| = ",Magnitude(difference(a(4,:),a(7,:)))
  write(*,10) "v46", difference(a(4,:),a(6,:)),"|v46| = ",Magnitude(difference(a(4,:),a(6,:)))
  write(*,10) "v45", difference(a(4,:),a(5,:)),"|v45| = ",Magnitude(difference(a(4,:),a(5,:)))
  write(*,*) ""

  write(*,*) "Polygon 123675"
  write(*,'(A)') repeat("=", 15)
  
  ! The aim is to find the surface area of polygon
  ! A technique was applied; it is using side surface area big pyramid minus side surface small pyramid
  ! Then plus with areas of two bases

  ! To find the side surface area of pyramid, we need the total area of three triangles (on the side)
  ! To obtain triangle area, we compute by half of parallelogram which can be obtained by cross product of edge
  ! So we will first find each edge
  
  v14 = difference(a(1,:),a(4,:)) ! An edge of big pyramid
  v24 = difference(a(2,:),a(4,:)) ! An edge of big pyramid
  v34 = difference(a(3,:),a(4,:)) ! An edge of big pyramid
  area1 = Magnitude(cross_product(v14,v24))/2 + Magnitude(cross_product(v24,v34))/2 + Magnitude(cross_product(v14,v34))/2 !Side surface area of the big pyramid

  v45 = difference(a(4,:),a(5,:)) ! An edge of small pyramid
  v46 = difference(a(4,:),a(6,:)) ! An edge of small pyramid
  v47 = difference(a(4,:),a(7,:)) ! An edge of small pyramid 
  area2 = Magnitude(cross_product(v46,v47))/2 + Magnitude(cross_product(v45,v46))/2 + Magnitude(cross_product(v45,v47))/2
  ! We also need to add area of two bases of polygon: top and bottom
  v12 = difference(a(1,:),a(2,:))
  v13 = difference(a(1,:),a(3,:))
  v56 = difference(a(5,:),a(6,:))
  v57 = difference(a(5,:),a(7,:))
  surfacearea = area1-area2 + Magnitude(cross_product(v12,v13))/2 + Magnitude(cross_product(v56,v57))/2 ! Here, we obtain the surface area
  write(*,'(A15,F8.3)') "Surface area = ", surfacearea

  ! To find volume, we use big pyramid volume minuse small pyramid volume
  volume = triple_product(v14,v24,v34)/6 - triple_product(v45,v46,v47)/6
  write(*,'(A9,F8.3)') "Volume = ",volume

contains
  
  ! Get_Data is to read the file and obatin to array
  subroutine Get_Data
    open(1, file = 'coord.txt') ! Open file
    do i = 1,7 ! Use loop to get the file
       read(1,*) a(i,:)
    end do
    close(1) ! Close file
  end subroutine Get_data

  function difference(a,b) ! To find difference and show output in array(3)
    real(8):: difference(3),a(3),b(3)
    integer :: i
    do i = 1,3
       difference(i) = b(i)-a(i)
    end do
  end function difference

  function Magnitude(x) !Find magnitude
    real(8) :: Magnitude,x(3)
    integer:: i
    magnitude = sqrt(dot_product(x,x)) !Here it maps to function dot_product
  end function Magnitude
    
  function cross_product(u,v) ! Cross product function. It is straight forward
    real(8):: cross_product(3),u(3),v(3)
    cross_product(1) = u(2)*v(3) - u(3)*v(2)
    cross_product(2) = u(3)*v(1) - u(1)*v(3)
    cross_product(3) = u(1)*v(2) - u(2)*v(1)
  end function cross_product

  function dot_product(u,v) ! Dot product function. It is also straight forward
    real(8):: dot_product, u(3),v(3)
    integer :: i
    dot_product = 0
    do i = 1,3
       dot_product = dot_product + u(i)*v(i)
    end do
  end function dot_product

  function triple_product(u,v,w) !Triple product. We apply dot and cross product
    real(8)::u(3),v(3),w(3),triple_product
    triple_product = abs(dot_product(u,cross_product(v,w)))
  end function triple_product
  
end program boom

