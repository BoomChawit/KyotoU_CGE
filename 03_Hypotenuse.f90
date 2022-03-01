program Boom

  implicit none
  integer :: input, i=0, a, b, c, x, g 

  write (*,*) "Enter maximum hypotenuse: "
  read (*,*) input !get the maximum hypoteuse

  do c = 1,input !for three loops, we will run c from 1-100, and b from 1 to c-1, and a from 1 to b
     do b = 1,c-1
        do a = 1,b
           if (c**2 == b**2 + a**2) then !if it is pythagorean
              g=0 !assume to be output and try to delete it by factors
              do x = 2,a !try to find the factor of a b c
                 if (mod(a,x)==0) then !if some x is a factor of a
                    if (mod(b,x)==0 .AND. mod(c,x)==0) then !if it is also factor of b
                       g=1 !we will not print it
                    end if
                 end if
              end do
              if(g==0)then !if we cannot find the factor
                 i=i+1 !for count the number of hypothenuse
                 write (*,*) i, ")", a, b, c
              end if
           end if         
        end do
     end do
  end do
  
  write (*,*) "The number of primitive Pythagorean triples found is ", i
end program Boom


   
