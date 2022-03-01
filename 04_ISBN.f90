program chawit

  implicit none
  integer, parameter::n=10 
  character(n)::string 
  integer, dimension(n):: number
  !announce parameter and string components
  integer::i,g,r=10,sum=0,b,t 
  ! i is for loop to get each digit of string
  ! g is for determining programs (between validation or finding ?)
  ! r is for the value that will multiply with each digit
  ! b is for finding the value of ?
  ! t is the value that multiply with the digit ?

  write(*,*) "Type in a 10-digit ISBN marked with “?” for the missing digit"
  read(*,*) string !get IBSN

  if (len(string)==10) then !check if it is ISBN 10 digits
     g = 1 !assume that it is a program for validation
     do i =1,10 !from digit 1 to 10
        if (string(i:i)=='?') then !if there is an ? in any digit
           number(i)=0 !it will be 0 at first and will try to find this number later
           g = 0 !tell that this the program to find ?
           t = r !t will the multiple of the digit
        else if (string(i:i)=='X' .or. string(i:i)=='x') then
           number(i)=10 !if there is an x, get 10 for x
        else
           read(string(i:i),*) number(i) !normal number in string will be in number
        end if
        number(i) = number(i)*r !each digit will multiply with r for that digit. For example the 1st digit will multiply with 10. 
        sum = sum + number(i) !add it to sum to determine if it is valid
        r = r-1 !the multiplied value will change, decreasing by 1 from 10 to 1
     end do
  else !if it is not ISBN, stop the program
     stop
  end if


  if (g==0) then !the program for validation
     do b = 0,10 !run from 0 to 10 to find the ?
        if (mod(sum+b*t,11) == 0) then
           if (b==10) then !change 10 to x
              write(*,*) "The missing digit is x"
           else
              write(*,*) "The missing digit is", b
              stop
        end if
     end if
     end do
  end if
  
  if(g==1) then !program for validation
     if (mod(sum,11)==0) then !if the sum can be divided by 11, then it is valid
        write(*,*) string, " is valid."
     else !if not, it is invalid
        write(*,*) string, " is invalid."
     end if
  end if

end program chawit
