  !Name: Nkeiru Ubadike
  !Date: 3/11/2021
  !Purpose: To display partial permutations
  !When input = 5,# of arrangements = 60.

program Partial_permutation
  implicit none

  !Variable dictionary
  integer(8) :: input !User input
  integer(8) :: num_arangmt !TOtal number of arrangements
  integer :: n1 !Counter variable
  integer :: n2 !Counter variable
  integer :: n3 !Counter variable
  
  num_arangmt = 0

  write(*,*) "Please enter an integer"
  read(*,*) input
  if(input<0)then
     write(*,*) "Please enter number greater than 1"
     stop 1
     end if
  Outer: do n1= 1,input
     Inner: do n2 = 1, input
        if(n1==n2)cycle inner
        Innermost: do n3 =1, input
           if ((n3==n1).or. (n3==n2)) cycle innermost
           write(*,*) n1, n2, n3
           num_arangmt = num_arangmt + 1
        end do Innermost
     end do Inner
  end do Outer

write(*,*) "Number of arrangement:", num_arangmt
stop 0
end program Partial_permutation
