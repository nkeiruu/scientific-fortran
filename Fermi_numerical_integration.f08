! Author:Nkeiru Ubadike
! Date: 3/14/2021
! Purpose: Calculates Fesnel integral
! x_in= 5, nsub = 490, S(x) = 8.8286e-2
program fresnel
  implicit none

  integer ::  nsub,sub ! number of subintervals, and variable for integral loop
  real(8) :: x_in ! x point to evaluate S(x)
  real(8) :: dx ! width of subinterval
  real(8) :: xi,xmid,xf ! beginnning, middle, end of subinterval
  real(8) :: s_of_x ! stores integral for evaluation of S(x)
  real(8) :: accuracy ! accuracy of evaluation of S(x)
  real(8) :: s_of_x_prev ! previous evaluation of S(x) to test convergence
  real(8):: fi,fmid,fend !Holds value of function at start, middle
                         !and end of interval

  ! Prompt user for x_in
  write(*,*) "Enter a real number x where you wish to evaluate the &
              Fresnel Integral, S(x)"
  read(*,*) x_in
  ! Prompt the user for accuracy
  write(*,*) "Enter number for accuracy of evaluation of S(x)"
  read(*,*) accuracy
  
  ! Initialize number of subintervals
  nsub = 1

  ! Initialize the result for the "previous" number of
  ! subintervals. We set it to a very large number to make sure we do
  ! more than one iteration of the outer loop.
  s_of_x_prev = huge(1.0)
  
  ! Loop over number of subintervals
  Convergence_Test: do 

     ! For nsub number of subintervals, calculate width of a subinterval
     dx = (x_in-0)/real(nsub)
     
     ! Initialize S(x)
     s_of_x = 0.0

     ! Initialize xi,xmid,xf (which will depend on dx)
     xi = 0
     xmid = xi + (0.5*dx)
     xf = xi + dx
     
     ! Loop to evaluate S(x) with Simpson's rule for nsub subintervals.
     Simpson:do sub = 1,nsub

        !Evaluating function at start, mid and endpoints
        fi = sin(xi**2)
        fmid = sin(xmid**2)
        fend = sin(xf**2)
        !Calculating area of subinterval and add to total
        s_of_x = s_of_x + ((fi + (4*fmid) + fend)/6)*dx
        xi = xi + dx
        
     end do Simpson

     !write(*,*)"nsub = ", nsub
     
     !write(*,*) "S(x) = ", s_of_x
     !write(*,*) s_of_x-s_of_x_prev
     !write(*,*) "S(X)", s_of_x
     !write(*,*)"S(x) prev:", s_of_x_prev
     
     
     ! Test if s_of_x is converged (compare it to s_of_x_prev, see if
     ! difference is less than accuracy). If it is converged, stop the
     ! program.
     if (abs(s_of_x-s_of_x_prev)<accuracy)then
        write(*,*)"S(x) converged"

        ! Write out result of S(x), x_in, and nsub to the terminal
        write(*,*) "S(x) = ", s_of_x
        write(*,*)"x in =", x_in
        write(*,*)"nsub = ", nsub

        exit
        
     end if

     ! Set the present s_of_x to s_of_x_prev for the next loop
     s_of_x_prev = s_of_x

     ! Increment nsub by 1
     nsub = nsub + 1
     !write(*,*) "S(X)", s_of_x
     !write(*,*)"S(x) prev:", s_of_x_prev
     
  end do Convergence_Test ! End of do loop over nsub

  stop 1
end program fresnel
