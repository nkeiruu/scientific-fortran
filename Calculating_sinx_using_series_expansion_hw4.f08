  !Name: Nkeiru Ubadike
  !Date: 2/28/2021
!Purpose: To calculate sinx using series expansion
! Results:  X | expansion sin(X) |Difference
!          5.0| -0.9589242       | 2.022e-33   
!         -5.0| 0.9589242        | 2.022e-33

program Series_Expansion
  implicit none

  !Variable Dictionary
  real(16) :: input_deg !User input for degrees in radians i.e. x
  real(16):: abs_input_deg !Takes absolute value of user input
  real(16) :: accuracy
  real(16)::  intr_sin !Stores value of sin calculated with intrinsic fts
  real(16) :: expsn_sin!Stores value of individual term calculated with
  !                     series expansion
  integer(16) :: n_1 !Counter variable
  integer(16) :: n_2 !Counter variable
  real(16) :: factorial !Holds the value of (2n + 1)!
  integer(16) :: t_1 !Holds the value of 2n+1
  real(16):: expsn_sin_sum !Holds value for sum of the sin expansion
  integer(16), parameter:: NN = 1000 !Max numbers of terms for sin expansio
!n_1 = 0
!Initializing values
expsn_sin_sum = 0
factorial = 1
expsn_sin = 0

  !Prompts user for input and take abolute value
  write(*,*) "Please enter an angle in radians"
  read(*,*) input_deg
  abs_input_deg = abs(input_deg)

  intr_sin = sin(input_deg)
  
  Sin_expansion: do n_1 = 0, NN
     factorial = 1.0
     t_1 = 2*n_1 + 1
     !write(*,*) "2n+1=", t_1
     do n_2 = 1, t_1!Calculate two_n_plus_one factorial       
        factorial = factorial*n_2
     end do

     !write(*,*) "Iterative DO loop: The factorial of 2:",n_1," + 1 is",factorial


     !Calculating indvidual sin expansion terms
     expsn_sin = ((-1.0)**(n_1)*(abs_input_deg)**(2*n_1+1))/factorial
     !write(*,*) "The expsn sin is:", expsn_sin
     
     !Checking if ind. term is less than accuracy
     if (abs(expsn_sin)<accuracy) exit
  
     !Adding individual terms to final total
     expsn_sin_sum = expsn_sin_sum + expsn_sin
     !write(*,*) expsn_sin_sum

     
     
  end do Sin_expansion

  !If input is negative, multiples result by -1 as per sinx property
  !sin(-x) = -sin(x)
  if (input_deg<0) then
  expsn_sin_sum = -1.0*expsn_sin_sum
  end if

  !Writes result to screen
  write(*,*)"Sin Expansion:", expsn_sin_sum
  write(*,*)"Intrinsic sin", intr_sin
  write(*,*)"Difference:", abs(expsn_sin_sum)-abs(intr_sin)
  
  stop 0
  end program Series_Expansion
