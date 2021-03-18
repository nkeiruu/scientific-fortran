! Author: Nkeiru Ubadike
! Date: 2/11/2021
! Purpose: Divide some numbers
program divide_two_numbers
implicit none ! Turn off implicit typing
! Variable dictionary
real :: factor1 ! Variable for factor 1
real :: factor2 ! Variable for factor 2
real :: divide ! Variable for result
integer :: ifactor1 = 4
integer :: ifactor2 = 3
integer :: idivide


factor1 = 1.0 ! Assign a value of 1 to factor1
factor2 = 2.0 ! Assign a value of 2 to factor2
divide = factor1/factor2 ! Divide and store in product

idivide = ifactor1/ifactor2

write(*,*) " Dividing ",factor1," and ", & ! Output
     factor2," = ",divide

write(*,*) " Dividing ",ifactor1," and ", & ! Output for extra credit
     ifactor2," = ",idivide

!I do not get the result expected.
! Changing the variables to real should solve problem.

stop 0 ! Stop execution of the program
end program divide_two_numbers
