! Author:Nkeiru Ubadike
! Date: 3/14/2021
! Purpose: Integrate x^2sin(x) =xcos(x)

program midpoint
  implicit none

  integer::  nsub ! number of subintervals
  integer:: loop !loop index
  real(8) :: xmid !midpoint to evaluate at
  real(8) :: dx ! width of subinterval
  real(8) :: xlow, xhi ! beginnning, middle, end of subinterval
  real(8):: sum !Variable to hold sum


  real(8):: fi !Holds value of function at midpoint of interval

  ! Prompt user for bounds of integral
  write(*,*) "Enter a value for lower and upper bound respectively"
  
  read(*,*) xlow, xhi
  ! Prompt the user for accuracy
  write(*,*) "Enter number of subintervals you wish to calculate"
  read(*,*) nsub
  
  sum = 0
  ! For nsub number of subintervals, calculate width of a subinterval
  dx = (xhi-xlow)/(1.0*real(nsub))
       
  ! Initialize xmid
  xmid = xlow+0.5*dx

  do loop = 1,nsub
     fi = (xmid**2)*sin(xmid) + xmid*cos(xmid)
     sum = sum + fi*dx
     xmid = xmid + dx
  end do
write(*,*) "sum = ", sum
  stop 1
end program midpoint
