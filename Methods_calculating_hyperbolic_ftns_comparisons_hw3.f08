  !Name: Nkeiru Ubadike
  !Date: 2/23/2021
  !Purpose: To calculate the hyperbolic functions via two methods: directly
  !intrinsic Fortran functions and in terms of exponential functions and to 
  !compare the results of two methods using relational operators

  !Step 4 differences for 45 degrees: Sinh and tanh definitions agree
  !up to 6 decimal places. Cosh defintions agree up to 8 decimal places

  !Step 5 discrepancies for 45 degrees: sinh(x/2) definitions agree only
  !up to 6 decimal places not 10.
  ! Cosh(x/2) definitions match up to 5 decimal places
  !Tanh(x/2) match up to 6 decimal places

program Hyperbolic_Functions
  implicit none

  !Variable dictionary
  real, parameter:: PI= 4.0*atan(1.0)
  real, parameter::RAD_TO_DEG = PI/180.0 !radians per degree
  real, parameter:: tol_1 = 1.0d-6 !tolerance/acceptable difference between
                                   !two real numbers
  real, parameter:: tol_2 = 1.0d-10
  real:: degrees !User input
  real:: radians
  real:: hyp_sin !Intrinsic hyperbolic sin
  real:: hyp_cos
  real:: hyp_tan
  real:: hyp_sin_exp ! Hyperbolic sin in terms of exponentials
  real:: hyp_cos_exp
  real:: hyp_tan_exp
  real:: sinh_half !Half argument formula for sin
  real:: cosh_half
  real:: tanh_half
  real:: sin_half_idy !Holds value for result of 1/2 arg. formula
  real:: cos_half_idy
  real:: tan_half_idy
  logical:: logi_test !Logical test variable for comparison

  !Converts degrees to radians and writes to terminal
  write(*,*) "Enter angle in degrees"
  read(*,*) degrees
  radians = degrees*RAD_TO_DEG
  write(*,*) degrees, "is ", radians, "radians"

  !Calculating hyperbolic functions with intrinsic functions
  hyp_sin = sinh(radians)
  hyp_cos = cosh(radians)
  hyp_tan = tanh(radians)
  write(*,*) "These hyperbolic functions are calculated with intrinsic function"
  write(*,*)"SINH:  ", hyp_sin, "COSH: ", hyp_cos, "TANH: ",hyp_tan

  !Calculating hyperbolic functions in terms of exponentials
  hyp_sin_exp = (exp(radians)-exp(-1.0*radians))/2.0
  hyp_cos_exp = (exp(radians)+exp(-1.0*radians))/2.0
  hyp_tan_exp = hyp_sin_exp/hyp_cos_exp
  !write(*,*) "Exponentials:", hyp_sin_exp, hyp_cos_exp, hyp_tan_exp

  !Testing if intrinsic and exponential defn. equal within 6
  !decimal places....(Check intermediate values)
  logi_test = (abs(hyp_sin-hyp_sin_exp)<tol_1)
  write(*,*) "Intrinsic function sinh(theta) equals &
       exponential defintition?", logi_test
  
  logi_test = (abs(hyp_cos-hyp_cos_exp)<tol_1)
  write(*,*) "Intrinsic function cosh(theta) equals &
       exponential defintition?", logi_test
  
  logi_test = (abs(hyp_tan-hyp_tan_exp)<tol_1)
  write(*,*) "Intrinsic function tanh(theta) equals &
       exponential defintition?", logi_test

  !Defining half argument formula
  sin_half_idy= (sinh(radians))/sqrt(2.0*(cosh(radians)+1))
  cos_half_idy= sqrt((cosh(radians) +1)/2.0)
  tan_half_idy= (sinh(radians))/(cosh(radians)+1)

  !Replacing argument of intrinsic ftns with x/2
  hyp_sin = sinh(radians/2.0)
  hyp_cos = cosh(radians/2.0)
  hyp_tan = tanh(radians/2.0)

  !write(*,*) "Intrinsic 2: ", hyp_cos
  !write(*,*) "Idy:", cos_half_idy

  !Testing if half argument formulas satisfied within 10
  !decimal places....(Check intermediate values)
  logi_test = (abs(sin_half_idy-hyp_sin)<tol_2)
  write(*,*) "Is the half argument formula for sinh(x) &
              satisfied?", logi_test
  logi_test = (abs(cos_half_idy-hyp_cos)<tol_2)
  write(*,*) "Is the half argument formula for cosh(x) &
              satisfied?", logi_test
  logi_test = (abs(tan_half_idy-hyp_tan)<tol_2)
  write(*,*) "Is the half argument formula for tanh(x) &
              satisfied?", logi_test

  


stop 0
end program Hyperbolic_Functions
