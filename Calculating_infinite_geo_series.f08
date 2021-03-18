  !Name: Nkeiru Ubadike
  !Date: 3/10/2021
!Purpose: To calculate geometric series

program Geo_Series
  implicit none

  !Variable Dictionary
  real(16) :: coeff !coefficient of the geometric series (a)
  real(16):: ratio ! ratio of geometric series (r)
  real(16):: geo_series_sum ! sum of geometric series
  real(16):: geo_series_term ! terms of geometric series
  real(16), parameter :: accuracy = 1.0e-6 !Sum is stopped if terms smaller
                                           !smaller than this number
  real(16)::  sum !Sum of geometric series if convergent
  integer(16) :: n_1 !Counter variable
  integer(16) :: n_2 !Counter variable
  integer(16), parameter:: NN = 1000 !



  !Prompts user for input and take abolute value
  write(*,*) "Please enter the coefficient and ratio"
  write(*,*)"If ratio is less than 1, please write in decimal form"
  read(*,*) coeff, ratio

  !Initializing values
  If (ratio/=1)then
  sum = coeff/(1-ratio)
  end if

 if (abs(ratio)< 1.0) then !Checking abs(ratio) less than 1
  Convergent_geo_series: do n_1 = 0, NN
     
     !Calculating geometric series term
     geo_series_term = coeff*(ratio**n_1)
     !write(*,*) "The geo series term:", geo_series_term
     
     !Checking if ind. term is less than accuracy
     if(abs(geo_series_term)<accuracy)exit
    
     !Adding individual terms to final total
     geo_series_sum = geo_series_sum + geo_series_term
     !write(*,*) expsn_sin_sum
     
  end do Convergent_geo_series
  write(*,*)"The series converges"
  write(*,*) "Geometric series sum:", geo_series_sum
  write(*,*) "a/(1-r):",sum
  
else if(abs(ratio)>=1.0)then
   Divergent_geo_series: do n_1 = 0,19
     
     !Calculating geometric series term
     geo_series_term = coeff*(ratio**n_1)
     !write(*,*) "The geo series term:", geo_series_term
    
     !Adding individual terms to final total
     geo_series_sum = geo_series_sum + geo_series_term
     !write(*,*) expsn_sin_sum
     
  end do Divergent_geo_series
  write(*,*)"Since series diverges, only first 20 terms calculated"
  write(*,*) "Geometric series sum:", geo_series_sum
end if

 
  
  stop 0
  end program Geo_Series
