  !Name: Nkeiru Ubadike
  !Date: 2/14/2021
  !Purpose: To calculate the average number of electrons in single particle state using Fermi Dirac distibution

  !Question 1: When Fermi energy equal to Energy at State i, I get n=0.5. For fermi energy larger than Energy i,
  !as temperature increases, n decreases. Temperatures must be in order of magnitude e8 to see any signficant decrease in n 

  !Question 2: For Fermi energy>>Energy if state i, n tends to 1.  For Fermi energy<<Energy if state i, n tends to 0
!Question 3: To get n approx. equal 0.02, energy of state i needs to be approx. equal to 0.2. I was unable to find n approx. equal to 0.998 for Fermi energy = 0. As I decreased energy of state i, n increased no further than 0.5.


program Fermi_Dirac
  implicit none

  !Variable dictionary
  real(8) :: electron_number !average number of electrons in single particle state i.
  real(8) :: energy_of_i !Energy of state i. Units: eV
  real(8) :: fermi_energy !Fermi energy/work to add/remove electron. Units: eV
  real(8) :: temp_celsius !Temperature. Units : Celsius
  real(8) :: temp_kelvin  !Temperature. Units: Kelvin
  real(8) :: gamma        !Variable gamma in Fermi Dirac distribution
  real(8), parameter :: K_B = 8.617e-5 !Boltzmann constant. Units: eV/K

  write(*,*) "Enter value for Fermi Energy and Energy of State i" !Prompting user to input fermi energy and energy of state i
  read(*,*) fermi_energy, energy_of_i
  write(*,*) "Enter temperature in CELSIUS"
  read(*,*) temp_celsius

  temp_kelvin = temp_celsius + 273.15 !Converting celsius to kelvin

  gamma = (energy_of_i - fermi_energy)/(K_B * temp_kelvin) !Defining gamma

  write(*,*)"The value of gamma is ", gamma
  electron_number = 1.0/(exp(gamma) + 1.0)


  write(*,*) "The average number of electrons in single particle state with energy",energy_of_i, "eV and &

       Fermi energy ", fermi_energy, "eV is",electron_number




stop 0
end program Fermi_Dirac
