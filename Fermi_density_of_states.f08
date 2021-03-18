  !Name: Nkeiru Ubadike
!Date: 3/10/2021
!Purpose: To calculate Fermi level and density of states
!for a free electron gas
!Fermi Energy: 11.667800929441862eV
!DOS: 9.6338818446982295E+027 eV


program Fermi_dos
  implicit none

  !Variable dictionary
  real(8) :: density ! Density of electrons(Units: m^-3)
  real(8) :: energy_ev !Energy. (Units: eV)
  real(8) :: energy_J !Energy. (Units: joules)
  real(8) :: fermi_level_J !Fermi level for free electron gas
  real(8) :: fermi_level_eV !Fermi level for free electron gas (in eV)
  real(8) :: dos  !Density of electron states
  real(8), parameter :: HBAR  = 1.0545718d-34 !Reduced Planck. Units: J*s
  real(8), parameter :: K_B = 8.617e-5 !Boltzmann constant. Units: eV/K
  real(8), parameter :: PI = 4.0*atan(1.0_8) !pi with 8 byte precision
  real(8), parameter :: MASS_E = 9.10938356d-31 !mass of electron in kg
  real(8), parameter :: EV_TO_J = 1.60218d-19 ! eV per Joule
  real:: test

  !Prompting user to input fermi energy and energy of state i
  write(*,*) "Enter value for density of electrons (units: m^-3) and energy (eV)" 
  read(*,*) density, energy_ev

  !Converting energy in eV to joules
  energy_J = energy_eV*EV_TO_J

  
  !Defining Fermi Level (joules)
  fermi_level_J = (HBAR**2)*(3*(PI**2)*density)**(2.0/3.0)/(2*MASS_E)
  fermi_level_eV = fermi_level_J/EV_TO_J !Converting fermi level to eV
  
  !Writing fermi energy to terminal
  write(*,*)"Fermi energy (J): ",  fermi_level_J
  write(*,*)"Fermi energy (eV): ",  fermi_level_eV
  !write(*,*)"pi: ", PI
  
  !Defining density of states
  dos = (3.0/2.0)*(density/fermi_level_eV)*sqrt(energy_eV/fermi_level_eV)

  
  !Testing if user energy<Fermi Energy
  if(energy_eV<fermi_level_eV)then
     write(*,*) "Density of Electron States:", dos, "eV"
  else if(energy_eV>fermi_level_eV) then
     write(*,*) "Energy is greater than Fermi level" 
  end if
  




stop 0
end program Fermi_dos
