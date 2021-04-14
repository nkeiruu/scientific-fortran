  ! Author: Nkeiru Ubadike
  ! Purpose: Solve a 2D poisson problem with the relaxation method using modules, subroutines
! Date: 4/12/2021

module mod_poisson
  implicit none
  ! Allocatable arrays to store potentials and charge density
  real(16), allocatable :: uij_new(:,:), q_den(:,:)
end module mod_poisson


program poisson
  use mod_poisson
  implicit none
  real(16),parameter :: four_pi=16.0*atan(1.0)
  integer :: ni,nj ! input cell number in the x and y direct
  real(16) :: h_size ! input cell size (cm)
  integer :: qi,qj ! input positione of charge density 
  real :: q_mag ! input magnitude of charge density 
  integer :: ii,jj ! For loops
  real(16) :: change,accuracy ! To determine convergence
  

  ! For gnuplot file
  

  
  ! Prompt user for inputs
  write(*,*) "Enter number of cells in x and y, and cell size:"
  read(*,*) ni,nj,h_size
  write(*,*) "Enter x and  y and magnitude of charge density:"
  read(*,*) qi,qj,q_mag
  write(*,*) "Enter desired accuracy:"
  read(*,*) accuracy

 
  ! Test to make sure charge density is inside the box
  if (qi > ni .or. qj > nj) then
     write(*,*) "The charge density must be inside the box."
     stop 1
  end if

  ! Allocate arrays (including ghost layers)
  !allocate(uij(0:ni+1,0:nj+1))
  allocate(uij_new(0:ni+1,0:nj+1))
  allocate(q_den(0:ni+1,0:nj+1))

  ! initialize potential everywhere to be zero
  !uij=0.0
  uij_new=0.0
  ! initialize charge density
  q_den=0.0
  q_den(qi,qj) = q_mag

  !Calling subroutine to calculate potential via relaxation method
  call relax_method(ni,nj,accuracy,h_size)
 

  ! Write potential out in table form
  do ii=0,ni+1
     write(*,'(*(es16.4e2))') uij_new(ii,:) 
  end do


  ! Calling subroutine to write gnuplot commands to a file
  call write_gnu_file(ni, nj, h_size)


  ! deallocate arrays
 ! deallocate(uij)
  deallocate(uij_new)
  deallocate(q_den)
  

  stop 0
  
end program poisson



subroutine relax_method(ni, nj, accuracy, h_size)
  use mod_poisson
  implicit none

  
  integer, intent(in):: ni,nj ! input cell number in the x and y direct
  real(16), intent(in) :: h_size ! input cell size (cm)
  real(16), intent(in) :: accuracy ! To determine convergence
  integer :: ii, jj !Counter variable
  real(16):: change ! To determine convergence
  real(16),parameter :: four_pi=16.0*atan(1.0)
  real(16) :: uij(0:ni+1,0:nj+1) !Array of potential 

  uij = 0.0
  uij_new = 0.0
  ! Loop until exit criteria is met
  do
     ! Loop over cells
     change=0.0 ! NOTE THE PLACEMENT OF THIS INITIALIZATION
     do jj = 1, nj
        do ii = 1,ni
           
           ! Find new guess
           uij_new(ii,jj)=0.25*( uij(ii+1,jj)+uij(ii-1,jj)+uij(ii,jj+1) &
                & +uij(ii,jj-1)-four_pi*q_den(ii,jj)*h_size**2 )

           ! Store max change in potential for all cells
           if (abs(uij_new(ii,jj)-uij(ii,jj)) > change) then
              change=abs(uij_new(ii,jj)-uij(ii,jj))

           end if

        end do ! ii
     end do ! jj
           
     ! Test for convergence
     if (change < accuracy) then
        write(*,*) "Reached required accuracy"
        exit
     end if

     ! set uij to uij_new
     uij=uij_new
     
  end do ! iteration loop

end subroutine relax_method



subroutine write_gnu_file(ni,nj, h_size)
  use mod_poisson
  implicit none
  
  integer, intent(in):: ni,nj ! input cell number in the x and y direct
  real(16), intent(in) :: h_size ! input cell size (cm)
  real(16) :: change ! To determine convergence
  integer :: lun_gnu !integer to identify file
  real :: x_plot,y_plot ! x & y coordinate of box
  integer :: ii, jj !Counter variable
  real(16),parameter :: four_pi=16.0*atan(1.0)
  
  ! Open data file for gnuplotting
  open(newunit=lun_gnu,file='gnu_poisson.dat',status='replace')
  
  ! Write gnu commands at the top of the file
  write(lun_gnu,*) "set terminal png"
  write(lun_gnu,*) "set output 'plot_poisson.png'"
  write(lun_gnu,*) "plot '-' using 1:2:3 with image"
  
  
  ! Write potential out to be plotted with gnuplot
  y_plot = -0.5*h_size    ! Initialize y coordinate of cell
  do jj=0,nj+1
       x_plot = -0.5*h_size    ! Initialize x coordinate of cell
       do ii=0,ni+1
          
        write(lun_gnu,'(2f10.4,es20.8e2)') x_plot,y_plot,uij_new(ii,jj)

        ! iterate x coordinate
        x_plot=x_plot+h_size
        
     end do

     write(lun_gnu,*) " " ! Blank line between rows
     
     ! iterate y coordinate
     y_plot=y_plot+h_size
        
  end do

  close(lun_gnu)

end subroutine write_gnu_file
