  ! Author: Nkeiru Ubadike
  ! Purpose: Solve a 2D poisson problem with the relaxation method
  ! Date: 4/4/2021
program poisson
  implicit none
  real(16),parameter :: four_pi=16.0*atan(1.0)
  integer, parameter :: ni = 20, nj= 20 ! input cell number in the x and y direct
  real, parameter :: h_size = 1.0 ! input cell size (cm)
  integer :: qi,qj ! input positione of charge density 
  real :: q_mag ! input magnitude of charge density 
  integer :: ii,jj, mm ! For loops
  real(16) :: change,accuracy ! To determine convergence
  ! Allocatable arrays to store potentials and charge density
  real(16),allocatable :: uij(:,:), uij_new(:,:), q_den(:,:)

  ! For gnuplot file
  integer :: lun_gnu, lun_gnu2
  real :: x_plot,y_plot

  
 !Prompt user to enter desired accuracy
  write(*,*) "Enter desired accuracy:"
  read(*,*) accuracy

 
  
  ! Allocate arrays (including ghost layers)
  allocate(uij(0:ni+1,0:nj+1))
  allocate(uij_new(0:ni+1,0:nj+1))
  allocate(q_den(0:ni+1,0:nj+1))

  ! initialize potential everywhere to be zero
  uij=0.0
  uij_new=0.0

  !======Modified code ========================
  ! initialize charge density with a gradientt
  Charge_density: do mm =1, ni
     q_den(mm, 1:nj) = mm
     
  end do Charge_density
  
  !Write charge density in table form
  do mm= 0, ni +1 
  write(*,'(*(es16.4e2))') q_den(mm,:)
end do

!===================================================

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


  ! Write potential out in table form
  do ii=0,ni+1
     write(*,'(*(es16.4e2))') uij_new(ii,:) 
  end do


  ! BELOW PRODUCES GNUPLOT FILE FOR PLOTTING:

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

!========MODIFIED CODE: PLOTTING THE CHARGE GRADIENT==================!

  ! Open data file for gnuplotting
  open(newunit=lun_gnu2,file='gnu_density.dat',status='replace')
  
  ! Write gnu commands at the top of the file
  write(lun_gnu2,*) "set terminal png"
  write(lun_gnu2,*) "set output 'plot_density.png'"
  write(lun_gnu2,*) "plot '-' using 1:2:3 with image"
  
  
  ! Write charge density out to be plotted with gnuplot
  y_plot = -0.5*h_size    ! Initialize y coordinate of cell
  do jj=0,nj+1
       x_plot = -0.5*h_size    ! Initialize x coordinate of cell
       do ii=0,ni+1
          
        write(lun_gnu2,'(2f10.4,es20.8e2)') x_plot,y_plot,q_den(ii,jj)

        ! iterate x coordinate
        x_plot=x_plot+h_size
        
     end do

     write(lun_gnu2,*) " " ! Blank line between rows
     
     ! iterate y coordinate
     y_plot=y_plot+h_size
        
  end do

  close(lun_gnu2)


  ! deallocate arrays
  deallocate(uij)
  deallocate(uij_new)
  deallocate(q_den)
  

  stop 0
  
end program poisson
