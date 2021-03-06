!!! module for global parameters
!include "global_parameters.f90"
!include "subroutines.f90"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MAIN
program main
use global_parameters
use subroutines
implicit none

double precision, allocatable :: Xstar(:,:) ! Xstar(1:DIM, 1:NUM)
double precision, allocatable :: Vstar(:,:) ! Vstar(1:DIM, 1:NUM)
double precision, allocatable :: Astar(:,:) ! Astar(1:DIM, 1:NUM)

double precision, allocatable :: Force(:,:) ! Force(1:DIM, 1:NUM)
integer :: i,n,k

call set_parameters

allocate( Xstar(1:DIM,1:NUM) )
allocate( Vstar(1:DIM,1:NUM) )
allocate( Astar(1:DIM,1:NUM) )
allocate( Force(1:DIM,1:NUM) )

call set_initial(Xstar,Vstar)
call calc_force(Force,Xstar)
do n=1,NUM
  Astar(:,n)=Force(:,n)/Mass(n)
enddo

open(unit=XV_FILE, file=XV_FILE_NAME, status='replace', action='write')
do k=0,Ntau-1
  time=time+deltaT
  !call time_evolution_1st(Xstar,Vstar)
  call time_evolution_LeapFrog(Xstar,Vstar,Astar)

  write(XV_FILE,'(E15.8,2X)',advance='no') time 
  do n=1,NUM
    do i=1,DIM
      write(XV_FILE,'(E12.5,2x)',advance='no') Xstar(i,n)
    enddo
  enddo
  do n=1,NUM
    do i=1,DIM
      write(XV_FILE,'(E12.5,2x)',advance='no') Vstar(i,n)
    enddo
  enddo
  write(XV_FILE,*)
enddo
close(XV_FILE)

open(unit=FinalConfig_FILE, file=FinalConfig_FILE_NAME, status='replace', form='unformatted')
write(FinalConfig_File) job_number
write(FinalConfig_FILE) time 
write(FinalConfig_FILE) Xstar
write(FinalConfig_FILE) Vstar
close(FinalConfig_FILE)

call system('&
  cd CONFIG; &
  FILE=$(ls lastconfig_* | tail -1); &
  LINK="lastconf"; if [ -e "$LINK" ]; then /bin/rm $LINK; &
  fi; ln -s $FILE $LINK')


end program main


