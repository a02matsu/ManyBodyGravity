!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! global parameters
module parameters
implicit none

integer :: DIM
integer :: NUM
double precision :: time

double precision :: Grav
double precision, allocatable :: MASS(:)


integer :: Ntau


character(128) :: INPUT_FILE_NAME="input.dat"
character(128) :: PAR_FILE_NAME="parameters.dat"
character(128) :: LastConfig_FILE_NAME
character(128) :: XV_FILE_NAME
character(128) :: FinalConfig_FILE_NAME

integer, parameter :: INPUT_FILE=10
integer, parameter :: PAR_FILE=11
integer, parameter :: LastConfig_FILE=12
integer, parameter :: XV_FILE=13
integer, parameter :: FinalConfig_FILE=14

integer :: job_number 
integer :: new_config
double precision :: deltaT
double precision :: totalT

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine set_parameters
implicit none

integer i

!!!!! read parameter file !!!!
open(PAR_FILE, file=PAR_FILE_NAME, status='old', action='READ')

read(PAR_FILE,*) DIM
read(PAR_FILE,*) NUM
read(PAR_FILE,*) Grav
allocate( MASS(1:NUM) )
do i=1,NUM
  read(PAR_FILE,*) MASS(i)
enddo

close(PAR_FILE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!! read input file !!!!
open(INPUT_FILE, file=INPUT_FILE_NAME, status='old', action='READ')

read(INPUT_FILE,*) job_number
read(INPUT_FILE,*) new_config
read(INPUT_FILE,*) totalT
read(INPUT_FILE,*) deltaT

close(INPUT_FILE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Grav=1d0
do i=1,NUM
  MASS(i)=1d0
enddo

NTAU=totalT/deltaT

if( job_number == 0 ) then
  LastConfig_FILE_NAME="lastconf.dat"
else
  write(LastConfig_FILE_NAME, '("config_",i4.4)') job_number-1
endif

end subroutine set_parameters


end module parameters


