!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! global parameters
module global_parameters
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


end module global_parameters


