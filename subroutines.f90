module subroutines
use global_parameters
implicit none

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine set_parameters
use global_parameters
implicit none

integer SNUM
integer i,n
double precision s_mass


!!!!! read parameter file !!!!
open(PAR_FILE, file=PAR_FILE_NAME, status='old', action='READ')

read(PAR_FILE,*) DIM
read(PAR_FILE,*) NUM
read(PAR_FILE,*) Grav
read(PAR_FILE,*) SNUM
allocate( MASS(1:NUM) )
MASS=1d0
do i=1,SNUM
  read(PAR_FILE,*) n, s_mass
  MASS(n)=s_mass
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
  LastConfig_FILE_NAME="CONFIG/lastconf.dat"
else
  write(LastConfig_FILE_NAME, '("CONFIG/config_",i4.4)') job_number-1
endif

end subroutine set_parameters


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Initial values
subroutine set_initial(X,V)
use global_parameters
implicit none

double precision, intent(inout) :: X(1:DIM,1:NUM), V(1:DIM, 1:NUM)
integer :: n, i
double precision :: r

integer :: seedsize
integer, allocatable :: seed(:)

double precision :: Ptot(1:DIM)

! ディレクトリ生成
call system('./make_directory.sh')

if( new_config == 1 ) then 
  job_number=0
  time=0d0

  call random_seed(size=seedsize)
  allocate( seed(seedsize) )
  do i=1,seedsize
    call system_clock(count=seed(i))
  enddo
  call random_seed(put=seed(:))

  Ptot=0d0
  do n=1,NUM
    do i=1,DIM
      call random_number(r)
      r = 2d0*r - 1d0
      X(i,n)=r
      !!!!!
      call random_number(r)
      r = 2d0*r - 1d0
      V(i,n)=r
      !!!!!
      Ptot(i) = Ptot(i) + mass(n)*r
    enddo
  enddo

  do n=1,NUM
    V(:,n) = V(:,n) - Ptot/(mass(n)*dble(NUM))
  enddo

else
  if( job_number== 0 ) then 
    LastConfig_FILE_NAME="CONFIG/lastconf"
  else
    write(LastConfig_FILE_NAME,'("CONFIG/lastconfig_",i4.4)') job_number-1
  endif

  open(unit=LastConfig_FILE, file=LastConfig_FILE_NAME, status='old', action='read',form='unformatted')

  read(LastConfig_File) job_number
  job_number=job_number+1

  read(LastConfig_File) time
  read(LastConfig_File) X
  read(LastConfig_File) V

  close(LastConfig_FILE)
endif


write(FinalConfig_FILE_NAME,'("CONFIG/lastconfig_",i4.4)') job_number
write(XV_FILE_NAME,'("OUTPUT/XV_",i4.4)') job_number
end subroutine set_initial



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate forces
subroutine calc_force(F,X)
use global_parameters
implicit none

double precision, intent(out) :: F(1:DIM, 1:NUM)
double precision, intent(in) :: X(1:DIM, 1:NUM)

double precision :: dist
integer :: n,m,i,j

F=0d0
do n=1,NUM
  do m=1,NUM
    if( m .ne. n) then
      call calc_dist(dist, X(:,m), X(:,n))
      do i=1, DIM
        F(i,n)=F(i,n)-MASS(n)*MASS(m)*(X(i,n)-X(i,m))/(dist*dist*dist)
      enddo
    endif
  enddo
enddo
F=F*Grav

end subroutine calc_force

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate distance
subroutine calc_dist(dist,pos1,pos2)
use global_parameters
implicit none

double precision, intent(out) :: dist
double precision, intent(in) :: pos1(1:DIM), pos2(1:DIM)

integer :: i

dist=0d0
do i=1,DIM
  dist=dist+(pos1(i)-pos2(i))*(pos1(i)-pos2(i))
enddo

dist = dsqrt(dist)

end subroutine calc_dist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 1st order time evolution
subroutine time_evolution_LeapFrog(Xstar,Vstar,Astar)
use global_parameters
implicit none

double precision, intent(inout) :: Astar(1:DIM,1:NUM)
double precision, intent(inout) :: Xstar(1:DIM,1:NUM)
double precision, intent(inout) :: Vstar(1:DIM,1:NUM)
double precision :: Astar2(1:DIM,1:NUM)
double precision :: Force(1:DIM,1:NUM)
integer n

Xstar = Xstar + deltaT*Vstar + 0.5d0*deltaT*deltaT*Astar

call calc_force(Force,Xstar)
do n=1,NUM
  Astar2(:,n)=Force(:,n)/Mass(n)
enddo
Vstar = Vstar + 0.5d0*deltaT*(Astar+Astar2)
Astar=Astar2

end subroutine time_evolution_LeapFrog

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 1st order time evolution
subroutine time_evolution_1st(Xstar,Vstar)
use global_parameters
implicit none

double precision :: Force(1:DIM,1:NUM)
double precision :: Xstar(1:DIM,1:NUM)
double precision :: Vstar(1:DIM,1:NUM)
integer n

call calc_force(Force,Xstar)
Xstar = Xstar + deltaT*Vstar
do n=1,NUM
  Vstar(:,n) = Vstar(:,n) + deltaT*Force(:,n)/Mass(n)
enddo

end subroutine time_evolution_1st

!****************************************************************
!****************************************************************
!***  Box-Muller method for generating Gaussian random number ***
!****************************************************************
!****************************************************************
SUBROUTINE BoxMuller(p,q)
implicit none

doubleprecision p,q,r,s,Pi

Pi=2d0*DASIN(1d0)
call random_number(r)
call random_number(s)

p=dsqrt(-2d0*dlog(r))*DSIN(2d0*Pi*s)
q=dsqrt(-2d0*dlog(r))*DCOS(2d0*Pi*s)

return

END SUBROUTINE BoxMuller

end module subroutines
