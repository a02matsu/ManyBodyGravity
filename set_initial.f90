!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Initial values
subroutine set_initial(X,V)
use parameters
implicit none

double precision :: X(1:DIM,1:NUM), V(1:DIM, 1:NUM)
integer :: n, i
double precision :: r

double precision :: Ptot(1:DIM)

if( new_config == 1 ) then 
  job_number=0
  time=0d0

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
    LastConfig_FILE_NAME="lastconf"
  else
    write(LastConfig_FILE_NAME,'("lastconfig_",i4.4)') job_number-1
  endif

  open(unit=LastConfig_FILE, file=LastConfig_FILE_NAME, status='old', action='read',form='unformatted')

  read(LastConfig_File) job_number
  job_number=job_number+1

  read(LastConfig_File) time
  read(LastConfig_File) X
  read(LastConfig_File) V

  close(LastConfig_FILE)
endif


write(FinalConfig_FILE_NAME,'("lastconfig_",i4.4)') job_number
write(XV_FILE_NAME,'("XV_",i4.4)') job_number



end subroutine set_initial

