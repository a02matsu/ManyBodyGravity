module subroutines
use parameters
implicit none

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate forces
subroutine calc_force(F,X)
use parameters
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
use parameters
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
