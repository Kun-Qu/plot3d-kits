
MODULE mergep3d
CONTAINS


SUBROUTINE mergep3dfunc(ndatafiles, datafiles, &
                        nb,ni,nj,nk,  nvar,  newp3df)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndatafiles, nvar(:)
  CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: datafiles
  CHARACTER(LEN=*),INTENT(IN) :: newp3df
  INTEGER,POINTER,DIMENSION(:) :: ni,nj,nk
  INTEGER,INTENT(in) :: nb
  ! local
  INTEGER nvars, tmpint, b, n, i,j,k,l
  INTEGER,POINTER,DIMENSION(:) :: offset, funit,shift
  REAL(4),POINTER,DIMENSION(:,:,:,:) :: gdata
  
  nvars = SUM( nvar(1:ndatafiles) )
  OPEN(33,FILE=TRIM(newp3df),action='write', form='unformatted')
  WRITE(33) nb  
  WRITE(33) (ni(b),nj(b),nk(b),nvars, b=1,nb)
  
  ALLOCATE( offset(ndatafiles), funit(ndatafiles), shift(ndatafiles) )
  offset=0
  shift = 0
  DO n=1, ndatafiles
    funit(n) = 1000 + n
    OPEN(UNIT=funit(n),FILE=TRIM(datafiles(n)),action='read', form='unformatted')
    READ(funit(n)) tmpint
    READ(funit(n)) (tmpint,tmpint,tmpint,tmpint, b=1,nb)  
    offset(n) = FTELL(funit(n))
    
    shift(n+1) = shift(n) + nvar(n) 
  ENDDO
  
  
  DO b=1, nb
    ALLOCATE( gdata(ni(b),nj(b),nk(b), nvars) )
    DO n=1, ndatafiles
      READ(funit(n)) (( ((gdata(i,j,k,l),i=1,ni(b)),j=1,nj(b)),k=1,nk(b)  ), &
                          l=shift(n)+1,shift(n+1) )      
    ENDDO
    WRITE(33) (( ((gdata(i,j,k,l),i=1,ni(b)),j=1,nj(b)),k=1,nk(b)  ),   l=1,nvars )
  ENDDO
  
  

END SUBROUTINE


SUBROUTINE checkp3dfunc(ndatafiles, datafiles,  &
                        nb, ni, nj, nk, ierr)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndatafiles
  CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: datafiles
  INTEGER,POINTER,DIMENSION(:) :: ni,nj,nk
  INTEGER,INTENT(OUT) :: nb,  ierr
  
  ! local
  INTEGER :: n,b,db,nfunc,dfunc
  INTEGER,DIMENSION(:),POINTER :: di,dj,dk
  
  OPEN(33,FILE=TRIM(datafiles(1)),action='read', form='unformatted')
  READ(33) nb;  WRITE(*,*) 'nblock=',nb
  ALLOCATE(ni(nb),nj(nb),nk(nb),   &
           di(nb),dj(nb),dk(nb),    )
  READ(33) (ni(b),nj(b),nk(b),nfunc, b=1,nb)
  WRITE(*,*) 'done read'
  !DO n=1,nb
  !  WRITE(*,*) ni(n),nj(n),nk(n),nvar(n)
  !ENDDO
  CLOSE(33)
  
  ierr = 0
  DO n=1,ndatafiles
    OPEN(33,FILE=TRIM(datafiles(n)),action='read', form='unformatted')
    READ(33) db
    IF(db /= nb) THEN
      ierr = 1
      CLOSE(33)
      RETURN
    ENDIF
    
    READ(33) (di(b),dj(b),dk(b),dfunc, b=1,nb)
    DO b=1,nb
      IF(di(b) /= ni(b) .OR. dj(b) /= nj(b) .OR. dk(b) /= nk(b) ) THEN
      
        WRITE(*,*) 'block ',b, di(b),dj(b),dk(b), ni(b),nj(b),nk(b)
        ierr = 2
        CLOSE(33)
        RETURN
      ENDIF
    ENDDO
    
    
    
    CLOSE(33)
  ENDDO
END SUBROUTINE


SUBROUTINE getvarnames(ndatafiles, datafiles,varnames, nvar)
  IMPLICIT NONE
  INTEGER,INTENT(OUT) :: ndatafiles
  INTEGER,DIMENSION(:), INTENT(OUT) :: nvar
  CHARACTER(LEN=*),DIMENSION(:),INTENT(OUT) :: varnames,datafiles
  
  ! local 
  CHARACTER(LEN=128) :: tmpstr
  
  INTEGER :: IO, argn,n,i
  LOGICAL :: notend
  
  argn = command_argument_count()
  ndatafiles = (argn - 2)/2
  WRITE(*,*) 'argn=',argn,'ndatafiles =',ndatafiles 
  
  i=0
  DO n=1,ndatafiles
    nvar(n)=0
    CALL get_command_argument(n+n+1,  datafiles(n))
    CALL get_command_argument(n+n+2,  tmpstr)
    !WRITE(*,*) tmpstr
    OPEN(33,FILE=TRIM(tmpstr))
    
    notend = .TRUE.
    DO WHILE (notend)
      READ(33, *, IOSTAT = IO ) tmpstr
      IF(IO>=0) THEN
        nvar(n) = nvar(n) + 1
        i = i + 1
        varnames(i) = tmpstr
        WRITE(*,*) i, TRIM( varnames(i) )
      ELSE
        notend = .FALSE.
      ENDIF
    ENDDO
    CLOSE(33)  
    
  ENDDO
  !DO n=1,i
  !  WRITE(*,*) TRIM(  varnames(n)  )
  !ENDDO
  !WRITE(*,*) nvar(1:ndatafiles)
END SUBROUTINE

SUBROUTINE mergename(ndata, nvar, varnames, namefile)
  INTEGER,INTENT(IN) :: ndata
  INTEGER,DIMENSION(:),INTENT(IN) :: nvar
  CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: varnames
  CHARACTER(LEN=*),INTENT(IN) :: namefile
  ! local
  INTEGER :: i,j,n
  OPEN(33,FILE=TRIM(namefile))
  
  j = 0
  DO n=1, ndata
  DO i=1, nvar(n)
      j=j+1
      WRITE(33, *) TRIM(varnames(j))
  ENDDO    
  ENDDO

END SUBROUTINE
END MODULE


PROGRAM main
  USE mergep3d
  IMPLICIT NONE
  INTEGER :: ndata,nvar(1024),nb,ierr
  INTEGER,POINTER,DIMENSION(:) :: ni,nj,nk
  CHARACTER(LEN=64) :: fnames(1024),varnames(1024)
  
  CALL getvarnames(ndata, fnames, varnames, nvar)
  CALL checkp3dfunc(ndata, fnames,  nb,ni,nj,nk,  ierr)
  WRITE(*,*) 'ierr=',ierr
  CALL mergep3dfunc(ndata, fnames,  nb,ni,nj,nk,  nvar,  'merge.p3df')
  
  CALL mergename(ndata, nvar, varnames, 'merge.nam')
END




