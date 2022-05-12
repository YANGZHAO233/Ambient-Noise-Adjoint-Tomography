c
c* The sample of test driver for FTAN with phase match filter for
c* subroutines aftanpg and aftanipg
c
      use addition
      implicit none
      integer*4 n, npoints, nfin, nfout1, nfout2, ierr, nprpv
      real*8    t0, dt, delta, vmin, vmax, tmin, tmax
      real*8    snr,tresh,ffact1,ffact2,perc,taperl,fmatch,piover4
      real*4    sei(32768)
      real*8    arr1(8,100),arr2(7,100)
      real*8    tamp, ampo(32768,100), pred(300,2)
      integer*4 nrow, ncol, npred
      real*8    prpvper(300),prpvvel(300)
      character*250 name,cmd
      integer*4 i,i0,sac,nargc,iargc

      character*1 isPerCut, isOutput, isVerbose
      integer*4 k, nerr
      real*8 nLambda, x(1), y(1)

      sac = 1
      i0  = 1

c---  input command line arguments treatment
      nargc = iargc()
      if(nargc.ne.1) then
          write(*,*) "Usage: AFTAN_PROG sacfile\n"
          stop
      endif

      open(unit=99, file='input.dat', status='old')
      do k = 1, 27
          read(99,"(a)") cmd
      end do
      read(99,*) piover4
      read(99,*) vmin,vmax
      read(99,*) tmin,tmax
      read(99,*) isPerCut, nLambda
      read(99,*) isOutput
      if (tmin<1) isOutput = 'Y'
      read(99,*) tresh
      read(99,*) ffact1, ffact2
      read(99,*) taperl
      read(99,*) snr
      read(99,*) fmatch
      do k = 1, 6
          read(99,"(a)") cmd
      end do
      read(99,*) isVerbose
      close(unit=99)


      call getarg(1,name)

      if (isVerbose == 'Y' .or. isVerbose == 'y') write(*,'(a,a)') 'AFTAN: ', trim(name)
c      cmd = trim(name)//'_PHP'
      cmd = 'ref1Dmod.dat'
      open(11,file=cmd,status='old', iostat=nerr)
      if (nerr/=0) then
          write(*,*)'Error: No phase velocity reference file!'
          return
      end if
      nprpv = 1
  3   read(11,*,end=4) prpvper(nprpv),prpvvel(nprpv)
      nprpv = nprpv+1
      goto 3
  4   nprpv = nprpv-1
      close(11)
c
c    read SAC or ascii data
c
      call readdata(sac,name,n,dt,delta,t0,sei,ierr)

      if (isPerCut == 'Y' .or. isPerCut == 'y') then
          do k = 1, int(tmax)
              x(1) = k
              call linear_interpo(prpvper(1:nprpv), prpvvel(1:nprpv), x, y, nerr)
              if (nLambda*x(1)*y(1)>delta .and. k<tmax) then
                  tmax = k
                  EXIT
              end if
          end do
      end if

      nfin    = 64
      npoints = 5
      perc    = 50.0d0

c---  FTAN with phase with phase match filter. First Iteration.


      call aftanpg(piover4,n,sei,t0,dt,delta,vmin,vmax,tmin,tmax,tresh,
     *        ffact1,perc,npoints,taperl,nfin,snr,nprpv,prpvper,prpvvel,
     *        nfout1,arr1,nfout2,arr2,tamp,nrow,ncol,ampo,ierr)
      if (isOutput == 'Y' .or. isOutput == 'y') then
      call printres(dt,delta,nfout1,arr1,nfout2,arr2,tamp,nrow,ncol,ampo,
     *              ierr,name,"_1")
      end if

      if (tmin>=1) call write_data(arr1, nfout1, arr2, nfout2, trim(name)//'_1')

      if(nfout2 .eq. 0) goto 2

c---  Make prediction based on the first iteration

      npred = nfout2
      tmin = arr2(2,1)
      tmax = arr2(2,nfout2)
      do i = 1,nfout2
          pred(i,1) = arr2(2,i)
          pred(i,2) = arr2(3,i)
      enddo

c---  FTAN with phase with phase matching filter. Second Iteration.


      call aftanipg(piover4,n,sei,t0,dt,delta,vmin,vmax,tmin,tmax,tresh,
     *        ffact2,perc,npoints,taperl,nfin,snr,fmatch,npred,pred,
     *        nprpv,prpvper,prpvvel,
     *        nfout1,arr1,nfout2,arr2,tamp,nrow,ncol,ampo,ierr)
      if (isOutput == 'Y' .or. isOutput == 'y') then
      call printres(dt,delta,nfout1,arr1,nfout2,arr2,tamp,nrow,ncol,ampo,
     *              ierr,name,"_2")
      end if

      if (tmin>=1) call write_data(arr1, nfout1, arr2, nfout2, trim(name)//'_2')

   2  end
