MODULE Main_Proc

    USE, INTRINSIC :: ISO_C_BINDING     ! Allow to define the equivalents of C data types (e.g. C_PTR, C_INT)
    USE my_definition                   ! imported module which contains data type and variable definitions.
    USE sac_io
    USE date_time
    USE string
    USE my_data_io
    USE math
    IMPLICIT NONE

    INCLUDE 'fftw3.f03' ! fftw3.f03: contains the Fortran constant definitions and interface definitions (e.g. FFTW_BACKWARD)

CONTAINS


    ! =======================================================================================
    ! Process the sac file for one particular record and fill in the sdb.rec info.
    ! evpath: event path containing the sac data [input]
    ! sdb: sac_db struct [input and output]
    ! iev: event iterator [input]
    ! ist: station iterator [input]
    ! channel: channel name [input]
    ! isverbose: verbose indicator [input]
    ! =======================================================================================
    SUBROUTINE mk_one_rec(evpath, sdb, iev, ist, channel, isverbose)
        IMPLICIT NONE

        CHARACTER(len=*), INTENT(IN) :: evpath
        TYPE(sac_db), INTENT(INOUT) :: sdb
        INTEGER, INTENT(IN) :: iev, ist
        CHARACTER(len=*), INTENT(IN) :: channel
        LOGICAL, INTENT(IN) :: isverbose

        CHARACTER(len=200) :: str_temp
        LOGICAL :: alive
        CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: strArray
        INTEGER :: nerr, nfrac, nstrArray
        REAL(DBL) :: frac, dt, t0

        ! ***************************************************************
        ! Copy one station sac file from evpath to target folder if it exists.
        ! ***************************************************************
        str_temp = TRIM(evpath)//'/'//TRIM(sdb%st(ist)%n_name)//'.'//TRIM(channel)//'.SAC'

        ! Return if it doesn't exist.
        INQUIRE(FILE=str_temp, EXIST=alive)
        IF (.NOT. alive) RETURN

        ! ***************************************************************
        IF (isverbose) THEN
            CALL split_string(evpath, '/', strArray, nstrArray)
            str_temp =  strArray(nstrArray)
            WRITE(*,"('Event: ',A,'   Station: ',A)") TRIM(str_temp), TRIM(sdb%st(ist)%n_name)
            IF (ALLOCATED(strArray)) DEALLOCATE(strArray)
        END IF

        ! Copy the sac file to target event folder
        str_temp = 'cp '//TRIM(evpath)//'/'//TRIM(sdb%st(ist)%n_name)//'.'//TRIM(channel)//'.SAC '//TRIM(sdb%ev(iev)%name)//'/'
        CALL SYSTEM(str_temp)

        ! ***************************************************************
        ! Correct the fraction time and set the reference time to be the beginning time (b=0)).
        ! ***************************************************************
        str_temp = TRIM(sdb%ev(iev)%name)//'/'//TRIM(sdb%st(ist)%n_name)//'.'//TRIM(channel)//'.SAC'
        CALL correct_sac_file(str_temp, frac, nfrac, dt, t0, nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: correct_sac_file failed! '//TRIM(str_temp)
            RETURN
        END IF

        ! ***************************************************************
        ! Fill the elements in this record.
        ! ***************************************************************
        IF (nfrac /= 0) THEN
            sdb%rec(iev, ist)%nrec = nfrac
            sdb%rec(iev, ist)%frac = frac
            sdb%rec(iev, ist)%dt = REAL(dt)
            sdb%rec(iev, ist)%t0 = t0
            sdb%rec(iev,ist)%name = TRIM(str_temp)
            sdb%rec(iev,ist)%channel = TRIM(channel)
        END IF

    END SUBROUTINE mk_one_rec


    ! =======================================================================================
    ! Correct the fraction time and set the reference time to be the beginning time (b=0)).
    ! if t=10.6s, then t=11s, frac=-0.4s; if t=10.4s, then t=10s, frac=0.4s
    ! fname: sac filename [input]
    ! frac: fraction time of this sac file  [output]
    ! nfrac: npts of this sac header [output]
    ! dt: time sampling interval [output]
    ! t: beginning time of the first data point [output]
    ! nerr: status indicator [output]
    ! =======================================================================================
    SUBROUTINE correct_sac_file(fname, frac, nfrac, dt, t, nerr)
        IMPLICIT NONE

        CHARACTER(len=*), INTENT(IN) :: fname
        REAL(DBL), INTENT(OUT) :: frac, dt, t
        INTEGER, INTENT(OUT) :: nfrac, nerr

        TYPE(sachead) :: shd
        REAL(SGL), ALLOCATABLE, DIMENSION(:) :: seis_data
        INTEGER :: nf
        REAL(DBL) :: sec, frac1

        ! ***************************************************************
        ! Read the sac file
        CALL sacio_readsac(fname, shd, seis_data, nerr)
        IF (nerr /= 0) THEN
            WRITE(*,*) "Error: Can not read: "//TRIM(fname)
            RETURN
        END IF

        ! npts and dt
        nfrac = shd%npts
        dt = shd%delta

        ! Get the initial beginning time of the first data point.
        t = htoepoch(shd%nzyear,shd%nzjday,shd%nzhour,shd%nzmin,shd%nzsec+shd%nzmsec/1000.0D0+shd%b)

        ! ***************************************************************
        ! Make the time fraction correction
        ! ***************************************************************
        frac1 = t - FLOOR(t)
        nf = INT(frac1/dt)
        frac = t - (FLOOR(t)+nf*dt)
        t = FLOOR(t) + nf*dt
        IF (frac > 0.5*dt) THEN
            t = t + dt
            frac = frac - dt
        END IF

        ! ***************************************************************
        ! Change the sac header to make sure b=0
        ! ***************************************************************
        CALL epochtoh(t,shd%nzyear,shd%nzjday,shd%nzhour,shd%nzmin,sec)
        shd%nzsec = INT(sec)
        shd%nzmsec = INT((sec-shd%nzsec)*1000.0)
        shd%b = 0.0

        ! Overwrite the sac file
        CALL sacio_writesac(fname, shd, seis_data, nerr)
        IF (nerr /= 0) THEN
            WRITE(*,*) "Error: Can not overwrite: "//TRIM(fname)
            RETURN
        END IF

        IF (ALLOCATED(seis_data)) DEALLOCATE(seis_data)

    END SUBROUTINE correct_sac_file


    ! =======================================================================================
    ! Write the info database into a ascii file if isrecord is true.
    ! sdb: sac_db struct [input]
    ! filename: name of the output ascii file [input]
    ! =======================================================================================
    SUBROUTINE sacdb_to_asc(sdb, filename)
        IMPLICIT NONE

        TYPE(sac_db), INTENT(IN) :: sdb
        CHARACTER(len=*), INTENT(IN) :: filename

        TYPE(sachead) :: shd
        INTEGER :: iev, ist, nerr, nstrArray
        CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: strArray
        CHARACTER(len=200) :: str_temp

        ! ***************************************************************
        OPEN(UNIT=17, FILE=filename, STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)

        ! ***************************************************************
        ! Write the number of stations and events in the first line.
        ! ***************************************************************
        WRITE(17,"(A,I8,5X,A,I6)") 'Number of events:',sdb%nev,'Number of stations:',sdb%nst
        WRITE(17,"(A)") '===================================================================='

        ! ***************************************************************
        ! Write the data record.
        ! ***************************************************************
        DO iev = 1, sdb%nev
            DO ist = 1, sdb%nst
                CALL split_string(sdb%ev(iev)%name, '/', strArray, nstrArray)
                str_temp =  strArray(nstrArray)
                WRITE(17,"(A20,$)") str_temp

                IF (sdb%rec(iev,ist)%nrec == 0) THEN    ! Write "NO DATA" if rec[ie][is] == 0
                    WRITE(17,"(A)") 'NO DATA at '//TRIM(sdb%st(ist)%n_name)
                ELSE
                    ! Read the sac file to retrive its header information into shd struct.
                    CALL sacio_readhead(sdb%rec(iev,ist)%name, shd, nerr)

                    ! Write sac file name, t0 (reference time), frac(time fraction),
                    ! data length (npts*delta)
                    CALL split_string(sdb%rec(iev,ist)%name, '/', strArray, nstrArray)
                    str_temp =  strArray(nstrArray)
                    IF (ALLOCATED(strArray)) DEALLOCATE(strArray)

                    WRITE(17,"(A25,3X,'t0: ',I4,'/',I3.3,'/',I2.2,':',I2.2,':',A,4X,'Frac:',&
                    &F10.5,'s',4X,'Record Length:',F10.2,'s')")str_temp,shd%nzyear,&
                    shd%nzjday,shd%nzhour,shd%nzmin,TRIM(padzero(shd%nzsec+0.001*shd%nzmsec,2,2)),&
                    sdb%rec(iev,ist)%frac,shd%delta*(shd%npts-1)
                END IF
            END DO
        END DO

        IF (ALLOCATED(strArray)) DEALLOCATE(strArray)

        CLOSE(UNIT=17)

    END SUBROUTINE sacdb_to_asc


    ! =======================================================================================
    ! Remove the instrument response
    ! sdb: sac_db struct [input]
    ! iev: event iterator [input]
    ! ist: station iterator [input]
    ! my_id: process id number [input]
    ! f1, f2, f3, f4: freqency limits [input]
    ! isverbose: verbose indicator [input]
    ! =======================================================================================
    SUBROUTINE remove_RESP(sdb, iev, ist, my_id, f1, f2, f3, f4, isverbose)
        IMPLICIT NONE

        TYPE(sac_db), INTENT(IN) :: sdb
        INTEGER, INTENT(IN) :: iev, ist, my_id
        REAL(SGL), INTENT(IN) :: f1, f2, f3, f4
        LOGICAL, INTENT(IN) :: isverbose

        CHARACTER(len=3) :: str_temp

        ! ***************************************************************
        IF (f1>0 .AND. f2>f1 .AND. f3>f2 .AND. f4>f3) THEN

            IF (sdb%rec(iev,ist)%nrec > 0) THEN

                ! Each process has its own sac script
                WRITE(str_temp, '(I3.3)') my_id

                OPEN(UNIT=18, FILE=TRIM(str_temp)//'.txt', STATUS='REPLACE', ACTION='WRITE')
                WRITE(18, "(A)") 'sac<<EOF'
                WRITE(18, "(A)") 'r '//TRIM(sdb%rec(iev,ist)%name)
                WRITE(18, "(A)") 'rmean'
                WRITE(18, "(A)") 'rtrend'
                WRITE(18, "(A,F10.4,F10.4,F10.4,F10.4)") 'transfer from polezero subtype PZfiles/'&
                //TRIM(sdb%st(ist)%n_name)//'..LHZ.PZ to vel freq ',f1, f2, f3, f4
                WRITE(18, "(A)") 'mul 1.0e9'
                WRITE(18, "(A)") 'w over'
                WRITE(18, "(A)") 'quit'
                WRITE(18, "(A)") 'EOF'
                CLOSE(UNIT=18)

                ! ***************************************************************
                ! Remove the instrument response to obtain the velocity mesurement
                ! using transfer command in SAC with frequency limits [f1,f2,f3,f4].
                ! ***************************************************************
                IF (isverbose) THEN
                    CALL SYSTEM('sh '//TRIM(str_temp)//'.txt')
                ELSE
                    CALL SYSTEM('sh '//TRIM(str_temp)//'.txt > /dev/null')
                END IF

                CALL SYSTEM('rm -rf '//TRIM(str_temp)//'.txt')

            END IF

        ELSE

            WRITE(*,"(A)") 'ERROR: The corner periods should satisfy f1<f2<f3<f4'

        END IF

    END SUBROUTINE remove_RESP


    ! =======================================================================================
    ! Cut the data with fixed starting time and length
    ! sdb: sac_db struct [input]
    ! iev: event iterator [input]
    ! ist: station iterator [input]
    ! t0: starting time [input]
    ! tlen: data length [input]
    ! isverbose: verbose indicator [input]
    ! =======================================================================================
    SUBROUTINE cut_data(sdb, iev, ist, t0, tlen, isverbose)
        IMPLICIT NONE

        TYPE(sac_db), INTENT(IN) :: sdb
        INTEGER, INTENT(IN) :: iev, ist
        REAL(DBL), INTENT(IN) :: t0, tlen
        LOGICAL, INTENT(IN) :: isverbose

        TYPE(sachead) :: shd
        REAL(SGL), ALLOCATABLE, DIMENSION(:) :: seis_data
        CHARACTER(len=200) :: str_temp
        CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: strArray
        REAL(DBL) :: tend, dt, trb, tre
        INTEGER :: N, Nlen, Ngap, nerr, nstrArray

        IF (sdb%rec(iev,ist)%nrec > 0) THEN

            ! Record info.
            dt = sdb%rec(iev,ist)%dt
            N = sdb%rec(iev,ist)%nrec       ! N: number of real data points
            Nlen = INT(tlen/dt)+1           ! Nlen: number of desired data points

            ! tend: desired ending time of the signal
            tend = t0 + tlen

            ! trb: real data beginning time relative to the event time (2005_4_1_0_0_0) in seconds
            trb = sdb%rec(iev,ist)%t0 - sdb%ev(iev)%t0

            ! tre: real data ending time relative to the event time (2005_4_1_0_0_0) in seconds
            tre = trb + (N-1)*dt

            ! ***************************************************************
            ! If the real data beginning time is larger than t1 or the real data ending time
            ! is smaller than t2, the sac file will not be processed.
            ! ***************************************************************
            IF (trb>t0 .OR. tre<tend) THEN
                IF (isverbose) THEN
                    CALL split_string(sdb%rec(iev,ist)%name, '/', strArray, nstrArray)
                    str_temp =  TRIM(strArray(nstrArray-1))//'/'//TRIM(strArray(nstrArray))
                    WRITE(*,"(A,A,A,F10.2,A,F10.2,A)") 'Short length file: ', TRIM(str_temp), &
                    '  Beginning time:', trb, 's  Bad length:',(N-1)*dt,'s'
                    IF (ALLOCATED(strArray)) DEALLOCATE(strArray)
                END IF
                CALL SYSTEM('rm -rf '//TRIM(sdb%rec(iev,ist)%name))
                RETURN
            END IF

            ! Read the sac file.
            CALL sacio_readsac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data, nerr)

            Ngap = INT((t0-trb)/dt+0.5)

            ! Set the common time header information for the sac file.
            shd%npts = Nlen
            shd%nzyear = 2000
            shd%nzjday = 1
            shd%nzhour = 0
            shd%nzmin = 0
            shd%nzsec = 0
            shd%nzmsec = 0
            shd%b = 0.0
            shd%user1 = REAL(sdb%rec(iev,ist)%frac)

            ! Write the sac file after cut on [t0, t0+tlen]
            CALL sacio_writesac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data(Ngap+1:), nerr)

            IF (ALLOCATED(seis_data)) DEALLOCATE(seis_data)

        END IF

    END SUBROUTINE cut_data


    ! =======================================================================================
    ! Peform time fraction correction and apply the band-pass filtering
    ! sdb: sac_db struct [input]
    ! iev: event iterator [input]
    ! ist: station iterator [input]
    ! f1, f2, f3, f4: frequency limits [input]
    ! npow: power of cosine tapering function [input]
    ! isverbose: verbose indicator [input]
    ! =======================================================================================
    SUBROUTINE frac_filter4(sdb, iev, ist, f1, f2, f3, f4, npow, isverbose)
        IMPLICIT NONE

        ! the type of the dummy argumets should be the same as the real arguments defined in the main program.
        TYPE(sac_db), INTENT(IN) :: sdb
        INTEGER, INTENT(IN) :: iev, ist, npow
        REAL(SGL), INTENT(IN) :: f1, f2, f3, f4
        LOGICAL, INTENT(IN) :: isverbose

        REAL(SGL), ALLOCATABLE, DIMENSION(:) :: seis_data
        INTEGER :: n, Nfft, Nk, k, nerr, nstrArray
        REAL(DBL) :: fs, frac
        REAL(SGL) :: dt
        COMPLEX(DBL), ALLOCATABLE, DIMENSION(:) :: s, sf
        COMPLEX(DBL) :: ci, czero
        TYPE(C_PTR) :: plan
        TYPE(sachead) :: shd
        LOGICAL :: alive
        CHARACTER(len=200) :: str_temp
        CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: strArray

        ! ***************************************************************
        INQUIRE(FILE=TRIM(sdb%rec(iev,ist)%name), EXIST=alive)
        IF (.NOT. alive) RETURN

        ! Read the sac file
        CALL sacio_readsac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data, nerr)

        ! Obtain the useful header information.
        n = shd%npts
        dt = shd%delta
        frac = shd%user1

        ci = (0.0D0, 1.0D0)
        czero = (0.0D0, 0.0D0)

        ! Determine the power for FFT
        Nfft = 2**MAX(INT(LOG(REAL(n))/LOG(2.0))+1, 13)   ! Nfft: number of points for FFT

        fs = 1.0D0/(dt*Nfft)     ! fs: frequency interval

        ! Allocate memory for s and sf.
        ALLOCATE(s(Nfft), sf(Nfft), STAT=nerr)

        ! Initialize s with complex zero.
        DO k = 1, Nfft
            s(k) = czero
        END DO

        ! Fill s with real data.
        DO k = 1, n
            s(k) = seis_data(k)
        END DO

        ! Make backward FFT for the seismogram: s => sf
        plan = fftw_plan_dft_1d(Nfft, s, sf, FFTW_BACKWARD, FFTW_ESTIMATE)
        CALL fftw_execute_dft(plan, s, sf)
        CALL fftw_destroy_plan(plan)

        ! Make time fraction correction
        IF (DABS(frac) > 0.05D0*dt) THEN
            DO k = 1, Nfft/2
                sf(k) = sf(k) * CDEXP(ci*fs*frac*(k-1))
            END DO
        END IF

        ! Kill half spectra
        Nk = Nfft/2 + 1
        Do k = Nk+1, Nfft
            sf(k) = czero
        END DO

        ! Correct the ends
        sf(1) = sf(1) / 2.0D0
        sf(Nk) = DCMPLX(DREAL(sf(Nk)), 0.0D0)

        ! ***************************************************************
        ! Apply the band-pass filtering to the first half frequency band.
        ! ***************************************************************
        CALL band_pass(f1, f2, f3, f4, fs, Nk, npow, sf)

        ! Make forward FFT for the seismogram: sf => s
        plan = fftw_plan_dft_1d(Nfft, sf, s, FFTW_FORWARD, FFTW_ESTIMATE)
        CALL fftw_execute_dft(plan, sf, s)
        CALL fftw_destroy_plan(plan)

        ! Get the final result.
        DO k = 1, n
            seis_data(k) = 2.0*REAL(DREAL(s(k)))/Nfft  ! 2 is introduced because half of the spectra is set as complex zero.
        END DO

        CALL sacio_writesac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data, nerr)

        IF (ALLOCATED(s)) DEALLOCATE(s)
        IF (ALLOCATED(sf)) DEALLOCATE(sf)
        IF (ALLOCATED(seis_data)) DEALLOCATE(seis_data)

        IF (isverbose) THEN
            CALL split_string(sdb%rec(iev,ist)%name, '/', strArray, nstrArray)
            str_temp =  TRIM(strArray(nstrArray-1))//'/'//TRIM(strArray(nstrArray))
            IF (ALLOCATED(strArray)) DEALLOCATE(strArray)
            WRITE(*,"(A,' band-pass filtering and time fraction correction FINISHED.')") TRIM(str_temp)
        END IF

    END SUBROUTINE frac_filter4


    ! =======================================================================================
    ! band-pass filtering computed in the frequency domain
    ! f1, f2, f3, f4: frequency limits [input]
    ! fs: frequency interval [input]
    ! Nk: half-length of the data points in the frequency domain [input]
    ! npow: power of the cosine tapering function [input]
    ! sf: FFT values in complex form [input and output]
    ! =======================================================================================
    SUBROUTINE band_pass(f1, f2, f3, f4, fs, Nk, npow, sf)
        IMPLICIT NONE

        REAL(SGL), INTENT(IN) :: f1, f2, f3, f4
        REAL(DBL), INTENT(IN) :: fs    ! type of fs must conform the type declared in the main program (double precision).
        INTEGER, INTENT(IN) :: Nk, npow
        COMPLEX(DBL), DIMENSION(:), INTENT(INOUT) :: sf

        INTEGER :: i, j
        REAL(DBL) :: pi, temp, f
        REAL(DBL), DIMENSION(Nk) :: alpha

        pi = DATAN(1.0D0)*4.0D0

        ! Initialize alpha with zero
        DO i = 1, Nk
            alpha(i) = 0.0D0
        END DO

        DO i = 1, Nk
            f = (i-1)*fs

            ! Keep alpha to be zero if f<=f1
            IF (f<=f1) THEN
                CYCLE

            ! alpha=(1-cos((f-f1)/(f2-f1))) if f1<f<=f2
            ELSE IF (f<=f2) THEN
                temp = 1.0D0
                DO j = 1, npow
                    temp = temp*(1-DCOS(pi*(f-f1)/(f2-f1)))/2.0D0
                END DO
                alpha(i) = temp

            ! alpha=1 if f2<f<=f3
            ELSE IF (f<=f3) THEN
                alpha(i) = 1.0D0
            ELSE IF (f<=f4) THEN
                temp = 1.0D0
                DO j = 1, npow
                    temp = temp*(1+DCOS(pi*(f-f3)/(f4-f3)))/2.0D0
                END DO
                alpha(i) = temp
            END IF

            ! Keep alpha to be zero if f>f4

        END DO

        ! Apply the tapering.
        DO i = 1, Nk
            sf(i) = sf(i) * alpha(i)
        END DO

    END SUBROUTINE band_pass


    ! =======================================================================================
    ! Apply time domain normalization, spectra whitening, band-rejection filtering [optional]
    ! and band-pass filtering
    ! sdb: sac_db struct [input]
    ! iev: event iterator [input]
    ! ist: station iterator [input]
    ! my_id: process id number [input]
    ! tnorm: time normalization indicator [input]
    ! onebit: onebit normalization indicator [input]
    ! notch: notch indicator [input]
    ! f1, f2, f3, f4: frequency limits [input]
    ! tfilter: if earthquake band_pass filtering at [fr1 fr2] [input]
    ! fr1, fr2: period limits for earthquake band-pass filtering in time normalization [input]
    ! npow: power of cosine tapering function [input]
    ! nwt, nwf: half-window length time normalization and spectral whitening [input]
    ! freqmin: retaining factor for notch repressing [input]
    ! isverbose: verbose indicator [input]
    ! =======================================================================================
    SUBROUTINE white_reject_filter4(sdb,iev,ist,my_id,tnorm,onebit,notch,f1,f2,f3,f4,tfilter,fr1,fr2,npow,nwt,nwf,freqmin,isverbose)
        IMPLICIT NONE

        TYPE(sac_db) :: sdb
        REAL(SGL), INTENT(IN) :: f1, f2, f3, f4, fr1, fr2, freqmin
        INTEGER, INTENT(IN) :: iev, ist, my_id, npow, nwt, nwf
        LOGICAL, INTENT(IN) :: tnorm, tfilter, onebit, notch
        LOGICAL, INTENT(IN) :: isverbose

        INTEGER :: n, Nk, Nfft, nerr, k, nstrArray
        REAL(SGL) :: dt
        REAL(DBL) :: fs
        REAL(SGL), ALLOCATABLE, DIMENSION(:) :: seis_data
        COMPLEX(DBL), ALLOCATABLE, DIMENSION(:) :: s, sf, fftout
        COMPLEX(DBL) :: czero
        TYPE(C_PTR) :: plan
        CHARACTER(len=3) :: str_temp
        TYPE(sachead) :: shd
        LOGICAL :: alive
        CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: strArray
        CHARACTER(len=200) :: str_temp2

        ! ***************************************************************
        INQUIRE(FILE=TRIM(sdb%rec(iev,ist)%name), EXIST=alive)
        IF (.NOT. alive) RETURN

        ! Each process has its own sac script
        WRITE(str_temp, '(I3.3)') my_id

        CALL split_string(sdb%rec(iev,ist)%name, '/', strArray, nstrArray)
        str_temp2 =  TRIM(strArray(nstrArray-1))//'/'//TRIM(strArray(nstrArray))

        ! ***********************************************************************
        ! Perform one-bit normalization if onebit == .TRUE.
        ! ***********************************************************************
        IF (onebit) THEN

            ! Read the sac header and data.
            CALL sacio_readsac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data, nerr)

            ! Apply one-bit normalization.
            WHERE (seis_data>0)
                seis_data = 1
            ELSEWHERE (seis_data<0)
                seis_data = -1
            END WHERE

            ! Write the sac file after one-bit normalization.
            CALL sacio_writesac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data, nerr)

            IF (isverbose) WRITE(*,*) TRIM(str_temp2)//' one-bit normalization FINISHED.'

        ELSE

            ! ***********************************************************************
            ! Perform time domain running average if tnorm is TRUE and onebit is FALSE.
            ! ***********************************************************************
            IF (tnorm) THEN

                OPEN(UNIT=23, FILE=TRIM(str_temp)//'.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)

                WRITE(23,"(A)") 'sac<<EOF'
                WRITE(23,"(A)") 'r '//TRIM(sdb%rec(iev,ist)%name)
                WRITE(23,"(A)") 'rmean;rtrend'

                ! band-pass filtering between fr1 and fr2 if tfilter is TRUE
                IF (tfilter) THEN
                    WRITE(23,"('bp co ', F10.6, F10.6,' n 4 p 2')") fr1, fr2
                END IF

                WRITE(23,"(A)") 'abs'

                ! Obtain the smoothed amplitude (weight) at the center point with smooth window [n-h, n+h]
                WRITE(23,"('smooth mean h ', I3)") nwt
                WRITE(23,"(A)") 'w '//TRIM(str_temp)//'.avg'
                WRITE(23,"(A)") 'r '//TRIM(sdb%rec(iev,ist)%name)
                WRITE(23,"(A)") 'divf '//TRIM(str_temp)//'.avg'
                WRITE(23,"(A)") 'rmean;rtrend'
                WRITE(23,"(A)") 'w over'
                WRITE(23,"(A)") 'q'
                WRITE(23,"(A)") 'EOF'
                CLOSE(UNIT=23)

                ! Invoke shell to execute the sac commands.
                CALL SYSTEM('sh '//TRIM(str_temp)//'.txt')
                CALL SYSTEM('rm -rf '//TRIM(str_temp)//'.txt '//TRIM(str_temp)//'.avg')

                IF (isverbose) WRITE(*,"(A)") TRIM(str_temp2)//' time domain running average FINISHED.'

            END IF

        END IF

        ! Read the sac header and data.
        CALL sacio_readsac(TRIM(sdb%rec(iev,ist)%name), shd, seis_data, nerr)

        n = shd%npts
        dt = shd%delta

        ! Destroy the original sac file.
        CALL SYSTEM('rm -rf '//TRIM(sdb%rec(iev,ist)%name))

        ! ***********************************************************************
        ! Apply spectra whitening, band-rejection filtering [optional] and
        ! band-pass filtering.
        ! ***********************************************************************
        czero = (0.0D0, 0.0D0)

        ! Determine the power for FFT
        Nfft = 2**MAX(INT(LOG(REAL(n))/LOG(2.0))+1, 13)   ! Nfft: number of points for FFT

        fs = 1.0D0/(dt*Nfft)     ! fs: frequency interval

        ! Allocate memory for s and sf.
        ALLOCATE(s(Nfft), sf(Nfft), STAT=nerr)

        ! Initialize s with complex zero.
        DO k = 1, Nfft
            s(k) = czero
        END DO

        ! Fill s with real data.
        DO k = 1, n
            s(k) = seis_data(k)
        END DO

        ! Make backward FFT for the seismogram: s => sf
        plan = fftw_plan_dft_1d(Nfft, s, sf, FFTW_BACKWARD, FFTW_ESTIMATE)
        CALL fftw_execute_dft(plan, s, sf)
        CALL fftw_destroy_plan(plan)

        ! Kill half spectra
        Nk = Nfft/2 + 1
        Do k = Nk+1, Nfft
            sf(k) = czero
        END DO

        ! Correct the ends
        sf(1) = sf(1) / 2.0D0
        sf(Nk) = DCMPLX(REAL(sf(Nk)), 0.0D0)

        ! Allocate memory for seis_outreal and seis_outimag
        ALLOCATE(fftout(Nk), STAT=nerr)

        ! Initialize fft data.
        fftout = czero

        ! ***************************************************************
        ! Apply spectra whitening.
        ! ***************************************************************
        CALL whiten_spectra(f1, f4, fs, Nk, sf, nwf)

        ! ***************************************************************
        ! Reject the spike at the period band [25s 27s].
        ! ***************************************************************
        IF (notch) CALL band_rejection(0.0350,0.0360,0.0390,0.0400,fs,Nk,sf,npow,freqmin)

        ! ***************************************************************
        ! Band-pass filtering.
        ! ***************************************************************
        CALL band_pass(f1, f2, f3, f4, fs, Nk, npow, sf)

        ! ***************************************************************
        ! Output half-length data of sf.
        ! ***************************************************************
        DO k = 1, Nk
            fftout(k) = sf(k)
        END DO

        ! ***********************************************************************
        ! Write the complex value of the FFT results to a local file, using the
        ! original sac name.
        ! ***********************************************************************
        CALL write_mydata(TRIM(sdb%rec(iev,ist)%name), Nk, CMPLX(fftout), nerr)

        IF (isverbose) WRITE(*,"(A)") TRIM(str_temp2)//' spectral whitening FINISHED.'

        IF (ALLOCATED(s)) DEALLOCATE(s)
        IF (ALLOCATED(sf)) DEALLOCATE(sf)
        IF (ALLOCATED(fftout)) DEALLOCATE(fftout)
        IF (ALLOCATED(seis_data)) DEALLOCATE(seis_data)
        IF (ALLOCATED(strArray)) DEALLOCATE(strArray)

    END SUBROUTINE white_reject_filter4


    ! =======================================================================================
    ! Spectra whitening algorithm. It works the same as running average amplitude in the
    ! time domain, and it is equivalent to do 'smooth mean h nwt' and 'divf avg.amp' in SAC.
    ! f1, f4: frequency band to do spectral whitening [input]
    ! fs: frequency interval [input]
    ! Nk: half-length of the data points in the frequency domain [input]
    ! sf: FFT values in complex form [input and output]
    ! nwf: half-window length in spectral whitening [input]
    ! =======================================================================================
    SUBROUTINE whiten_spectra(f1, f4, fs, Nk, sf, nwf)
        IMPLICIT NONE

        REAL(SGL), INTENT(IN) :: f1, f4
        REAL(DBL), INTENT(IN) :: fs
        INTEGER, INTENT(IN) :: Nk, nwf
        COMPLEX(DBL), DIMENSION(:), INTENT(INOUT) :: sf ! sf: assumed-shape dummy array

        INTEGER :: k, jk, ijk
        REAL(DBL), DIMENSION(Nk) :: sf_amp, sf_weight ! temporay arrays
        REAL(DBL) :: f, sum

        ! Return if nwf == 0
        IF (nwf == 0) THEN
            WRITE(*,"(A)") 'Error: nwf should be a positive integer!'
            RETURN
        END IF

        ! ***************************************************************
        ! compute the amplitude of the spectra
        ! ***************************************************************
        DO k = 1, Nk
            sf_amp(k) = ABS(sf(k))
        END DO

        ! ***************************************************************
        ! Loop through each individual frequency
        ! ***************************************************************
        DO k = 1, Nk

            f = (k-1) * fs      ! f: frequency value

            ! Obtain the weight at frequency band [f1 f4] with half-window length nwt.
            ! Set the weight at frequency band <f1 and >f4 to be zero.
            IF (f>=f1 .AND. f<=f4) THEN
                sum = 0.0D0
                DO jk = -nwf, nwf
                    ijk = k + jk
                    sum = sum + sf_amp(ijk)
                END DO
                sf_weight(k) = 1.0D0 / (sum / (2.0D0*nwf+1.0D0))
            ELSE
                sf_weight(k) = 0.0D0
            END IF

        END DO

        ! ***************************************************************
        ! Obtain the whitened spectra (running averaged amplitude) at frequency band [f1 f4].
        ! Set the spectra at frequency band <f1 and >f4 to be zero.
        ! ***************************************************************
        DO k = 1, Nk
            sf(k) = sf(k) * sf_weight(k)
        END DO

    END SUBROUTINE whiten_spectra


    ! =======================================================================================
    ! Band-rejection filtering
    ! This function works just like the opposite of the band-pass filtering with
    ! two fliped consine taper function acting at [f1 f2] and [f3 f4], respectively.
    ! f1, f2, f3, f4: frequency limits [input]
    ! fs: frequency interval [input]
    ! Nk: half-length of the data points in the frequency domain [input]
    ! sf: FFT values in complex form [input and output]
    ! npow: power of the cosine tapering function [input]
    ! freqmin: retaining factor for the spectral whitening
    ! =======================================================================================
    SUBROUTINE band_rejection(f1, f2, f3, f4, fs, Nk, sf, npow, freqmin)
        ! freqmin is the percentage (0.5 means 50%) of amplitude we try to retain

        IMPLICIT NONE

        REAL(SGL), INTENT(IN) :: f1, f2, f3, f4, freqmin
        REAL(DBL), INTENT(IN) :: fs
        INTEGER, INTENT(IN) :: Nk, npow
        COMPLEX(DBL), DIMENSION(:), INTENT(INOUT) :: sf ! sf: assumed-shape dummy array

        INTEGER :: i, j
        REAL(DBL) :: pi, temp, f
        REAL(DBL), DIMENSION(Nk) :: alpha

        pi = DATAN(1.0D0)*4.0D0

        ! Initialize alpha with 1
        DO i = 1, Nk
            alpha(i) = 1.0D0
        END DO

        DO i = 1, Nk

            f = (i-1)*fs

            ! Keep alpha to be 1 if f<=f1.
            IF (f<=f1) THEN
                CYCLE

            ! alpha=(1+cos((f-f1)/(f2-f1)))/2(1-freqmin)+freqmin if f1<f<=f2
            ELSE IF (f<=f2) THEN
                temp = 1.0D0
                DO j = 1, npow
                    temp = temp*(1+dcos(pi*(f-f1)/(f2-f1)))/2.0D0*(1.0D0-freqmin) + freqmin
                END DO
                alpha(i) = temp

            ! alpha=1 if f2<f<=f3
            ELSE IF (f<=f3) THEN
                alpha(i) = freqmin

            ! alpha=(1-cos((f-f1)/(f2-f1)))/2(1-freqmin)+freqmin if f3<f<=f4
            ELSE IF (f<=f4) THEN
                temp = 1.0D0
                DO j = 1, npow
                    temp = temp*(1-dcos(pi*(f-f3)/(f4-f3)))/2.0D0*(1.0D0-freqmin) + freqmin
                END DO
                alpha(i) = temp
            END IF

            ! Keep alpha to be 1 if f>=f4

        END DO

        ! Apply the tapering.
        DO i = 1, Nk
            sf(i) = sf(i) * alpha(i)
        END DO

    END SUBROUTINE band_rejection


    ! =======================================================================================
    ! Do the cross-correlation computation
    ! sdb: sac_db struct [input]
    ! lag: lat time of the cross-correlation function [input]
    ! tarfolder: target folder to store the cross-correlation functions [input]
    ! bs_N: number of repeating times of the BOOTSTRAP method (e.g., 500)
    ! bs_type: which type does the BOOTSTRAP method apply to (e.g., 2_2)
    ! jsta1, jsta2: station indicies [input]
    ! my_id: process id number [input]
    ! isverbose: verbose indicator [input]
    ! isrecord: if output cross-correlation records [input]
    ! =======================================================================================
    SUBROUTINE cc_and_aftan(sdb,lag,tarfolder,isbs,bs_N,bs_type,jsta1,jsta2,my_id,&
        isverbose,isrecord,f1,f2,f3,f4,ispws,nweight,isrms)
        IMPLICIT NONE

        TYPE(sac_db), INTENT(IN) :: sdb
        REAL(SGL), INTENT(IN) :: f1, f2, f3, f4
        INTEGER, INTENT(IN) :: lag, bs_N, jsta1, jsta2, my_id, ispws, nweight
        LOGICAL, INTENT(IN) :: isbs
        CHARACTER(len=*), INTENT(IN) :: tarfolder, bs_type
        LOGICAL, INTENT(IN) :: isverbose, isrecord, isrms

        INTEGER :: nev, iev, nerr, p1, p2, p, vk
        INTEGER :: nstack, nout, nlen, k, ncc, nk
        REAL(SGL), DIMENSION(2*lag+1) :: tempcorr, xcorr_bs
        CHARACTER(len=300) :: str_temp, str_temp2
        CHARACTER(len=3) :: str_temp3
        TYPE(sachead) :: shd
        COMPLEX(SGL), ALLOCATABLE, DIMENSION(:) :: fftdata1, fftdata2
        REAL(SGL), ALLOCATABLE, DIMENSION(:) :: dataout
        REAL(SGL) :: dt, delta, nwin
        REAL(DBL), ALLOCATABLE, DIMENSION(:) :: rand_tmp
        INTEGER, ALLOCATABLE, DIMENSION(:) :: rand_array
        INTEGER :: i, period
        REAL(DBL) :: groupV, phaseV
        REAL(DBL), ALLOCATABLE, DIMENSION(:,:) :: grv_2darr, phv_2darr
        REAL(DBL), DIMENSION(100) :: grv_mean, grv_std, phv_mean, phv_std
        INTEGER :: xrow1, xrow2, ii, jj, period1, period2
        REAL(DBL), ALLOCATABLE, DIMENSION(:,:) :: matrix1, matrix2
        REAL(DBL) :: u_mean, u_std, c_mean, c_std
        LOGICAL :: alive, is_nan
        CHARACTER(len=20) :: per2, per3, spws, sweight

        ! ***************************************************************
        ! Return if the corresponding dispersion file already exists.
        ! ***************************************************************
        str_temp = TRIM(tarfolder)//'/FINAL/PWS/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
        TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'
        INQUIRE(FILE=str_temp, EXIST=alive)
        IF (alive) THEN
            WRITE(*,"(A)") TRIM(str_temp)//' exist, skip!'
            RETURN
        ELSE
            str_temp = TRIM(tarfolder)//'/FINAL/LINEAR/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'
            INQUIRE(FILE=str_temp, EXIST=alive)
            IF (alive .AND. ispws < 1) THEN
                WRITE(*,"(A)") TRIM(str_temp)//' exist, skip!'
                RETURN
            ENDIF
        ENDIF

        ! ======================================================================
        WRITE(per2,'(F8.2)') 1.0/f2
        WRITE(per3,'(F8.2)') 1.0/f3
        WRITE(spws,'(I5)') ispws
        WRITE(sweight,'(I5)') nweight

        ! Each process has its own process id
        WRITE(str_temp3, '(I3.3)') my_id

        ! Create temp directory to save single cross-correlation data.
        CALL SYSTEM('rm -rf '//TRIM(tarfolder)//'/'//TRIM(str_temp3))
        CALL SYSTEM('mkdir '//TRIM(tarfolder)//'/'//TRIM(str_temp3))

        ! Initiate stacking number and cross-correlation function.
        nstack = 0
        nev = sdb%nev

        ! Loop through the events.
        DO iev = 1, nev

            ! ***************************************************************
            ! Check if there are FFT data for this station pair
            ! at this event. check_data is an internal procedure.
            ! ***************************************************************
            IF (check_data(sdb, jsta1, jsta2, iev)) THEN

                ! Get the time interval.
                dt = sdb%rec(iev,jsta1)%dt

                ! ***************************************************************
                ! Read in the FFT data for the two stations.
                ! ***************************************************************
                CALL read_mydata(TRIM(sdb%rec(iev,jsta1)%name), nlen, fftdata1, nerr)
                CALL read_mydata(TRIM(sdb%rec(iev,jsta2)%name), nlen, fftdata2, nerr)

                ! ***************************************************************
                ! Compute the cross-correlation in the frequency domain.
                ! ***************************************************************
                CALL multi_fft(DCMPLX(fftdata1), DCMPLX(fftdata2), nlen, dataout, nout)

                ! ***************************************************************
                ! Assign the cross-correlation resulted from frequency domain
                ! computation to the time domain cross-correlation series.
                ! ***************************************************************
                tempcorr(lag+1) = dataout(1)
                DO k = 2, lag+1
                    tempcorr(lag+2-k) = dataout(k)
                    tempcorr(lag+k) = dataout(nout+2-k)
                END DO

                ! **************************************************************
                ! If the station wavefrom data are all zero, the resulting ccf
                ! will contain NaN values. Skip this ccf if this is the case.
                ! **************************************************************
                is_nan = .FALSE.
                DO k = 1, 2*lag + 1
                    IF (ISNAN(tempcorr(k))) is_nan = .TRUE.
                    EXIT
                END DO
                IF (is_nan) CYCLE

                ! **************************************************************
                ! Count the cross-correlation times.
                ! **************************************************************
                nstack = nstack + 1

                ! ***************************************************************
                ! Save single cross-correlation function into tempfolder
                ! ***************************************************************
                WRITE(str_temp,"(I6)") nstack
                str_temp = TRIM(tarfolder)//'/'//TRIM(str_temp3)//'/'//TRIM(sdb%st(jsta1)%n_name)&
                //'_'//TRIM(sdb%st(jsta2)%n_name)//'_'//TRIM(ADJUSTL(str_temp))

                CALL sacio_newhead(shd,dt,2*lag+1,-lag*dt)
                shd%evla = sdb%st(jsta1)%lat
                shd%evlo = sdb%st(jsta1)%lon
                shd%stla = sdb%st(jsta2)%lat
                shd%stlo = sdb%st(jsta2)%lon
                shd%kevnm = TRIM(sdb%st(jsta1)%name)
                shd%kstnm = TRIM(sdb%st(jsta2)%name)
                shd%kuser1 = TRIM(sdb%st(jsta1)%n_name)
                shd%kuser2 = TRIM(sdb%st(jsta2)%n_name)
                shd%nzyear = 2000
                shd%nzjday = 1
                shd%nzhour = 0
                shd%nzmin = 0
                shd%nzsec = 0
                shd%nzmsec = 0


                ! Write the single cross-correlation function.
                CALL sacio_writesac(str_temp, shd, tempcorr, nerr)

                ! IF (nstack == 1112) THEN
                !     WRITE(*,*) TRIM(sdb%rec(iev,jsta1)%name)
                !     WRITE(*,*) TRIM(sdb%rec(iev,jsta2)%name)
                !     WRITE(*,*) tempcorr
                ! END IF

            END IF

        END DO

        ! ***************************************************************
        ! Write cross-correlation log if isrecord is true.
        ! ***************************************************************
        IF (isrecord) THEN
            str_temp = TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)
            WRITE(str_temp2,"(I6)") nstack
            CALL SYSTEM('echo "'//TRIM(str_temp)//' '//TRIM(str_temp2)//'" | column -t >> CCRecord.lst')
        END IF

        ! ***************************************************************
        ! After the event iteration, write the final cross-correlation to
        ! a local binary SAC file, compute the snr, measure the dispersion
        ! curves and do the bootstrap measurements [optional].
        ! ***************************************************************
        IF (nstack > 0) THEN

            str_temp = 'mkdir -p '//TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)
            CALL SYSTEM(str_temp)

            ! ***************************************************************
            ! Apply phase weighted stacking procedure, outputing both linear
            ! and phase stacking (optional) final cross-correlations
            ! ***************************************************************
            str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)

            CALL SYSTEM('ls '//TRIM(tarfolder)//'/'//TRIM(str_temp3)//'/*'//&
            ' | TF_PWS -B '//TRIM(per3)//' -E '//TRIM(per2)//&
            ' -W '//TRIM(sweight)//' -O '//TRIM(str_temp)//' -P '//TRIM(spws))


            ! ***************************************************************
            ! Process the PWS CCF (optional)
            ! ***************************************************************
            IF (ispws > 0) THEN
                str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_pws.SAC'

                CALL SYSTEM('printf "r '//TRIM(str_temp)//'\nwh over\nq\n" | sac')

                ! Retrive distance header.
                CALL sacio_readhead(str_temp, shd, nerr)
                delta = shd%dist

                ! ***************************************************************
                ! 1. Convert CCF from sac format to ascii format for Huajian Yao' code.
                ! 2. Compute the spectral snr for the CCF.
                ! 3. Do the AFTAN
                ! ***************************************************************
                CALL sac_to_asc(TRIM(str_temp))
                CALL spectral_snr(TRIM(str_temp), f1, f4)
                CALL SYSTEM('AFTAN_PROG '//TRIM(str_temp))

                ! ***************************************************************
                ! Write final dispersion data based on pws cross-correlation.
                ! ***************************************************************
                str_temp2 = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_pws.SAC_'//TRIM(bs_type)

                INQUIRE(FILE=str_temp2, EXIST=alive)
                IF (.NOT. alive) THEN
                    IF (isverbose) WRITE(*,"(A)") '  NO final disperion data for pws cross-correlation!'
                ELSE
                    ! Interpolate the snr value for integer periods
                    str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_pws.SAC_snr'
                    CALL per_grp_phs_snr(str_temp, str_temp2)

                    CALL SYSTEM('mkdir -p '//TRIM(tarfolder)//'/FINAL/PWS/'//TRIM(sdb%st(jsta1)%n_name))

                    str_temp = TRIM(tarfolder)//'/FINAL/PWS/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'

                    OPEN(UNIT=29, FILE=str_temp, STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)
                    WRITE(29, "(A,2X,A)") TRIM(sdb%st(jsta1)%n_name),TRIM(sdb%st(jsta2)%n_name)
                    WRITE(29, "(4F12.4,F14.4)") sdb%st(jsta1)%lon,sdb%st(jsta1)%lat,&
                    sdb%st(jsta2)%lon,sdb%st(jsta2)%lat,delta
                    WRITE(29, "(A)")  "  Period    GroupV      PhaseV     SNR(left, right, symmetric)"
                    CLOSE(UNIT=29)

                    CALL SYSTEM('cat '//TRIM(str_temp2)//' >> '//TRIM(str_temp))
                END IF
            ENDIF


            ! ***************************************************************
            ! Process the LINEAR CCF
            ! ***************************************************************
            str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_ls.SAC'

            CALL SYSTEM('printf "r '//TRIM(str_temp)//'\nwh over\nq\n" | sac')

            ! Retrive distance header.
            CALL sacio_readhead(str_temp, shd, nerr)
            delta = shd%dist

            ! ***************************************************************
            ! 1. Convert CCF from sac format to ascii format for Huajian Yao' code.
            ! 2. Compute the spectral snr for the CCF.
            ! 3. Do the AFTAN
            ! ***************************************************************
            CALL sac_to_asc(TRIM(str_temp))
            CALL spectral_snr(TRIM(str_temp), f1, f4)
            CALL SYSTEM('AFTAN_PROG '//TRIM(str_temp))

            ! ***************************************************************
            ! Write final dispersion data based on linear stacking cross-correlation.
            ! ***************************************************************
            str_temp2 = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_ls.SAC_'//TRIM(bs_type)

            INQUIRE(FILE=str_temp2, EXIST=alive)
            IF (.NOT. alive) THEN
                IF (isverbose) WRITE(*,"(A)") '  NO final disperion data for linear stacking cross-correlation!'
            ELSE
                ! Interpolate the snr value for integer periods
                str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_ls.SAC_snr'
                CALL per_grp_phs_snr(str_temp, str_temp2)

                IF (.NOT. isbs) THEN
                    CALL SYSTEM('mkdir -p '//TRIM(tarfolder)//'/FINAL/LINEAR/'//TRIM(sdb%st(jsta1)%n_name))

                    str_temp = TRIM(tarfolder)//'/FINAL/LINEAR/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'

                    OPEN(UNIT=30, FILE=str_temp, STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)

                    WRITE(30, "(A,2X,A)") TRIM(sdb%st(jsta1)%n_name),TRIM(sdb%st(jsta2)%n_name)
                    WRITE(30, "(4F12.4,F14.4)") sdb%st(jsta1)%lon,sdb%st(jsta1)%lat,&
                    sdb%st(jsta2)%lon,sdb%st(jsta2)%lat,delta
                    WRITE(30, "(A)") "  Period    GroupV      PhaseV     SNR(left, right, symmetric)"
                    CLOSE(UNIT=30)

                    CALL SYSTEM('cat '//TRIM(str_temp2)//' >> '//TRIM(str_temp))

                ELSE
                    ! Allocate memory.
                    ALLOCATE(rand_tmp(nstack), rand_array(nstack))
                    ALLOCATE(grv_2darr(bs_N,100), phv_2darr(bs_N,100))

                    ! Initialize the BOOTSTRAP matrix with zero
                    grv_2darr = 0.0
                    phv_2darr = 0.0

                    ! ***************************************************************
                    ! Do the BOOTSTRAP
                    ! ***************************************************************
                    DO i = 1, bs_N

                        xcorr_bs = 0.0

                        ! Generate random integer data in [1,nstack]
                        CALL init_random_seed()
                        CALL RANDOM_NUMBER(rand_tmp)
                        rand_tmp = rand_tmp*(nstack-1)+1
                        rand_array = NINT(rand_tmp)

                        ! Stack selected daily cross-collelations
                        DO k = 1, nstack
                            WRITE(str_temp,"(I5)") rand_array(k)
                            str_temp = TRIM(tarfolder)//'/'//TRIM(str_temp3)//'/'//TRIM(sdb%st(jsta1)%n_name)&
                            //'_'//TRIM(sdb%st(jsta2)%n_name)//'_'//TRIM(ADJUSTL(str_temp))

                            CALL sacio_readsac(str_temp, shd, dataout, nerr)
                            tempcorr = dataout
                            xcorr_bs = xcorr_bs + tempcorr
                        END DO

                        ! Fill in the sac header.
                        CALL sacio_newhead(shd,dt,2*lag+1,-lag*dt)

                        shd%evla = sdb%st(jsta1)%lat
                        shd%evlo = sdb%st(jsta1)%lon
                        shd%stla = sdb%st(jsta2)%lat
                        shd%stlo = sdb%st(jsta2)%lon
                        shd%kevnm = TRIM(sdb%st(jsta1)%name)
                        shd%kstnm = TRIM(sdb%st(jsta2)%name)
                        shd%kuser1 = TRIM(sdb%st(jsta1)%n_name)
                        shd%kuser2 = TRIM(sdb%st(jsta2)%n_name)
                        shd%user0 = nstack

                        str_temp = TRIM(tarfolder)//'/'//TRIM(str_temp3)//'/'//TRIM(sdb%st(jsta1)%n_name)&
                        //'_'//TRIM(sdb%st(jsta2)%n_name)//'.SAC'

                        ! Write the bootstrap cross-correlation.
                        CALL sacio_writesac(str_temp, shd, xcorr_bs, nerr)

                        ! Update the sac header (e.g., delta).
                        CALL SYSTEM('printf "r '//TRIM(str_temp)//'\nwh over\nq\n" | sac')

                        ! Do the AFTAN
                        CALL SYSTEM('AFTAN_PROG '//TRIM(str_temp))

                        ! Read the dispersion data file.
                        str_temp = TRIM(str_temp)//'_'//TRIM(bs_type)
                        OPEN(UNIT=25, FILE=str_temp, STATUS='OLD', ACTION='READ', IOSTAT=nerr)

                        ! Fill the BOOTSTRAP matrix with dispersion data.
                        DO
                            READ(25, *, IOSTAT=nerr) period, groupV, phaseV
                            IF (nerr/=0) EXIT
                            grv_2darr(i, period) = groupV
                            phv_2darr(i, period) = phaseV
                        END DO

                        CLOSE(UNIT=25)

                    END DO

                    ! ***************************************************************
                    ! Calculate the mean and standard deviation of the BOOTSTRAP
                    ! measurements
                    ! ***************************************************************
                    CALL matrix_mean_std(grv_2darr, grv_mean, grv_std, 0.0D0)
                    CALL matrix_mean_std(phv_2darr, phv_mean, phv_std, 0.0D0)

                    CALL SYSTEM('mkdir -p '//TRIM(tarfolder)//'/BOOTSTRAP/'//TRIM(sdb%st(jsta1)%n_name))

                    str_temp = TRIM(tarfolder)//'/BOOTSTRAP/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'

                    OPEN(UNIT=26, FILE=str_temp, STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)

                    DO k = 1, 100
                        IF (grv_mean(k)>0.0 .OR. grv_std(k)>0.0 .OR. phv_mean(k)>0.0 .OR. phv_std(k)>0.0) THEN
                            WRITE(26, "(I4,4F12.6)") k, grv_mean(k), grv_std(k), phv_mean(k), phv_std(k)
                        END IF
                    END DO

                    CLOSE(UNIT=26)

                    DEALLOCATE(rand_tmp, rand_array)
                    DEALLOCATE(grv_2darr, phv_2darr)

                    ! ***************************************************************
                    ! Merge the dispersion and bootstrap data together.
                    ! ***************************************************************
                    xrow1 = 0       ! xrow1: number of rows of the dispersion file
                    xrow2 = 0       ! xrow2: number of rows of the bootstrap file

                    ! ***************************************************************
                    ! Count the rows of the dispersion file and load the data.
                    ! ***************************************************************
                    OPEN(UNIT=27, FILE=str_temp2, STATUS='OLD', ACTION='READ', IOSTAT=nerr)

                    DO
                        READ(27, *, IOSTAT=nerr)
                        IF (nerr/=0) EXIT
                        xrow1 = xrow1 + 1
                    END DO

                    IF (ALLOCATED(matrix1)) DEALLOCATE(matrix1)
                    ALLOCATE(matrix1(xrow1,3))

                    REWIND(UNIT=27)
                    DO k = 1, xrow1
                        READ(27,*) matrix1(k,1), matrix1(k,2), matrix1(k,3)
                    ! READ(27,*) ((matrix1(ii,jj), jj=1,3), ii=1,xrow1)
                    END DO

                    CLOSE(UNIT=27)

                    ! ***************************************************************
                    ! Count the rows of the bootstrap file and load the data.
                    ! ***************************************************************
                    str_temp = TRIM(tarfolder)//'/BOOTSTRAP/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'
                    OPEN(UNIT=28, FILE=str_temp, STATUS='OLD', ACTION='READ', IOSTAT=nerr)

                    DO
                        READ(28, *, IOSTAT=nerr)
                        IF (nerr/=0) EXIT
                        xrow2 = xrow2 + 1
                    END DO

                    IF (ALLOCATED(matrix2)) DEALLOCATE(matrix2)
                    ALLOCATE(matrix2(xrow2,5))

                    REWIND(UNIT=28)
                    READ(28,*) ((matrix2(ii,jj), jj=1,5), ii=1,xrow2)

                    CLOSE(UNIT=28)

                    ! ***************************************************************
                    ! Write the final result.
                    ! ***************************************************************
                    CALL SYSTEM('mkdir -p '//TRIM(tarfolder)//'/FINAL/LINEAR/'//TRIM(sdb%st(jsta1)%n_name))

                    str_temp = TRIM(tarfolder)//'/FINAL/LINEAR/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'

                    OPEN(UNIT=29, FILE=str_temp, STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)
                    WRITE(29, "(A,2X,A)") TRIM(sdb%st(jsta1)%n_name),TRIM(sdb%st(jsta2)%n_name)
                    WRITE(29, "(4F12.4,F14.4)") sdb%st(jsta1)%lon,sdb%st(jsta1)%lat,&
                    sdb%st(jsta2)%lon,sdb%st(jsta2)%lat,delta
                    WRITE(29, "(A)") "  Period   GroupV       gMean       gStd       PhaseV       pMean       pStd"

                    DO ii = 1, xrow1
                        u_mean = 0.0D0
                        u_std = 0.0D0
                        c_mean = 0.0D0
                        c_std = 0.0D0

                        period1 = INT(matrix1(ii, 1))

                        DO jj = 1, xrow2
                            period2 = INT(matrix2(jj, 1))
                            IF (period1 == period2) THEN
                                u_mean = matrix2(jj, 2)
                                u_std = matrix2(jj, 3)
                                c_mean = matrix2(jj, 4)
                                c_std = matrix2(jj, 5)
                            END IF
                        END DO

                        WRITE(29, "(I5,7F12.4)") period1,matrix1(ii,2),u_mean,u_std,matrix1(ii,3),c_mean,c_std

                    END DO

                    CLOSE(UNIT=29)

                    DEALLOCATE(matrix1, matrix2)

                END IF
            END IF


            IF (isrms) THEN
                ! ***************************************************************
                ! Apply the RMSR-SS method.
                ! ***************************************************************
                OPEN(UNIT=151,FILE='passbands.dat',STATUS='OLD',ACTION='READ',IOSTAT=nerr)
                IF (nerr/=0) THEN
                    WRITE(*,"(A)") 'Error: Can not open passbands.dat!'
                ELSE
                    tempcorr = 0.0
                    DO
                        READ(151,*,IOSTAT=nerr) p1, p2, p, nwin
                        IF (nerr/=0) EXIT

                        str_temp = TRIM(tarfolder)//'/'//TRIM(str_temp3)
                        CALL SYSTEM('rm -rf '//TRIM(str_temp)//'_filter')
                        CALL SYSTEM('cp -r '//TRIM(str_temp)//' '//TRIM(str_temp)//'_filter')

                        OPEN(UNIT=152, FILE=TRIM(str_temp3)//'.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)
                        WRITE(152,"(A)") 'sac<<EOF'
                        WRITE(152,"(A)") 'r '//TRIM(str_temp)//'_filter/*'
                        WRITE(152,"(A)") 'rmean;rtrend;taper'
                        WRITE(152,"('bp co ', F10.6, F10.6,' n 4 p 2')") 1.0/p2, 1.0/p1
                        WRITE(152,"(A)") 'w over'

                        ! Compute the envelope to find the group arrival time
                        str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                        TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                        TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_ls.SAC'
                        WRITE(152,"(A)") 'r '//TRIM(str_temp)
                        WRITE(152,"(A)") 'reverse'
                        WRITE(152,"(A)") 'addf '//TRIM(str_temp)
                        WRITE(152,"(A)") 'rmean;rtrend;taper'
                        WRITE(152,"('bp co ', F10.6, F10.6,' n 4 p 2')") 1.0/p2, 1.0/p1
                        WRITE(152,"(A)") 'envelope '
                        WRITE(152,"(A)") 'w '//TRIM(str_temp3)//'.sac'
                        WRITE(152,"(A)") 'q'
                        WRITE(152,"(A)") 'EOF'
                        CLOSE(UNIT=152)

                        ! Invoke shell to execute the sac commands.
                        CALL SYSTEM('sh '//TRIM(str_temp3)//'.txt > /dev/null')

                        ! Find the npts of the group arrival
                        CALL sacio_readsac(TRIM(str_temp3)//'.sac', shd, dataout, nerr)
                        vk = MAXLOC(dataout((shd%npts+1)/2:), DIM=1)

                        ! ***************************************************************
                        ! Performing the rms ratio selection stacking method.
                        ! ***************************************************************
                        str_temp = TRIM(tarfolder)//'/'//TRIM(str_temp3)
                        CALL rmsr_ss(TRIM(str_temp)//'_filter', TRIM(str_temp3)//'.txt', p, nwin, vk, dataout, ncc, nk, nerr)

                        ! Delete the tmp files and filter folder
                        CALL SYSTEM('rm -rf '//TRIM(str_temp3)//'.txt '//TRIM(str_temp3)//'.sac '//TRIM(str_temp)//'_filter')

                        str_temp = TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)
                        IF (nerr/=0) THEN
                            CALL SYSTEM('echo "'//TRIM(str_temp)//' SHORT-LAG-TIME!'//'" | column -t >> RMSR_SS.lst')
                            CALL SYSTEM('rm -rf '//TRIM(tarfolder)//'/'//TRIM(str_temp3))
                            CLOSE(UNIT=151)
                            RETURN
                        ELSE
                            ! Write the rmsr_ss log.
                            WRITE(str_temp2,"(2I5,2I10,I5,'%')") p1, p2, ncc, nk, INT(REAL(nk)/REAL(ncc)*100)
                            CALL SYSTEM('echo "'//TRIM(str_temp)//' '//TRIM(str_temp2)//'" | column -t >> RMSR_SS.lst')

                            ! Write the individual cross-correlation function after performing rmsr_ss.
                            WRITE(str_temp2,"('_rms.T',I3.3,'_T',I3.3,'.SAC')") p1, p2
                            str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                            TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//TRIM(str_temp2)
                            CALL sacio_writesac(str_temp, shd, dataout, nerr)

                        END IF

                        tempcorr = tempcorr + dataout

                    END DO

                    CLOSE(UNIT=151)


                    ! Write the final cross-correlation function after performing rmsr_ss.
                    ! str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    ! TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                    ! TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_rms.SAC'
                    ! CALL sacio_writesac(str_temp, shd, tempcorr, nerr)

                    ! ***************************************************************
                    ! 1. Convert CCF from sac format to ascii format for Huajian Yao' code.
                    ! 2. Compute the spectral snr for the CCF.
                    ! 3. Do the AFTAN
                    ! ***************************************************************
                    ! CALL sac_to_asc(TRIM(str_temp))
                    ! CALL spectral_snr(TRIM(str_temp), f1, f4)
                    ! CALL SYSTEM('AFTAN_PROG '//TRIM(str_temp))

                    ! ***************************************************************
                    ! Write final dispersion data based on rmsr_ss cross-correlation.
                    ! ***************************************************************
                    ! str_temp2 = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    ! TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                    ! TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_rms.SAC_'//TRIM(bs_type)
                    !
                    ! INQUIRE(FILE=str_temp2, EXIST=alive)
                    ! IF (.NOT. alive) THEN
                    !     IF (isverbose) WRITE(*,"(A)") '  NO final disperion data for rms cross-correlation!'
                    ! ELSE
                    !     ! Interpolate the snr value for integer periods
                    !     str_temp = TRIM(tarfolder)//'/CC_AFTAN/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    !     TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'/'//&
                    !     TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'_rms.SAC_snr'
                    !     CALL per_grp_phs_snr(str_temp, str_temp2)
                    !
                    !     CALL SYSTEM('mkdir -p '//TRIM(tarfolder)//'/FINAL/RMS/'//TRIM(sdb%st(jsta1)%n_name))
                    !
                    !     str_temp = TRIM(tarfolder)//'/FINAL/RMS/'//TRIM(sdb%st(jsta1)%n_name)//'/'//&
                    !     TRIM(sdb%st(jsta1)%n_name)//'_'//TRIM(sdb%st(jsta2)%n_name)//'.dat'
                    !
                    !     OPEN(UNIT=29, FILE=str_temp, STATUS='REPLACE', ACTION='WRITE', IOSTAT=nerr)
                    !     WRITE(29, "(A,2X,A)") TRIM(sdb%st(jsta1)%n_name),TRIM(sdb%st(jsta2)%n_name)
                    !     WRITE(29, "(4F12.4,F14.4)") sdb%st(jsta1)%lon,sdb%st(jsta1)%lat,&
                    !     sdb%st(jsta2)%lon,sdb%st(jsta2)%lat,delta
                    !     WRITE(29, "(A)")  "  Period    GroupV      PhaseV     SNR(left, right, symmetric)"
                    !     CLOSE(UNIT=29)
                    !
                    !     CALL SYSTEM('cat '//TRIM(str_temp2)//' >> '//TRIM(str_temp))
                    ! END IF

                END IF

            END IF

        END IF

        ! Remove temp directory.
        CALL SYSTEM('rm -rf '//TRIM(tarfolder)//'/'//TRIM(str_temp3))

        IF (ALLOCATED(fftdata1)) DEALLOCATE(fftdata1)
        IF (ALLOCATED(fftdata2)) DEALLOCATE(fftdata2)
        IF (ALLOCATED(dataout)) DEALLOCATE(dataout)
        IF (ALLOCATED(rand_tmp)) DEALLOCATE(rand_tmp)
        IF (ALLOCATED(rand_array)) DEALLOCATE(rand_array)
        IF (ALLOCATED(grv_2darr)) DEALLOCATE(grv_2darr)
        IF (ALLOCATED(phv_2darr)) DEALLOCATE(phv_2darr)
        IF (ALLOCATED(matrix1)) DEALLOCATE(matrix1)
        IF (ALLOCATED(matrix2)) DEALLOCATE(matrix2)

    CONTAINS

        ! ***************************************************************
        ! Internal procedure to check if there are FFT data for one
        ! station pair at one particular event.
        ! sdb: sac_db struct [input]
        ! jsta1, jsta2: station iterators [input]
        ! iev: event iterator [input]
        ! ***************************************************************
        LOGICAL FUNCTION check_data(sdb, jsta1, jsta2, iev)
            IMPLICIT NONE

            TYPE(sac_db), INTENT(IN) :: sdb
            INTEGER, INTENT(IN) :: jsta1, jsta2, iev

            CHARACTER(len=200) :: str_temp
            LOGICAL :: alive

            check_data = .FALSE.

            str_temp = TRIM(sdb%rec(iev,jsta1)%name)
            INQUIRE(FILE=str_temp, EXIST=alive)
            IF (.NOT. alive) RETURN

            str_temp = TRIM(sdb%rec(iev,jsta2)%name)
            INQUIRE(FILE=str_temp, EXIST=alive)
            IF (.NOT.alive) RETURN

            check_data = .TRUE.

        END FUNCTION check_data

    END SUBROUTINE cc_and_aftan


    ! =======================================================================================
    ! Compute the cross-correlation in the frequency domain
    ! sf1: half-length FFT data at station 1 [input]
    ! sf2: half-length FFT data at station 2 [input]
    ! nlen: number of data points [input]
    ! dataout: output time domain cross-correlation data [output]
    ! nout: number of output time domain cross-correlation data [output]
    ! =======================================================================================
    SUBROUTINE multi_fft(sf1, sf2, nlen, dataout, nout)
        IMPLICIT NONE

        INTEGER, INTENT(IN) :: nlen
        COMPLEX(DBL), DIMENSION(nlen), INTENT(IN) :: sf1, sf2
        INTEGER, INTENT(OUT) :: nout
        REAL(SGL), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: dataout

        COMPLEX(DBL), ALLOCATABLE, DIMENSION(:) :: scorr, sfcorr
        COMPLEX(DBL) :: czero
        INTEGER :: k, nerr
        TYPE(C_PTR) :: plan

        czero = (0.0D0, 0.0D0)

        nout = (nlen-1)*2

        ALLOCATE(dataout(nout), scorr(nout), sfcorr(nout), STAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") "Error: Allocating memory for dataout, scorr and sfcorr failed!"
            RETURN
        END IF

        DO k = 1, nlen
            sfcorr(k) = sf1(k)*DCONJG(sf2(k))
        END DO

        DO k = nlen+1, nout
            sfcorr(k) = czero
        END DO

        ! Make forward FFT for the cross-correlation: sfconj => s
        plan = fftw_plan_dft_1d(nout, sfcorr, scorr, FFTW_FORWARD, FFTW_ESTIMATE)
        CALL fftw_execute_dft(plan, sfcorr, scorr)
        CALL fftw_destroy_plan(plan)

        dataout = 2.0*REAL(DREAL(scorr))/nout

        IF (ALLOCATED(scorr)) DEALLOCATE(scorr)
        IF (ALLOCATED(sfcorr)) DEALLOCATE(sfcorr)

    END SUBROUTINE multi_fft


    ! =======================================================================================
    ! Apply RMS ratio selection method
    ! ccdir: directory that contains all the ccfs [input]
    ! tmplist: tempory list file [input]
    ! per: central period of the pass band [input]
    ! nwin: ratio of signal window length to per [input]
    ! vk: npts of the group arrival [input]
    ! cclinear: final ccf stacked from the selected ccfs [output]
    ! ncc: number of all the ccfs [output]
    ! nk: number of the selected ccfs [output]
    ! =======================================================================================
    SUBROUTINE rmsr_ss(ccdir, tmplist, per, nwin, vk, cclinear, ncc, nk, nerr)
        IMPLICIT NONE

        CHARACTER(len=*), INTENT(IN) :: ccdir, tmplist
        INTEGER, INTENT(IN) :: per, vk
        REAL(SGL), INTENT(IN) :: nwin
        REAL(SGL), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: cclinear
        INTEGER, INTENT(OUT) :: ncc, nk, nerr

        CHARACTER(len=200) :: ccpath
        INTEGER :: npts, df, i, j, k, ws, wn, tk
        INTEGER :: ck1, ck2, ck3, ck4, ck5, ck6
        INTEGER :: ck7, ck8, ck9, ck10, nsig, nnoi
        REAL(DBL) ::  rs, rsrs
        TYPE(sachead) :: shd
        REAL(SGL), ALLOCATABLE, DIMENSION(:) :: seis_data
        REAL(SGL), ALLOCATABLE, DIMENSION(:,:) :: ccfs
        REAL(DBL), ALLOCATABLE, DIMENSION(:) :: s, r, ss, rr

        ! Generate the ccf list
        CALL SYSTEM('ls '//TRIM(ccdir)//'/* > '//TRIM(tmplist))

        ! Construct the initial 2-D array (ccfs) with each column
        ! representing one individual ccf
        OPEN(UNIT=11,FILE=TRIM(tmplist),STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: Can not open '//TRIM(tmplist)
            CLOSE(UNIT=11)
            RETURN
        END IF
        ncc = 0
        DO
            READ(11, "(A)", IOSTAT=nerr) ccpath
            IF (nerr /= 0) EXIT
            ncc = ncc + 1
        END DO
        CALL sacio_readsac(ccpath, shd, seis_data, nerr)
        npts = shd%npts
        df = INT(1.0/shd%delta)

        ALLOCATE(ccfs(npts, ncc), cclinear(npts))
        REWIND(11)
        i = 1
        DO
            READ(11, "(A)", IOSTAT=nerr) ccpath
            IF (nerr /= 0) EXIT
            CALL sacio_readsac(ccpath, shd, seis_data, nerr)
            DO j = 1, npts
                ccfs(j, i) = seis_data(j)
            END DO
            i = i + 1
        END DO
        CLOSE(UNIT=11)

        ! Sum along the row to produce the linearly stacked ccf (cclinear)
        cclinear = SUM(ccfs, 2)

        ws = INT(df*per*nwin)       ! npts of the half signal window
        wn = 2*ws                   ! npts of the half non-noise window
        tk = INT((npts-1)/2)+1      ! i-th location of the central point of the ccf

        ck1 = 1
        ck2 = tk-vk-wn
        ck3 = tk-vk-ws
        ck4 = tk-vk+ws
        ck5 = tk-vk+wn
        ck6 = tk+vk-wn
        ck7 = tk+vk-ws
        ck8 = tk+vk+ws
        ck9 = tk+vk+wn
        ck10 = npts

        IF (ck5 > ck6) THEN
            ck5 = tk
            ck6 = tk
        END IF

        nerr = 0
        IF (ck2 < ck1) THEN
            nerr = 1
            RETURN
        END IF

        nsig = ck4-ck3+1+ck8-ck7+1
        nnoi = ck2-ck1+1+ck6-ck5+1+ck10-ck9+1
        ALLOCATE(s(nsig), r(nnoi), ss(nsig), rr(nnoi))

        nk = 0
        DO i = 1, ncc

            ! Compute s
            k = 0
            DO j = ck3, ck4
                k = k + 1
                s(k) = cclinear(j)
            END DO
            DO j = ck7, ck8
                k = k + 1
                s(k) = cclinear(j)
            END DO

            ! Compute r
            k = 0
            DO j = ck1, ck2
                k = k + 1
                r(k) = cclinear(j)
            END DO
            DO j = ck5, ck6
                k = k + 1
                r(k) = cclinear(j)
            END DO
            DO j = ck9, ck10
                k = k + 1
                r(k) = cclinear(j)
            END DO

            ! Compute rms
            rs = rms(s)/rms(r)

            ! Subtract the i-th component from the all
            cclinear = cclinear - ccfs(:,i)

            ! Compute ss
            k = 0
            DO j = ck3, ck4
                k = k + 1
                ss(k) = cclinear(j)
            END DO
            DO j = ck7, ck8
                k = k + 1
                ss(k) = cclinear(j)
            END DO

            ! Compute rr
            k = 0
            DO j = ck1, ck2
                k = k + 1
                rr(k) = cclinear(j)
            END DO
            DO j = ck5, ck6
                k = k + 1
                rr(k) = cclinear(j)
            END DO
            DO j = ck9, ck10
                k = k + 1
                rr(k) = cclinear(j)
            END DO

            rsrs = rms(ss)/rms(rr)/(1.0D0+1.0D0/ncc)

            IF (rsrs < rs) THEN
                ! Add the i-th component back to the all
                cclinear = cclinear + ccfs(:,i)
                nk = nk + 1
            END IF

        END DO

        ! ***************************************************************
        ! The following code should be designed to normalize the narrow
        ! band filtered CCF since their spectrum are changed after the
        ! RMSR_SS process.
        ! The energy normalizing scheme and the simple division scheme
        ! are tested but the results are not satisfying enough.
        ! More sophisticated approach is needed to deal with this problem
        ! Currently, we do not do the normalization work.
        ! ***************************************************************

        ! k = 0
        ! DO j = ck3, ck4
        !     k = k + 1
        !     s(k) = cclinear(j)
        ! END DO
        ! DO j = ck7, ck8
        !     k = k + 1
        !     s(k) = cclinear(j)
        ! END DO

        ! Compute the energe normalized cclinear
        ! cclinear = cclinear/REAL(rms(s))

        ! another normalization scheme
        ! cclinear = cclinear/(REAL(nk)/REAL(ncc))

    END SUBROUTINE rmsr_ss


    ! =======================================================================================
    ! Compute the spectral snr for a given ccf
    ! sacpath: file path of the stacked ccf [input]
    ! fb: lower frequency bound [input]
    ! fe: upper frequency bound [input]
    ! =======================================================================================
    SUBROUTINE spectral_snr(sacpath, fb, fe)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: sacpath
        REAL, INTENT(IN) :: fb, fe

        TYPE(sachead) :: sachd
        CHARACTER(len=200) :: filepath
        LOGICAL :: alive
        REAL, ALLOCATABLE, DIMENSION(:) :: seisdata, f, per, seis_out, seis_tmp
        REAL, ALLOCATABLE, DIMENSION(:,:) :: snr
        INTEGER :: nf, nerr, k, nall, nreal, npow, kk, i, ck1, ck2
        REAL :: maxP, fstep, minV, maxV, dist, addfac
        REAL :: minT, maxT, tb, te, f1, f2, f3, f4
        REAL :: dt, sigmax, noimax, sum

        INQUIRE(FILE=sacpath, EXIST=alive)
        IF (.NOT. alive) RETURN

        maxP = 1.0/fb

        npow = 1
        nf = 40
        addfac = 0.01
        minV = 1.8
        maxV = 4.0

        ! Compute the discrete frequency series
        ALLOCATE(f(nf), per(nf), snr(nf-2, 3))

        fstep = (LOG(fb)-LOG(fe))/(nf-1)
        DO k = 1, nf
            f(k) = EXP(LOG(fe)+(k-1)*fstep)
            per(k) = 1.0/f(k)
        END DO

        ! Read sac data and header
        CALL sacio_readsac(sacpath, sachd, seisdata, nerr)
        IF (nerr/=0) THEN
            WRITE(*,"(A)") 'Error: Can not open '//sacpath
            RETURN
        END IF

        nall = sachd%npts
        nreal = (sachd%npts+1)/2
        dt = sachd%delta
        dist = sachd%dist

        ALLOCATE(seis_tmp(nreal), seis_out(nreal))

        tb = 0.0
        te = (nreal-1)*dt
        minT = dist/maxV - maxP
        maxT = dist/minV + 2*maxP
        IF (minT < tb) minT = tb
        IF (maxT > (te-300)) maxT = (te-300)
        IF (maxT <= minT .OR. INT((maxT-tb)/dt) >= nreal) THEN
            WRITE(*,"(A)") 'Warning: No SNR measurements since the CCF is too short to contain the signal window!'
            RETURN
        END IF

        ! the start of the noise window is 500 s after the signal window
        ! the length of the noise window is 500 s or to the end when the ccf is too short
        ck1 = INT((maxT-tb+500)/dt+1)
        ck2 = INT((maxT-tb+500+500)/dt)
        IF (ck1 >= nreal) THEN
            WRITE(*,*) 'ck1, nreal:', ck1, nreal
            WRITE(*,"(A)") 'Warning: No SNR measurements since the CCF is too short to contain the noise window!'
            RETURN
        END IF
        IF (ck2 > nreal) ck2 = nreal


        ! **********************************************************************
        ! Process the negative part
        ! **********************************************************************
        DO i = 1, nreal
            seis_tmp(i) = seisdata(nreal-i+1)
        END DO

        DO k = 2, nf-1
            f2 = f(k+1)
            f3 = f(k-1)
            f1 = f2 - addfac
            f4 = f3 + addfac
            IF (f1 <= 0.0) f1 = f2/2.0

            CALL filter4(f1,f2,f3,f4,npow,dt,nreal,seis_tmp,seis_out)

            seis_out = ABS(seis_out)

            sigmax = 0.0
            noimax = 0.0

            DO i = INT((minT-tb)/dt+1), INT((maxT-tb)/dt)
                IF (seis_out(i)>sigmax) THEN
                    sigmax = seis_out(i)
                END IF
            END DO

            sum = 0.0
            kk = 0
            DO i = ck1, ck2
                sum = sum + seis_out(i)*seis_out(i)
                kk = kk + 1
            END DO

            noimax = SQRT(sum/kk)
            snr(k-1, 1) = MIN(1000.0, sigmax/noimax)
        END DO


        ! **********************************************************************
        ! Process the positive part
        ! **********************************************************************
        DO i = 1, nreal
            seis_tmp(i) = seisdata(nreal+i-1)
        END DO

        DO k = 2, nf-1
            f2 = f(k+1)
            f3 = f(k-1)
            f1 = f2 - addfac
            f4 = f3 + addfac
            IF (f1 <= 0.0) f1 = f2/2.0

            CALL filter4(f1,f2,f3,f4,npow,dt,nreal,seis_tmp,seis_out)

            seis_out = ABS(seis_out)

            sigmax = 0.0
            noimax = 0.0

            DO i = INT((minT-tb)/dt+1), INT((maxT-tb)/dt)
                IF (seis_out(i)>sigmax) THEN
                    sigmax = seis_out(i)
                END IF
            END DO

            sum = 0.0
            kk = 0
            DO i = ck1, ck2
                sum = sum + seis_out(i)*seis_out(i)
                kk = kk + 1
            END DO

            noimax = SQRT(sum/kk)
            snr(k-1, 2) = MIN(1000.0, sigmax/noimax)
        END DO


        ! **********************************************************************
        ! Process the symmetric part
        ! **********************************************************************
        DO i = 1, nreal
            seis_tmp(i) = seisdata(nreal-i+1) + seisdata(nreal+i-1)
        END DO

        DO k = 2, nf-1
            f2 = f(k+1)
            f3 = f(k-1)
            f1 = f2 - addfac
            f4 = f3 + addfac
            IF (f1 <= 0.0) f1 = f2/2.0

            CALL filter4(f1,f2,f3,f4,npow,dt,nreal,seis_tmp,seis_out)

            seis_out = ABS(seis_out)

            sigmax = 0.0
            noimax = 0.0

            DO i = INT((minT-tb)/dt+1), INT((maxT-tb)/dt)
                IF (seis_out(i)>sigmax) THEN
                    sigmax = seis_out(i)
                END IF
            END DO

            sum = 0.0
            kk = 0
            DO i = ck1, ck2
                sum = sum + seis_out(i)*seis_out(i)
                kk = kk + 1
            END DO

            noimax = SQRT(sum/kk)
            snr(k-1, 3) = MIN(1000.0, sigmax/noimax)
        END DO

        ! Write the snr file
        filepath = TRIM(sacpath)//'_snr'
        OPEN(UNIT=44,FILE=filepath,ACTION='WRITE',STATUS='REPLACE')
        DO k = 2, nf-1
            WRITE(44, "(4F12.2)") per(k), snr(k-1, 1), snr(k-1, 2), snr(k-1, 3)
        END DO
        CLOSE(UNIT=44)

    END SUBROUTINE spectral_snr


    ! =======================================================================================
    ! Apply the band-pass filtering
    ! f1, f2, f3, f4: frequency limits [input]
    ! npow: power of cosine tapering function [input]
    ! dt: sampling interval [input]
    ! n: length of input seis_in [input]
    ! seis_out: bandpassed array [output]
    ! =======================================================================================
    SUBROUTINE filter4(f1, f2, f3, f4, npow, dt, n, seis_in, seis_out)
        IMPLICIT NONE

        ! the type of the dummy argumets should be the same as the real arguments defined in the main program.
        REAL, INTENT(IN) :: f1, f2, f3, f4, dt
        INTEGER, INTENT(IN) :: npow, n

        ! Unlike the declaration in the main program, seis_in and seis_out
        ! are declared as shape-assumed arrays.
        REAL, DIMENSION(n), INTENT(IN) :: seis_in
        REAL, DIMENSION(n), INTENT(OUT) :: seis_out

        INTEGER :: Nfft, Nk
        REAL(C_DOUBLE) :: fs
        COMPLEX(C_DOUBLE_COMPLEX), ALLOCATABLE, DIMENSION(:) :: s, sf
        COMPLEX(C_DOUBLE_COMPLEX) :: czero = (0.0D0, 0.0D0)
        INTEGER :: nerr, k
        TYPE(C_PTR) :: plan, plan2

        ! Determine the power for FFT
        Nfft = 2**MAX(INT(LOG(REAL(n))/LOG(2.0))+1, 13)   ! N: number of points for FFT


        fs = 1.0D0/(dt*Nfft)     ! fs: frequency interval

        ! Allocate memory for s and sf.
        ALLOCATE(s(Nfft), sf(Nfft), STAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") "Allocating memory for s and sf failed!"
            RETURN
        END IF

        ! Initialize s with complex zero.
        DO k = 1, Nfft
            s(k) = czero
        END DO

        ! Fill s with real data.
        DO k = 1, n
            s(k) = seis_in(k)
        END DO

        ! Make backward FFT for the seismogram: s => sf
        plan = fftw_plan_dft_1d(Nfft, s, sf, FFTW_BACKWARD, FFTW_ESTIMATE)
        CALL fftw_execute_dft(plan, s, sf)
        CALL fftw_destroy_plan(plan)

        ! Kill half spectra
        Nk = Nfft/2 + 1
        Do k = Nk+1, Nfft
            sf(k) = czero
        END DO

        ! Correct the ends
        sf(1) = sf(1) / 2.0D0
        sf(Nk) = DCMPLX(DREAL(sf(Nk)), 0.0D0)

        ! Apply the band-pass filtering to the first half frequency band.
        CALL band_pass(f1, f2, f3, f4, fs, Nk, npow, sf)

        ! Make forward FFT for the seismogram: sf => s
        plan2 = fftw_plan_dft_1d(Nfft, sf, s, FFTW_FORWARD, FFTW_ESTIMATE)
        CALL fftw_execute_dft(plan2, sf, s)
        CALL fftw_destroy_plan(plan2)

        ! Get the final result.
        DO k = 1, n
            seis_out(k) = 2.0*REAL(DREAL(s(k)))/Nfft  ! 2 is introduced because half of the spectra is set as complex zero.
        END DO

        DEALLOCATE(s, sf, STAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") "Deallocating memory for s and sf failed!"
            RETURN
        END IF

    END SUBROUTINE filter4


    ! =======================================================================================
    ! Assemble the dispersion and snr data
    ! snrfile: snr data file [input]
    ! dispfile: dispersion data file [input]
    ! =======================================================================================
    SUBROUTINE per_grp_phs_snr(snrfile, dispfile)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(IN) :: snrfile, dispfile

        REAL(DBL), ALLOCATABLE, DIMENSION(:,:) :: snrdata, dispdata
        REAL(DBL), ALLOCATABLE, DIMENSION(:,:) :: snr_new
        INTEGER :: nerr, n, i
        LOGICAL :: alive

        INQUIRE(FILE=snrfile, EXIST=alive)
        IF (.NOT. alive) RETURN

        ! Read the snr data file
        OPEN(UNIT=78,FILE=snrfile,STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: Can not open '//TRIM(snrfile)
            CLOSE(UNIT=78)
            RETURN
        END IF
        n = 0
        DO
            READ(78, *, IOSTAT=nerr)
            IF (nerr /= 0) EXIT
            n = n + 1
        END DO

        ALLOCATE(snrdata(n,4))

        REWIND(UNIT=78)

        DO i = 1, n
            READ(78, *) snrdata(i,1), snrdata(i,2), snrdata(i,3), snrdata(i,4)
        END DO
        CLOSE(UNIT=78)

        ! Read the dispersion data file
        OPEN(UNIT=79,FILE=dispfile,STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: Can not open '//TRIM(dispfile)
            CLOSE(UNIT=79)
            RETURN
        END IF
        n = 0
        DO
            READ(79, *, IOSTAT=nerr)
            IF (nerr /= 0) EXIT
            n = n + 1
        END DO

        ALLOCATE(dispdata(n,3), snr_new(n,3))

        REWIND(UNIT=79)
        DO i = 1, n
            READ(79, *) dispdata(i,1), dispdata(i,2), dispdata(i,3)
        END DO
        CLOSE(UNIT=79)

        ! Do the linear interpolation
        CALL linear_interpo(snrdata(:,1), snrdata(:,2), dispdata(:,1), snr_new(:,1), nerr)
        CALL linear_interpo(snrdata(:,1), snrdata(:,3), dispdata(:,1), snr_new(:,2), nerr)
        CALL linear_interpo(snrdata(:,1), snrdata(:,4), dispdata(:,1), snr_new(:,3), nerr)

        OPEN(UNIT=80,FILE=dispfile,STATUS='REPLACE',ACTION='WRITE')
        DO i = 1, n
            WRITE(80, "(I6,2F12.4,3F10.2)") INT(dispdata(i,1)), dispdata(i,2), dispdata(i,3), &
            snr_new(i,1), snr_new(i,2), snr_new(i,3)
        END DO
        CLOSE(UNIT=80)


    END SUBROUTINE per_grp_phs_snr

END MODULE Main_Proc
