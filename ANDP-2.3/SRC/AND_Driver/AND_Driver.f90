PROGRAM AND_Driver

    USE mpi
    USE Main_Proc           ! Main_Proc: imported module which contains all the main functions, subroutines
                            ! and also other imported modules (e.g. my_definition, string, math, date_time).
    IMPLICIT NONE


    ! ***********************************************************************
    ! Variable declaration section.
    ! ***********************************************************************
    CHARACTER(len=200) tarfolder, evpath, str_temp
    CHARACTER(len=3) :: channel
    CHARACTER(len=1) :: stnorm, stfilter, sonebit, snotch
    CHARACTER(len=1) :: sbs, srecord, sverbose, spws, srms
    INTEGER :: n_tmp, npow, lag, nwt, nwf, nweight, ispws
    REAL(SGL) :: f1, f2, f3, f4, fr1, fr2, freqmin
    REAL(DBL) :: t0, tlen
    TYPE(event) :: evt_tmp
    TYPE(station) :: sta_tmp
    TYPE(record) :: rec_tmp
    TYPE(sac_db) :: sdb
    INTEGER :: nst, nev, ist, iev, nstrArray, bs_N
    CHARACTER(len=10) :: netname, staname
    REAL(SGL) :: lon, lat, dt, f11, f22, f33, f44
    CHARACTER(len=50), ALLOCATABLE, DIMENSION(:) :: strArray
    LOGICAL :: tnorm, tfilter, onebit, notch, isbs, isrecord
    LOGICAL :: isverbose, flag, isrms
    CHARACTER(len=3) :: bs_type
    INTEGER :: my_id, root_id, num_procs, nerr, ntasks, jsta1, jsta2
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
    INTEGER :: event_type, station_type, record_type
    INTEGER :: count, dest, tag, source, tmp, num_sent, jmax, i, j
    INTEGER(MPI_ADDRESS_KIND), ALLOCATABLE, DIMENSION(:) :: base, disp
    INTEGER, ALLOCATABLE, DIMENSION(:) :: blocklen, types
    INTEGER, DIMENSION(2) :: task_info

    ! ***********************************************************************
    ! Initialize MPI.
    ! ***********************************************************************
    root_id = 0
    CALL MPI_INIT(nerr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, nerr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, nerr)

    ! ***********************************************************************
    ! Construct new event data type for later data sharing.
    ! ***********************************************************************
    count = 4
    ALLOCATE(base(count), disp(count), blocklen(count), types(count))
    blocklen = (/ 200, 6, 1, 1 /)
    types =(/ MPI_CHARACTER, MPI_INTEGER, MPI_REAL, MPI_DOUBLE_PRECISION /)
    CALL MPI_GET_ADDRESS(evt_tmp%name, base(1), nerr)
    CALL MPI_GET_ADDRESS(evt_tmp%yy, base(2), nerr)
    CALL MPI_GET_ADDRESS(evt_tmp%s, base(3), nerr)
    CALL MPI_GET_ADDRESS(evt_tmp%t0, base(4), nerr)
    disp(1) = 0
    disp(2) = base(2) - base(1)
    disp(3) = base(3) - base(1)
    disp(4) = base(4) - base(1)
    CALL MPI_TYPE_CREATE_STRUCT(count, blocklen, disp, types, event_type, nerr)
    CALL MPI_TYPE_COMMIT(event_type, nerr)
    DEALLOCATE(base, disp, blocklen, types)

    ! ***********************************************************************
    ! Construct new station data type for later data sharing.
    ! ***********************************************************************
    count = 3
    ALLOCATE(base(count), disp(count), blocklen(count), types(count))
    blocklen = (/ 6, 10, 2 /)
    types =(/ MPI_CHARACTER, MPI_CHARACTER, MPI_REAL /)
    CALL MPI_GET_ADDRESS(sta_tmp%name, base(1), nerr)
    CALL MPI_GET_ADDRESS(sta_tmp%n_name, base(2), nerr)
    CALL MPI_GET_ADDRESS(sta_tmp%lat, base(3), nerr)
    disp(1) = 0
    disp(2) = base(2) - base(1)
    disp(3) = base(3) - base(1)
    CALL MPI_TYPE_CREATE_STRUCT(count, blocklen, disp, types, station_type, nerr)
    CALL MPI_TYPE_COMMIT(station_type, nerr)
    DEALLOCATE(base, disp, blocklen, types)

    ! ***********************************************************************
    ! Construct new record data type for later data sharing.
    ! ***********************************************************************
    count = 6
    ALLOCATE(base(count), disp(count), blocklen(count), types(count))
    blocklen = (/ 200, 3, 1, 1, 1, 1 /)
    types =(/ MPI_CHARACTER, MPI_CHARACTER, MPI_DOUBLE_PRECISION, &
        MPI_DOUBLE_PRECISION, MPI_REAL, MPI_INTEGER /)
    CALL MPI_GET_ADDRESS(rec_tmp%name, base(1), nerr)
    CALL MPI_GET_ADDRESS(rec_tmp%channel, base(2), nerr)
    CALL MPI_GET_ADDRESS(rec_tmp%t0, base(3), nerr)
    CALL MPI_GET_ADDRESS(rec_tmp%frac, base(4), nerr)
    CALL MPI_GET_ADDRESS(rec_tmp%dt, base(5), nerr)
    CALL MPI_GET_ADDRESS(rec_tmp%nrec, base(6), nerr)
    disp(1) = 0
    disp(2) = base(2) - base(1)
    disp(3) = base(3) - base(1)
    disp(4) = base(4) - base(1)
    disp(5) = base(5) - base(1)
    disp(6) = base(6) - base(1)
    CALL MPI_TYPE_CREATE_STRUCT(count, blocklen, disp, types, record_type, nerr)
    CALL MPI_TYPE_COMMIT(record_type, nerr)
    DEALLOCATE(base, disp, blocklen, types)

    ! ***********************************************************************
    ! Master process is responsible for the pre-processing work.
    ! ***********************************************************************
    IF (my_id==root_id) THEN

        ! Exit if the number of processors is less than 2.
        IF (num_procs < 2) THEN
            WRITE(*,"(A)") 'Must have at least 2 processes!'
            CALL MPI_FINALIZE(nerr)
            RETURN
        END IF

        ! Get the number of command line argumet(s).
        n_tmp = COMMAND_ARGUMENT_COUNT()

        ! Return if the number of input argument(s) is wrong.
        IF (n_tmp /= 1) THEN
            WRITE(*,*) "Usage: AND_Driver tarfolder"
            CALL MPI_FINALIZE(nerr)
            RETURN
        END IF

        ! =====================================================================================
        ! =============================== SECTION 1 BEGINS ====================================
        ! =====================================================================================
        ! This section treats the input parameters.

        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") '                         SECTION 1 BEGINS'
        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") 'Reading input parameters ...'
        WRITE(*,"(A)") '************************************'

        ! Obtain the command line input argument.
        CALL GET_COMMAND_ARGUMENT(1, tarfolder)

        ! ***********************************************************************
        ! Read the parameters from the <input.dat> file.
        ! ***********************************************************************
        OPEN(UNIT=10,FILE='input.dat',STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: Can not open input.dat!'
            CLOSE(UNIT=10)
            CALL MPI_FINALIZE(nerr)
            RETURN
        END IF

        DO i = 1, 3
            READ(10, *)
        END DO
        READ(10, *) channel
        READ(10, *) f1, f2, f3, f4
        READ(10, *) f11, f22, f33, f44
        READ(10, *) t0, tlen
        READ(10, *) npow
        READ(10, *) stnorm, nwt, stfilter, fr1, fr2
        READ(10, *) sonebit
        READ(10, *) nwf
        READ(10, *) snotch, freqmin
        READ(10, *) lag
        READ(10, *) spws, nweight
        READ(10, *) srms
        DO i = 1, 5
            READ(10, *)
        END DO
        READ(10, *) sbs, bs_N
        READ(10, *) bs_type
        DO i = 1, 20
            READ(10, *)
        END DO
        READ(10, *) srecord
        READ(10, *) sverbose
        CLOSE(UNIT=10)

        ! ***********************************************************************
        ! Convert the periods to frequencies.
        ! ***********************************************************************
        f1 = 1.0/f1
        f2 = 1.0/f2
        f3 = 1.0/f3
        f4 = 1.0/f4
        f11 = 1.0/f11
        f22 = 1.0/f22
        f33 = 1.0/f33
        f44 = 1.0/f44
        fr1 = 1.0/fr1
        fr2 = 1.0/fr2

        ! ***********************************************************************
        ! Initialize the logical variables.
        ! ***********************************************************************
        tnorm = .FALSE.
        tfilter = .FALSE.
        onebit = .FALSE.
        notch = .FALSE.
        isbs = .FALSE.
        isrecord = .FALSE.
        isverbose = .FALSE.
        isrms = .FALSE.
        ispws = 0
        IF (stnorm == 'Y' .OR. stnorm == 'y') tnorm = .TRUE.
        IF (stfilter == 'Y' .OR. stfilter == 'y') tfilter = .TRUE.
        IF (sonebit == 'Y' .OR. sonebit == 'y') onebit = .TRUE.
        IF (snotch == 'Y' .OR. snotch == 'y') notch = .TRUE.
        IF (sbs == 'Y' .OR. sbs == 'y') isbs = .TRUE.
        IF (srecord == 'Y' .OR. srecord == 'y') isrecord = .TRUE.
        IF (sverbose == 'Y' .OR. sverbose == 'y') isverbose = .TRUE.
        IF (spws == 'Y' .OR. spws == 'y') ispws = 1
        IF (srms == 'Y' .OR. srms == 'y') isrms = .TRUE.

        WRITE(*,"(A,/)") 'Reading input parameters FINISHED.'

        ! ***********************************************************************
        ! Clear the [DATA] folder inside the target folder.
        ! ***********************************************************************
        CALL SYSTEM('rm -rf '//TRIM(tarfolder)//'/DATA')

        ! =====================================================================================
        ! =============================== SECTION 2 BEGINS ====================================
        ! =====================================================================================
        ! This section processes the sac files and fill in the elements in the sdb struct.

        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") '                         SECTION 2 BEGINS'
        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") 'Copying sac files and Constructing sdb struct ...'
        WRITE(*,"(A)") '************************************'

        ! ***********************************************************************
        ! Count the number of stations.
        ! ***********************************************************************
        OPEN(UNIT=11,FILE='stations.lst',STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: Can not open stations.lst!'
            CALL MPI_FINALIZE(nerr)
            CLOSE(UNIT=11)
            RETURN
        END IF
        nst = 0
        DO
            READ(11, *, IOSTAT=nerr)
            IF (nerr /= 0) EXIT
            nst = nst + 1
        END DO
        CLOSE(UNIT=11)

        ! ***********************************************************************
        ! Count the number of events.
        ! ***********************************************************************
        OPEN(UNIT=12,FILE='events.lst',STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        IF (nerr /= 0) THEN
            WRITE(*,"(A)") 'Error: Can not open events.lst!'
            CALL MPI_FINALIZE(nerr)
            CLOSE(UNIT=12)
            RETURN
        END IF
        nev = 0
        DO
            READ(12, *, IOSTAT=nerr)
            IF (nerr /= 0) EXIT
            nev = nev + 1
        END DO
        CLOSE(UNIT=12)

    END IF

    ! ***********************************************************************
    ! Broadcast input parametes from the master proces to all other processes.
    ! ***********************************************************************
    CALL MPI_BCAST(nev, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(nst, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f1, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f2, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f3, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f4, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f11, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f22, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f33, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(f44, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(t0, 1, MPI_DOUBLE_PRECISION, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(tlen, 1, MPI_DOUBLE_PRECISION, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(npow, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(tnorm, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(tfilter, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(onebit, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(notch, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(fr1, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(fr2, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(nwf, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(freqmin, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(isbs, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(bs_N, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(bs_type, 3, MPI_CHARACTER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(lag, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(ispws, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(isrms, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(nweight, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(tarfolder, 200, MPI_CHARACTER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(isrecord, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(isverbose, 1, MPI_LOGICAL, root_id, MPI_COMM_WORLD, nerr)

    ! ***********************************************************************
    ! Allocate memory for the station, event and record elements in sdb.
    ! ***********************************************************************
    ALLOCATE(sdb%st(nst), sdb%ev(nev), sdb%rec(nev,nst))

    ! ***********************************************************************
    ! sdb elements are filled in the master process.
    ! ***********************************************************************
    IF (my_id==root_id) THEN

        ! ***********************************************************************
        ! Fill the station elements (lon, lat, sta, net.sta) into sdb.st.
        ! ***********************************************************************
        OPEN(UNIT=13,FILE='stations.lst',STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        ist = 0
        DO
            READ(13, *, IOSTAT=nerr) netname, staname, lon, lat
            IF (nerr /= 0) EXIT
            ist = ist + 1
            sdb%st(ist)%name = TRIM(staname)
            sdb%st(ist)%n_name = TRIM(netname)//'.'//TRIM(staname)
            sdb%st(ist)%lon = lon
            sdb%st(ist)%lat = lat
        END DO

        ! Save the number of stations in sdb%nst.
        sdb%nst = nst

        CLOSE(UNIT=13)

        ! ***********************************************************************
        ! Do the time correction and fill in the sdb.
        ! ***********************************************************************
        OPEN(UNIT=14,FILE='events.lst',STATUS='OLD',ACTION='READ',IOSTAT=nerr)
        iev = 0
        DO
            READ(14, "(A)", IOSTAT=nerr) evpath
            IF (nerr /= 0) EXIT
            iev = iev + 1

            ! Split the input event path name.
            CALL split_string(evpath, '/', strArray, nstrArray)
            str_temp =  strArray(nstrArray)

            ! ***********************************************************************
            ! Fill in the event time information into sdb.ev.
            ! ***********************************************************************
            READ(str_temp(1:4),*) sdb%ev(iev)%yy
            READ(str_temp(5:6),*) sdb%ev(iev)%mm
            READ(str_temp(7:8),*) sdb%ev(iev)%dd
            READ(str_temp(10:11),*) sdb%ev(iev)%h
            READ(str_temp(12:13),*) sdb%ev(iev)%m
            READ(str_temp(14:15),*) sdb%ev(iev)%s
            sdb%ev(iev)%jday = date2jday(sdb%ev(iev)%yy,sdb%ev(iev)%mm,sdb%ev(iev)%dd)
            sdb%ev(iev)%t0 = htoepoch(sdb%ev(iev)%yy,sdb%ev(iev)%jday,&
            sdb%ev(iev)%h,sdb%ev(iev)%m,DBLE(sdb%ev(iev)%s))

            ! ***********************************************************************
            ! Save the absolute path of the target event folder name to sdb%ev%name.
            ! ***********************************************************************
            str_temp = TRIM(tarfolder)//'/DATA/'//TRIM(strArray(nstrArray-2))//'/'//&
            TRIM(strArray(nstrArray-1))//'/'//TRIM(str_temp)
            sdb%ev(iev)%name = TRIM(str_temp)

            ! Create the target event folder.
            CALL SYSTEM('mkdir -p '//TRIM(str_temp))

            ! Loop the station to process the sac files and fill in the sdb elements.
            DO ist = 1, nst

                ! ***********************************************************************
                ! Initiate the sdb.rec.nrec and sdb.rec.frac elements.
                ! ***********************************************************************
                sdb%rec(iev,ist)%nrec = 0
                sdb%rec(iev, ist)%frac = 0.0D0

                ! ***********************************************************************
                ! Process the sac file for one record and fill in the sdb.rec info.
                ! ***********************************************************************
                CALL mk_one_rec(evpath, sdb, iev, ist, channel, isverbose)

            END DO

        END DO

        ! Save the number of events in sdb%nev.
        sdb%nev = nev

        CLOSE(UNIT=14)

        ! Find dt
        flag = .FALSE.
        DO iev = 1, nev
            IF (flag) EXIT
            DO ist = 1, nst
                IF ((sdb%rec(iev,ist)%nrec) > 0) THEN
                    dt = sdb%rec(iev,ist)%dt
                    flag = .TRUE.
                    EXIT
                ENDIF
            END DO
        END DO

        ! ***********************************************************************
        ! Determine corresponding half-window length for time domain normalization
        ! Note: Maximum allowed half-window length is 128 in sac for smooth command
        ! ***********************************************************************
        ! If you don't change the source code in sac, you should use the following
        ! less resonable sentence.
        ! ***********************************************************************
        ! nwt = MIN(INT(nwt/dt), 128)

        ! ***********************************************************************
        ! If you have changed the source code in sac, you can use the following
        ! more resonable sentence. Change should be done to line 13 in sac-101.6a
        ! /src/scm/xsmooth.c:
        ! #define	MHALF	128 change to #define	MHALF	1024
        ! ***********************************************************************
        nwt = INT(nwt/dt)

        ! ***********************************************************************
        ! Write the info database into a ascii file if isrecord is true.
        ! ***********************************************************************
        IF (isrecord) THEN
            CALL sacdb_to_asc(sdb, 'DataRecord.lst')
            WRITE(*,"(A)") 'SAC data records are written into DataRecord.lst.'
        END IF

        WRITE(*,"(A,/)") 'Constructing sdb struct FINISHED.'

    END IF

    ! ***********************************************************************
    ! Broadcast all the elements in sdb to all other processes.
    ! ***********************************************************************
    CALL MPI_BCAST(sdb%ev, nev, event_type, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(sdb%st, nst, station_type, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(sdb%rec, nev*nst, record_type, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(sdb%nev, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(sdb%nst, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)

    ! ***********************************************************************
    ! Broadcast dt and nwt to all other processes.
    ! ***********************************************************************
    CALL MPI_BCAST(dt, 1, MPI_REAL, root_id, MPI_COMM_WORLD, nerr)
    CALL MPI_BCAST(nwt, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, nerr)


    IF (my_id==root_id) THEN

        ! =====================================================================================
        ! =============================== SECTION 3 BEGINS ====================================
        ! =====================================================================================
        ! This section removes the instrument response, cut the data, do the band-pass filtering,
        ! correct the time fraction, do the time domain normalization and spectra whitening.
        ! All the tasks are done using the so called self-scheduling mode.

        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") '                         SECTION 3 BEGINS'
        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") 'Data pre-processing ...'
        WRITE(*,"(A)") '************************************'

        ! ***********************************************************************
        ! Initiate the process.
        ! ***********************************************************************
        num_sent = 0
        iev = 1
        ist = 1
        jmax = MIN(num_procs-1, nev*nst)

        ! ***********************************************************************
        ! Distribute jmax tasks to the slave processes first.
        ! ***********************************************************************
        DO j = 1, jmax
            task_info(1) = iev
            task_info(2) = ist
            dest = j
            tag = j

            ! ***********************************************************************
            ! Send the (iev, ist) info to the slave process.
            ! ***********************************************************************
            CALL MPI_SEND(task_info, 2, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, nerr)

            ist = ist + 1
            IF (ist > nst) THEN
                ist = 1
                iev = iev + 1
            END IF
            num_sent = num_sent + 1
        END DO

        ! ***********************************************************************
        ! Loop through all the tasks (nev*nst).
        ! ***********************************************************************
        DO i = 1, nev*nst

            ! ***********************************************************************
            ! Receive completion info from the slave process.
            ! ***********************************************************************
            CALL MPI_RECV(tmp, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, nerr)

            source = status(MPI_SOURCE)
            tag = status(MPI_TAG)

            ! ***********************************************************************
            ! Distribute remaining tasks to the slave processes.
            ! ***********************************************************************
            IF (num_sent<nev*nst) THEN
                task_info(1) = iev
                task_info(2) = ist

                dest = source
                tag = num_sent + 1

                ! ***********************************************************************
                ! Continue to send one task to this hard-working process.
                ! ***********************************************************************
                CALL MPI_SEND(task_info, 2, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, nerr)

                ist = ist + 1
                IF (ist > nst) THEN
                    ist = 1
                    iev = iev + 1
                END IF
                num_sent = num_sent + 1

            ! ***********************************************************************
            ! Send a terminal message to stop the slave process if no more tasks left.
            ! ***********************************************************************
            ELSE
                dest = source
                tag = 0

                CALL MPI_SEND(1.0, 1, MPI_REAL, dest, tag, MPI_COMM_WORLD, nerr)

            END IF

        END DO

        WRITE(*,"(A,/)") 'Data pre-processing FINISHED.'

    ELSE IF (my_id<=nev*nst) THEN

        ! ***********************************************************************
        ! Slave processes are responsible for the calculation work.
        ! ***********************************************************************
        DO
            CALL MPI_RECV(task_info, 2, MPI_INTEGER, root_id, MPI_ANY_TAG, MPI_COMM_WORLD, status, nerr)

            tag = status(MPI_TAG)

            ! ***********************************************************************
            ! Exit when receiving the stop message.
            ! ***********************************************************************
            IF (tag==0) EXIT

            ! ***********************************************************************
            ! Data processing for each daily sac file.
            ! ***********************************************************************
            CALL remove_RESP(sdb, task_info(1), task_info(2), my_id, f1, f2, f3, f4, isverbose)
            CALL cut_data(sdb, task_info(1), task_info(2), t0, tlen, isverbose)
            CALL frac_filter4(sdb, task_info(1), task_info(2), f11, f22, f33, f44, npow, isverbose)
            CALL white_reject_filter4(sdb, task_info(1), task_info(2), my_id, tnorm, onebit,&
            notch, f11, f22, f33, f44, tfilter, fr1, fr2, npow, nwt, nwf, freqmin, isverbose)

            ! ***********************************************************************
            ! Send the completion message to the master process.
            ! ***********************************************************************
            CALL MPI_SEND(1, 1, MPI_INTEGER, root_id, tag, MPI_COMM_WORLD, nerr)

        END DO

    ELSE

        WRITE(*,"(A,I3,A)") 'Process ', my_id, ' has nothing to do in SECTION 3!'

    END IF

    CALL MPI_BARRIER(MPI_COMM_WORLD, nerr)


    IF (my_id==root_id) THEN

        ! =====================================================================================
        ! =============================== SECTION 4 BEGINS ====================================
        ! =====================================================================================
        ! This section Computes the cross-correlation and measures the dispersion curves.

        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") '                         SECTION 4 BEGINS'
        WRITE(*,"(A)") '***********************************************************************'
        WRITE(*,"(A)") 'Doing cross-correlation and AFTAN ...'
        WRITE(*,"(A)") '************************************'

        ! Delete the CCRecord.lst file if isrecord is true.
        IF(isrecord) CALL SYSTEM('rm -rf CCRecord.lst')

        ! Delete the RMSR_SS.lst file if isrms is true.
        IF(isrms) CALL SYSTEM('rm -rf RMSR_SS.lst')

        ! ***********************************************************************
        ! Use self-scheduling mode again.
        ! ***********************************************************************
        num_sent = 0
        jsta1 = 1
        jsta2 = jsta1 + 1
        ntasks = nst*(nst-1)/2
        jmax = MIN(num_procs-1, ntasks)

        ! ***********************************************************************
        ! Distribute jmax tasks to the slave processes fist.
        ! ***********************************************************************
        DO j = 1, jmax
            task_info(1) = jsta1
            task_info(2) = jsta2
            dest = j
            tag = j

            ! ***********************************************************************
            ! Send the (jsta1, jsta2) info to the slave process.
            ! ***********************************************************************
            CALL MPI_SEND(task_info, 2, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, nerr)
            jsta2 = jsta2 + 1
            IF (jsta2 > nst) THEN
                jsta1 = jsta1 + 1
                jsta2 = jsta1 + 1
            END IF
            num_sent = num_sent + 1
        END DO

        ! ***********************************************************************
        ! Loop through all the tasks (ntasks=nst*(nst-1)/2).
        ! ***********************************************************************
        DO i = 1, ntasks

            ! ***********************************************************************
            ! Receive completion info from the slave process.
            ! ***********************************************************************
            CALL MPI_RECV(tmp, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, nerr)
            source = status(MPI_SOURCE)
            tag = status(MPI_TAG)

            ! ***********************************************************************
            ! Distribute remaining tasks to the slave processes.
            ! ***********************************************************************
            IF (num_sent<ntasks) THEN
                task_info(1) = jsta1
                task_info(2) = jsta2

                dest = source
                tag = num_sent + 1

                ! ***********************************************************************
                ! Continue to send one task to this hard-working process.
                ! ***********************************************************************
                CALL MPI_SEND(task_info, 2, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, nerr)

                jsta2 = jsta2 + 1
                IF (jsta2 > nst) THEN
                    jsta1 = jsta1 + 1
                    jsta2 = jsta1 + 1
                END IF
                num_sent = num_sent + 1
            ELSE
                dest = source
                tag = 0

                ! ***********************************************************************
                ! Send a terminal message to stop the slave process if no more tasks left.
                ! ***********************************************************************
                CALL MPI_SEND(1.0, 1, MPI_REAL, dest, tag, MPI_COMM_WORLD, nerr)
            END IF

        END DO

    ELSE IF (my_id<=nst*(nst-1)/2) THEN

        ! ***********************************************************************
        ! Slave processes are responsible for the calculation work.
        ! ***********************************************************************
        DO
            CALL MPI_RECV(task_info, 2, MPI_INTEGER, root_id, MPI_ANY_TAG, MPI_COMM_WORLD, status, nerr)

            tag = status(MPI_TAG)

            ! ***********************************************************************
            ! Exit when receiving the stop message
            ! ***********************************************************************
            IF (tag==0) EXIT

            ! ***********************************************************************
            ! Cross-correlation calculation and dispersion measurements are done here.
            ! ***********************************************************************
            CALL cc_and_aftan(sdb,INT(lag/dt),tarfolder,isbs,bs_N,bs_type,task_info(1),&
            task_info(2),my_id,isverbose,isrecord,f11,f22,f33,f44,ispws,nweight,isrms)

            ! ***********************************************************************
            ! Send the completion message to the master process
            ! ***********************************************************************
            CALL MPI_SEND(1, 1, MPI_INTEGER, root_id, tag, MPI_COMM_WORLD, nerr)

        END DO

    ELSE

        WRITE(*,"(A,I3,A)") 'Process ', my_id, ' has nothing to do in SECTION 4!'

    END IF


    CALL MPI_BARRIER(MPI_COMM_WORLD, nerr)

    IF (my_id == 0) THEN

        ! ***********************************************************************
        ! Destroy the DATA folder.
        ! ***********************************************************************
        CALL SYSTEM('rm -rf '//TRIM(tarfolder)//'/DATA')

        ! ***********************************************************************
        ! Remove possible empty folder(s) and file(s).
        ! ***********************************************************************
        CALL SYSTEM("find "//TRIM(tarfolder)//" -depth -type 'd' -empty -exec rmdir {} \;")
        CALL SYSTEM("find "//TRIM(tarfolder)//' -name "*" -type f -size 0c | xargs -n 1 rm -f')

        ! ***********************************************************************
        ! Deallocate memory for the elements in sdb and strArray.
        ! ***********************************************************************
        DEALLOCATE(sdb%st, sdb%ev, sdb%rec, strArray)

        WRITE(*,"(A)") 'Cross-correlation and AFTAN FINISHED.'

    END IF

    CALL MPI_FINALIZE(nerr)

END PROGRAM AND_Driver
