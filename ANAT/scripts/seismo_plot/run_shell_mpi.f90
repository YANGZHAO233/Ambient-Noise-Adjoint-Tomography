! Compile: mpifort run_shell_mpi.f90 -o run_shell_mpi
!          sudo mv run_shell_mpi /usr/local/bin/

PROGRAM run_shell_mpi
    USE mpi
    IMPLICIT NONE

    INTEGER, PARAMETER :: maxlen = 500
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
    INTEGER :: ierr, num_procs, my_id, root_id
    INTEGER :: num_sent, nlines, i, j, jmax, dest, tag, source, dummy
    CHARACTER(LEN=maxlen) :: str_temp

    dummy = 1
    root_id = 0

    ! =====================================================
    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)


    ! =====================================================
    IF (my_id==root_id) THEN

        ! Exit the program if less than 2 processors are avaliable
        IF (num_procs < 2) THEN
            WRITE(*,"(A)") 'Must have at least 2 processes!'
            CALL MPI_FINALIZE(ierr)
            RETURN
        END IF

        ! Read the parameters
        nlines = 0
        OPEN(UNIT=10,FILE=TRIM('input.lst'),ACTION='READ',STATUS='OLD')
        DO
            READ(10,*,IOSTAT=ierr)
            IF (ierr/=0) EXIT
            nlines = nlines + 1
        END DO
        CLOSE(UNIT=10)

    END IF

    CALL MPI_BCAST(nlines, 1, MPI_INTEGER, root_id, MPI_COMM_WORLD, ierr)


    ! =====================================================
    IF (my_id==root_id) THEN

        OPEN(UNIT=10,FILE='input.lst',ACTION='READ',STATUS='OLD')

        ! Send initial tasks to the slave processes
        num_sent = 0
        jmax = MIN(num_procs-1, nlines)
        DO j = 1, jmax

            READ(10,"(A)") str_temp
            dest = j
            tag = j
            CALL MPI_SEND(str_temp, maxlen, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)
            num_sent = num_sent + 1

        END DO

        DO i = 1, nlines

            CALL MPI_RECV(dummy, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)

            source = status(MPI_SOURCE)
            tag = status(MPI_TAG)

            IF (num_sent<nlines) THEN

                READ(10,"(A)") str_temp
                num_sent = num_sent + 1
                dest = source
                tag = num_sent
                CALL MPI_SEND(str_temp, maxlen, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)

            ELSE

                dest = source
                tag = 0

                CALL MPI_SEND('END OF FILE!', 12, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)

            END IF

        END DO

        CLOSE(UNIT=10)

    ELSE IF (my_id<=nlines) THEN
        DO

            CALL MPI_RECV(str_temp, maxlen, MPI_CHARACTER, root_id, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)

            tag = status(MPI_TAG)

            IF (tag==0) EXIT

            CALL SYSTEM(TRIM(str_temp))

            CALL MPI_SEND(dummy, 1, MPI_INTEGER, root_id, tag, MPI_COMM_WORLD, ierr)
        END DO

    ELSE
        WRITE(*,"(A,I3,A)") 'Process ', my_id, ' has nothing to do!'
    END IF


    CALL MPI_FINALIZE(ierr)

END PROGRAM run_shell_mpi
