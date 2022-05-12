MODULE my_data_io

    USE my_definition
    USE sac_io

    IMPLICIT NONE

CONTAINS

    ! ====================================================================
    ! Write complex data into a local binary file
    ! filename: file name of the bianary file [input]
    ! n: number of points of the data [input]
    ! comdata: complex data array [input]
    ! nerr: status indicator [output]
    ! ====================================================================
    SUBROUTINE write_mydata(filename, n, comdata, nerr)
        IMPLICIT NONE

        CHARACTER(len=*), INTENT(IN) :: filename
        INTEGER, INTENT(IN) :: n
        COMPLEX(SGL), DIMENSION(:), INTENT(IN) :: comdata
        INTEGER, INTENT(OUT) :: nerr

        OPEN(UNIT=55,FILE=filename,STATUS='REPLACE',ACTION='WRITE',&
        IOSTAT=nerr,ACCESS='STREAM',FORM='UNFORMATTED')
        IF (nerr /= 0) THEN
            WRITE(*,"(A,A)") "Error: Can not open:", filename
            RETURN
        END IF
        WRITE(55) n
        WRITE(55) comdata(1:n)
        CLOSE(UNIT=55)

    END SUBROUTINE write_mydata


    ! ====================================================================
    ! Read complex data from a local binary file
    ! filename: file name of the bianary file [input]
    ! n: number of points to be read [output]
    ! comdata: complex data array [output]
    ! nerr: status indicator [output]
    ! ====================================================================
    SUBROUTINE read_mydata(filename, n, comdata, nerr)
        IMPLICIT NONE

        CHARACTER(len=*), INTENT(IN) :: filename
        INTEGER, INTENT(OUT) :: n
        COMPLEX(SGL), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: comdata
        INTEGER, INTENT(OUT) :: nerr

        OPEN(UNIT=55,FILE=filename,STATUS='OLD',ACTION='READ',&
        IOSTAT=nerr,ACCESS='STREAM',FORM='UNFORMATTED')
        IF (nerr /= 0) THEN
            WRITE(*,"(A,A)") "Error: Can not read:", filename
            RETURN
        END IF
        READ(55) n
        ALLOCATE(comdata(n), STAT=nerr)
        READ(55) comdata
        CLOSE(UNIT=55)

    END SUBROUTINE read_mydata


    ! ====================================================================
    ! Convert cross-correlation from sac format to ascii format
    ! in order to generate input files for Huajian Yao' code
    ! filename: file name of the cross-correlation file in sac format [input]
    ! ====================================================================
    SUBROUTINE sac_to_asc(filename)
        IMPLICIT NONE

        CHARACTER(len=*), INTENT(IN) :: filename

        CHARACTER(len=200) :: filename2
        LOGICAL :: alive
        INTEGER :: nerr, n, k, n0
        REAL, ALLOCATABLE, DIMENSION(:) :: data
        REAL :: dt, stlo, stla, evlo, evla
        TYPE(sachead) :: sachd

        INQUIRE(FILE=filename, EXIST=alive)
        IF (.NOT. alive) RETURN

        CALL sacio_readsac(filename, sachd, data, nerr)

        n = sachd%npts
        dt = sachd%delta
        stlo = sachd%stlo
        stla = sachd%stla
        evlo = sachd%evlo
        evla = sachd%evla

        n0 = n/2 + 1

        filename2 = TRIM(filename)//'.txt'
        OPEN(UNIT=11,FILE=filename2,ACTION='WRITE',STATUS='REPLACE')
        WRITE(11,"(2F18.5)") stlo, stla
        WRITE(11,"(2F18.5)") evlo, evla
        DO k = 1, n0
            WRITE(11,"(3F18.5)") (k-1)*dt, data(n0+k-1), data(n0-k+1)
        END DO

    END SUBROUTINE sac_to_asc

END MODULE my_data_io
