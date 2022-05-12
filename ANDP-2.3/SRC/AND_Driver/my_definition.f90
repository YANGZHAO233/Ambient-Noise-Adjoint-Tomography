MODULE my_definition
    IMPLICIT NONE

    ! ***********************************************************************************************
    ! ********************************* VARIABLE DEFINITION SECTION *********************************
    ! ***********************************************************************************************
    INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(5,20)
    INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(13,100)
    INTEGER, PARAMETER :: TIMEMAX = 100000 ! TIMEMAX: maximum time length of the sac file


    ! ***********************************************************************************************
    ! *********************************** TYPE DEFINITION SECTION ***********************************
    ! ***********************************************************************************************
    TYPE :: station
        CHARACTER(len=6) :: name        ! name: station name (e.g. MONP)
        CHARACTER(len=10) :: n_name     ! n_name: network.station name (e.g. AZ.MONP)
        REAL(SGL) :: lat, lon           ! lat, lon: latitude and longitude of the station
    END TYPE station

    TYPE :: event
        CHARACTER(len=200) :: name                  ! name: event name (e.g. absolute_path/20150401_000000)
        INTEGER :: yy, mm, dd, h, m, jday           ! yy,mm,dd,h,m,jday: year,month,day,hour,minute,julian day
        REAL(SGL) :: s                              ! ss: second in decimal form
        REAL(DBL) :: t0                             ! t0: epoch time of the event (e.g. 123456789)
    END TYPE event

    TYPE :: record
        CHARACTER(len=200) :: name      ! name: sac file path  (e.g. absolute_path/20150401_000000/AZ.MONP.LHZ.SAC)
        CHARACTER(len=3) :: channel     ! channel: channel name (e.g. LHZ)
        REAL(DBL) :: t0                 ! t0: epoch time of the first data point
        REAL(DBL) :: frac               ! frac: time fraction of the sac file (e.g. 0.0)
        REAL(SGL) :: dt                 ! dt: data sampling interval
        INTEGER :: nrec                 ! nrec: number of data points (should be default to 0)
    END TYPE record

    TYPE :: sac_db
        TYPE(event), ALLOCATABLE, DIMENSION(:) :: ev            ! ev: EVENT struct array
        TYPE(station), ALLOCATABLE, DIMENSION(:) :: st          ! st: STATION struct array
        TYPE(record), ALLOCATABLE, DIMENSION(:, :) :: rec       ! rec: RECORD struct array
        INTEGER :: nev, nst                                     ! nev, nst: number of events and stations
    END TYPE sac_db

END MODULE my_definition
