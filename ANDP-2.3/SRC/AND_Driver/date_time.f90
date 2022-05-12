MODULE date_time

    USE my_definition

    IMPLICIT NONE

CONTAINS

    ! ====================================================================
    ! Convert the date to julian day
    ! year: year [input]
    ! month: month [input]
    ! day: day [input]
    ! date2jday: converted julian day [output]
    ! ====================================================================
    INTEGER FUNCTION date2jday(year, month, day)

        INTEGER, INTENT(IN) :: year, month, day

        INTEGER :: i, leap_day

        IF (MOD(year, 400) == 0) THEN
            leap_day = 1
        ELSE IF (MOD(year, 100) == 0) THEN
            leap_day = 0
        ELSE IF (MOD(year, 4) == 0) THEN
            leap_day = 1
        ELSE
            leap_day = 0
        END IF

        date2jday = day

        DO i = 1, month-1
            SELECT CASE (i)
            CASE (1,3,5,7,8,10,12)
                date2jday = date2jday + 31
            CASE (4,6,9,11)
                date2jday = date2jday + 30
            CASE (2)
                date2jday = date2jday + 28 + leap_day
            END SELECT
        END DO

    END FUNCTION date2jday


    ! ====================================================================
    ! Convert human-readable time to epoch time relative to 1970 in seconds
    ! year: year [input]
    ! jday: julian day [input]
    ! hour: hour [input]
    ! min: minute [input]
    ! sec: second [input]
    ! htoepoch: epoch time in seconds [output]
    ! ====================================================================
    REAL(DBL) FUNCTION htoepoch(year, jday, hour, min, sec)
        INTEGER, INTENT(IN) :: year, jday, hour, min
        REAL(DBL), INTENT(IN) :: sec

        INTEGER :: yeardiff, vis

        yeardiff = year - 1970
        vis = (yeardiff+1)/4+(yeardiff+369)/400-(yeardiff+69)/100
        htoepoch = (365*yeardiff+vis+jday-1)*86400.D0+(hour*60+min)*60.D0+sec

    END FUNCTION htoepoch


    ! ====================================================================
    ! Convert epoch time to human-readable time
    ! t: epoch time relative to 1970 in seconds [input]
    ! year: year [output]
    ! jday: julian day [output]
    ! hour: hour [output]
    ! min: minute [output]
    ! sec: second [output]
    ! ====================================================================
    SUBROUTINE epochtoh(t, year, jday, hour, min, sec)
        REAL(DBL), INTENT(IN) :: t
        INTEGER, INTENT(OUT) :: year, jday, hour, min
        REAL(DBL), INTENT(OUT) :: sec

        INTEGER :: idate, itime, irest, iysupp, idsupp
        REAL(DBL) :: fsec

        idate = INT(t/86400.0)
        iysupp = idate/365
        idsupp = iysupp*365+(iysupp+1)/4+(iysupp+369)/400-(iysupp+69)/100
        IF (idate < idsupp) iysupp = iysupp - 1
        idsupp = iysupp*365+(iysupp+1)/4+(iysupp+369)/400-(iysupp+69)/100

        ! extract year
        year = 1970+iysupp
        ! extract jday
        jday = idate - idsupp+1
        ! time part
        fsec = t-idate*86400.0D0
        irest = INT(fsec)
        fsec = fsec - irest
        itime = irest
        irest = itime/60
        ! extract second
        sec = itime - irest*60 + fsec
        itime = irest
        irest = itime/60
        ! extract mininute
        min = itime - irest*60
        ! extract hour
        hour = irest

    END SUBROUTINE epochtoh

END MODULE date_time
