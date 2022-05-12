MODULE addition
    IMPLICIT NONE

    INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(5,20)
    INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(13,100)

CONTAINS

    ! ************************************************************
    ! Write the dispersion results after linear interpolation.
    ! array1: raw measurement matrix [input]
    ! nfout1: # of raw measurements (# of columns) [input]
    ! array2: clean measurement matrix [input]
    ! nfout2: # of clean measurements (# of columns) [input]
    ! filename: output file name [input]
    ! ************************************************************
    SUBROUTINE write_data(array1, nfout1, array2, nfout2, filename)
        IMPLICIT NONE

        REAL(DBL), DIMENSION(8,100), INTENT(IN) :: array1
        REAL(DBL), DIMENSION(7,100), INTENT(IN) :: array2

        INTEGER, INTENT(IN) :: nfout1, nfout2
        CHARACTER(len=*), INTENT(IN) :: filename

        INTEGER :: lowPeriod, highPeriod, n, i, nerr
        REAL(DBL), ALLOCATABLE, DIMENSION(:) :: x, y, z

        IF (nfout1>0) THEN
            lowPeriod = NINT(MINVAL(array1(2,1:nfout1)))
            highPeriod = NINT(MAXVAL(array1(2,1:nfout1)))
            n = highPeriod - lowPeriod + 1

            ALLOCATE(x(n), y(n), z(n))

            DO i = 1, n
                x(i) = lowPeriod + i - 1
            END DO

            CALL linear_interpo(array1(2,1:nfout1), array1(3,1:nfout1), x, y, nerr)
            CALL linear_interpo(array1(2,1:nfout1), array1(4,1:nfout1), x, z, nerr)

            OPEN(UNIT=33, FILE=TRIM(filename)//'_1', STATUS='REPLACE', ACTION='WRITE')
            DO i = 1, n
                WRITE(33, "(I6,2F12.4)") INT(x(i)), y(i), z(i)
            END DO
            CLOSE(UNIT=33)

            DEALLOCATE(x, y, z)
        END IF

        ! ************************************************************
        IF (nfout2>0) THEN
            lowPeriod = NINT(MINVAL(array2(2,1:nfout2)))
            highPeriod = NINT(MAXVAL(array2(2,1:nfout2)))
            n = highPeriod - lowPeriod + 1

            ALLOCATE(x(n), y(n), z(n))

            DO i = 1, n
                x(i) = lowPeriod + i - 1
            END DO

            CALL linear_interpo(array2(2,1:nfout2), array2(3,1:nfout2), x, y, nerr)
            CALL linear_interpo(array2(2,1:nfout2), array2(4,1:nfout2), x, z, nerr)

            OPEN(UNIT=33, FILE=TRIM(filename)//'_2', STATUS='REPLACE', ACTION='WRITE')
            DO i = 1, n
                WRITE(33, "(I6,2F12.4)") INT(x(i)), y(i), z(i)
            END DO
            CLOSE(UNIT=33)

            DEALLOCATE(x, y, z)
        END IF

    END SUBROUTINE write_data


    ! ************************************************************
    ! Sort the array to make sure the array value increases
    ! mononically.
    ! array_x: x array (e.g., period) [input and output]
    ! array_y: y array (e.g., phase velocity) [input and output]
    ! ************************************************************
    SUBROUTINE sort_array_xy(array_x, array_y)
      IMPLICIT NONE

      REAL(DBL), DIMENSION(:), INTENT(INOUT) :: array_x, array_y

      INTEGER :: n, i, j, ip
      REAL(DBL) :: temp, ip_x, ip_y

      n = SIZE(array_x)
      DO i = 1, n-1
        ip = i
        ip_x = array_x(i)
        DO j = i+1, n
          IF (array_x(j) < ip_x) THEN
            ip = j
            ip_x = array_x(j)
            ip_y = array_y(j)
          END IF
        END DO
        IF (i /= ip) THEN
          temp = array_x(i)
          array_x(i) = ip_x
          array_x(ip) = temp
          temp = array_y(i)
          array_y(i) = ip_y
          array_y(ip) = temp
        END IF
      END DO

    END SUBROUTINE sort_array_xy


    ! ************************************************************
    ! Check if there are duplicates in the array
    ! array: input array [input]
    ! ************************************************************
    LOGICAL FUNCTION any_near_dupl(array)
        IMPLICIT NONE

        REAL(DBL), DIMENSION(:), INTENT(IN) :: array
        INTEGER :: n, i

        any_near_dupl = .FALSE.
        n = SIZE(array)
        IF (n==1) RETURN
        DO i = 1, n-1
          IF (array(i) == array(i+1)) THEN
              any_near_dupl = .TRUE.
              RETURN
          END IF
        END DO

    END FUNCTION any_near_dupl

    ! ************************************************************
    ! Linear interpolation
    ! x0, y0: input data points [input]
    ! x: inquired x values [input]
    ! y: computed y values [output]
    ! nerr: status indicator [output]
    ! ************************************************************
    SUBROUTINE linear_interpo(x0, y0, x, y, nerr)
        IMPLICIT NONE
        REAL(DBL), DIMENSION(:), INTENT(IN) :: x0, y0, x
        REAL(DBL), DIMENSION(:), INTENT(OUT) :: y
        INTEGER, INTENT(OUT) :: nerr

        INTEGER :: n1, n2, k, i
        REAL(DBL), ALLOCATABLE, DIMENSION(:) :: x0_c, y0_c

        nerr = 1
        n1 = SIZE(x0)
        n2 = SIZE(x)

        ALLOCATE(x0_c(n1), y0_c(n1))

        x0_c = x0
        y0_c = y0

        CALL sort_array_xy(x0_c, y0_c)

        IF (any_near_dupl(x0_c)) THEN
            WRITE(*,"(A)") 'NO duplicates allowed in x0!'
            RETURN
        END IF

        DO k = 1, n2
            IF (x(k)<=x0_c(1)) THEN
                y(k) = (y0_c(2)-y0_c(1))/(x0_c(2)-x0_c(1))*(x(k)-x0_c(1))+y0_c(1)
            ELSEIF (x(k)>=x0_c(n1)) THEN
                y(k) = (y0_c(n1)-y0_c(n1-1))/(x0_c(n1)-x0_c(n1-1))*(x(k)-x0_c(n1-1))+y0_c(n1-1)
            ELSE
                DO i = 2, n1
                    IF (x(k) <= x0_c(i)) EXIT
                END DO
                y(k) = (y0_c(i)-y0_c(i-1))/(x0_c(i)-x0_c(i-1))*(x(k)-x0_c(i-1))+y0_c(i-1)
            END IF
        END DO

        DEALLOCATE(x0_c, y0_c)
        nerr = 0

    END SUBROUTINE linear_interpo

END MODULE addition
