MODULE math
    USE my_definition
    IMPLICIT NONE

CONTAINS

    ! ***************************************************************
    ! This program is downloaded from http://fcode.cn/guide-96-1.html
    ! After invoke this subroutine, RANDOM_NUMBER can produce different
    ! random numbers in the range of [0 1]. Without using this subroutine,
    ! RANDOM_NUMBER will produce the same random numbers every time.
    ! ***************************************************************
    SUBROUTINE init_random_seed()
        INTEGER :: ised , i , pid
        INTEGER(DBL) :: t
        INTEGER, ALLOCATABLE, DIMENSION(:) :: sed

        CALL RANDOM_SEED(size = ised) ! Get the size of the seed
        ALLOCATE(sed(ised)) ! Distribute the seed
        CALL SYSTEM_CLOCK(t) ! Get the time
        pid = GETPID() ! Get the id of the processor
        t = IEOR(t, INT(pid, KIND(t))) ! XOR operation
        DO i = 1, ised
            sed(i) = lcg(t) ! LCG operation
        END DO
        CALL RANDOM_SEED(put=sed) ! Set the seed value
    End SUBROUTINE init_random_seed

    ! ***************************************************************
    ! Linear congruential generator
    ! ***************************************************************
    FUNCTION lcg(s)
        INTEGER :: lcg
        INTEGER(DBL) :: s

        IF (s == 0) THEN
            s = 104729
        ELSE
            s = MOD(s, 4294967296_DBL)
        END IF
        s = MOD(s * 279470273_DBL, 4294967291_DBL)
        lcg = INT(MOD(s, INT(HUGE(0), DBL)), KIND(0))
    End FUNCTION lcg


    ! ***************************************************************
    ! ***************************************************************
    SUBROUTINE matrix_mean_std(A, mean, std, skip_value)
        IMPLICIT NONE
        REAL(DBL), DIMENSION(:,:), INTENT(IN) :: A
        REAL(DBL), DIMENSION(:), INTENT(OUT) :: mean, std
        REAL(DBL), INTENT(IN) :: skip_value

        REAL(DBL) :: sum, sum2
        INTEGER :: xrow, xcol, i, j, n
        xrow = SIZE(A,1)
        xcol = SIZE(A,2)

        DO i = 1, xcol
            sum = 0.0D0
            sum2 = 0.0D0
            n = 0
            mean(i) = 0.0D0
            std(i) = 0.0D0
            DO j = 1, xrow
                IF (ABS(A(j,i)-skip_value) > EPSILON(A(j,i))) THEN
                    sum = sum + A(j,i)
                    sum2 = sum2 + A(j,i)**2
                    n = n + 1
                END IF
            END DO
            IF (n>=1) mean(i) = sum/DBLE(n)
            IF (n>=2) std(i) = SQRT(ABS(DBLE(n)*sum2-sum**2)/(DBLE(n)*DBLE(n-1)))
        END DO
    END SUBROUTINE matrix_mean_std


    ! ***************************************************************
    ! Compute the root mean square value of a given array
    ! ***************************************************************
    REAL(DBL) FUNCTION rms(array)

        IMPLICIT NONE

        REAL(DBL), INTENT(IN), DIMENSION(:) :: array
        INTEGER :: n, i
        REAL(DBL) :: sum

        n = SIZE(array)

        sum = 0.0
        DO i = 1, n
            sum = sum + array(i)*array(i)
        END DO

        rms = SQRT(sum/n)

    END FUNCTION rms


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

END MODULE math
