MODULE string
    IMPLICIT NONE
CONTAINS

    ! =============================================================================
    ! Delete spaces within one string
    ! str: input and output string [input and output]
    ! =============================================================================
    SUBROUTINE delspace(str)

        CHARACTER(len=*), INTENT(INOUT) :: str
        CHARACTER(len=200) :: str_temp, str_result
        INTEGER :: str_len

        str_result = ''

        DO
            str = TRIM(ADJUSTL(str))
            IF (str == '') EXIT
            READ(str, *) str_temp
            str_len = LEN(TRIM(str_temp))
            str(1:str_len) = ''
            str_result = TRIM(str_result) // TRIM(str_temp)
        END DO

        str = str_result

    END SUBROUTINE delspace


    ! =============================================================================
    ! Split a string by the given delimiter(s)
    ! InStr: input string [input]
    ! delimiter: string containing the delimiter(s) (e.g. '/.') [input]
    ! strArray: output string array (allocatable) [output]
    ! nsize: number of output string array [output]
    ! =============================================================================
    SUBROUTINE split_string(InStr, delimiter, strArray, nsize)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(IN) :: InStr, delimiter
        CHARACTER(len=*), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: strArray
        INTEGER, INTENT(OUT) :: nsize

        INTEGER :: i, j, istart, nstr, ndelim, ncount

        nsize = 1
        istart = 1

        nstr = LEN(TRIM(InStr))
        ndelim = LEN(TRIM(delimiter))

        ! Count how many segements the input string has
        DO i = 1, nstr
            DO j = 1, ndelim
                IF (InStr(i:i) == delimiter(j:j)) THEN
                    nsize = nsize + 1
                END IF
            END DO
        END DO

        ALLOCATE(strArray(nsize))

        ! Split the input string
        ncount = 0
        DO i = 1, nstr
            DO j = 1, ndelim
                IF (InStr(i:i) == delimiter(j:j)) THEN
                    IF (i == 1) THEN        ! the first element of the string is the delimiter
                        strArray(1) = ''
                        ncount = ncount + 1
                        istart = 2
                    ELSE
                        ncount = ncount + 1
                        strArray(ncount) = InStr(istart:i-1)
                        istart = i+1
                    END IF
                END IF
            END DO
        END DO

        IF (nsize > 1) THEN
            IF (istart <= nstr) THEN
                strArray(nsize) = InStr(istart:nstr)
            ELSE        ! the last element of the string is the delimiter
                strArray(nsize) = ''
            END IF
        ELSE            ! no spliting occured
            strArray(1) = InStr
        END IF

    END SUBROUTINE split_string


    ! =============================================================================
    ! Convert a float point number to a string with leading zeros
    ! value: input floating point number [input]
    ! m: width of the integer part [input]
    ! n: width of the decimal part [input]
    ! padzero: output formatted string [output]
    ! =============================================================================
    FUNCTION padzero(value, m, n)
        IMPLICIT NONE
        REAL, INTENT(IN) :: value
        INTEGER, INTENT(IN) :: m, n
        CHARACTER(len=m+n+1) :: padzero
        CHARACTER(len=5) :: str_temp1, str_temp2, str_temp3
        CHARACTER(len=20) :: str_temp

        WRITE(str_temp1,'(I2)') m
        WRITE(str_temp2,'(I2)') n
        WRITE(str_temp3,'(I2)') m-1

        IF (value >= 0) THEN
            str_temp = '(I'//TRIM(ADJUSTL(str_temp1))//'.'//TRIM(ADJUSTL&
            (str_temp1))//',F0.'//TRIM(ADJUSTL(str_temp2))//')'
        ELSE
            str_temp = '(I'//TRIM(ADJUSTL(str_temp1))//'.'//TRIM(ADJUSTL&
            (str_temp3))//',F0.'//TRIM(ADJUSTL(str_temp2))//')'
        END IF
        WRITE(padzero,TRIM(str_temp)) INT(value), ABS(value-INT(value))
    END FUNCTION

END MODULE string
