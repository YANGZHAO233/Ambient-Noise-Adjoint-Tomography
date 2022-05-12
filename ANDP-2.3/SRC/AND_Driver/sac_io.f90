!Copyright  2015  Liang Wang & Dongdong Tian
!
!Licensed under the Apache License, Version 2.0 (the "License");
!you may not use this file except in compliance with the License.
!You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
!Unless required by applicable law or agreed to in writing, software
!distributed under the License is distributed on an "AS IS" BASIS,
!WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!See the License for the specific language governing permissions and
!limitations under the License.!
!
!=======================================================================
!
!   sacio.f90: Fortran 90 module for SAC I/O
!
!   SAC I/O Subroutines:
!       sacio_readhead          Read SAC binary header only
!       sacio_readsac           Read SAC binary file
!       sacio_writesac          Write SAC binary file
!       sacio_readsac_cut       Read SAC binary file with cut option
!       sacio_nullhead          Change a SAC header to undefined
!       sacio_newhead           Create a ready-to-use SAC header
!
!   Error codes:
!       0:  Succeed
!       1:  Unable to open file
!       2:  Error in reading SAC header
!       3:  No enough memory to allocate
!       4:  Error in reading SAC data
!       5:  Error in writing SAC file
!       6:  File not in SAC format
!       7:  Illegal time marker
!       8:  Time tmark undefined
!       9:  Cut window outside of time range
!
!   Revision History:
!       2015-09-11  Liang Wang & Dongdong Tian   Initial Coding
!
!=======================================================================
module sac_io

public :: sacio_readhead
public :: sacio_readsac
public :: sacio_writesac
public :: sacio_readsac_cut
public :: sacio_nullhead
public :: sacio_newhead

! structure of SAC header
type :: sachead
    real :: delta, depmin, depmax, scale, odelta
    real :: b, e, o, a, internal1
    real :: t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, f
    real :: resp0, resp1, resp2, resp3, resp4
    real :: resp5, resp6, resp7, resp8, resp9
    real :: stla, stlo, stel, stdp
    real :: evla, evlo, evel, evdp, mag
    real :: user0, user1, user2, user3, user4
    real :: user5, user6, user7, user8, user9
    real :: dist, az, baz, gcarc, internal2, internal3
    real :: depmen, cmpaz, cmpinc
    real :: xminimum, xmaximum, yminimum, ymaximum
    real :: unused1, unused2, unused3, unused4, unused5, unused6, unused7
    integer :: nzyear, nzjday, nzhour, nzmin, nzsec, nzmsec
    integer :: nvhdr, norid, nevid, npts
    integer :: internal4, nwfid, nxsize, nysize, unused8
    integer :: iftype, idep, iztype, unused9, iinst
    integer :: istreg, ievreg, ievtyp, iqual, isynth
    integer :: imagtyp, imagsrc, unused10, unused11, unused12
    integer :: unused13, unused14, unused15, unused16, unused17
    integer :: leven, lpspol, lovrok, lcalda, unused18
    character(len=8)  :: kstnm
    character(len=16) :: kevnm
    character(len=8)  :: khole, ko, ka
    character(len=8)  :: kt0, kt1, kt2, kt3, kt4, kt5, kt6, kt7, kt8, kt9
    character(len=8)  :: kf, kuser0, kuser1, kuser2
    character(len=8)  :: kcmpnm, knetwk, kdatrd, kinst
end type sachead

! constants
real, parameter    :: SAC_rnull = -12345.0
integer, parameter :: SAC_inull = -12345
integer, parameter :: SAC_lnull = -12345
character(len=8)   :: SAC_cnull = '-12345'

contains

!
!   sacio_readhead
!
!   Description: Read SAC binary header only
!   Input:
!       character(len=80) :: filename   filename to be read
!   Output:
!       type(sachead)     :: head       SAC header to be filled
!       integer           :: flag       Error code
!
subroutine sacio_readhead(filename, head, flag)
implicit none
character(len=*), intent(in) :: filename
type(sachead), intent(inout) :: head
integer, intent(inout) :: flag

open(unit=50, file=filename, status='old', action='read', iostat=flag, &
    & access='stream', form='unformatted', convert='little_endian')
if (flag /= 0) then
    flag = 1
    write(0, *) "Unable to open file ", filename
    close(unit=50)
    return
end if

read(50, iostat=flag) head
if (flag /= 0) then
    flag = 2
    write(0, *) "Error in reading SAC header ", filename
    close(unit=50)
    return
end if

if ((head%nvhdr>6) .or. (head%nvhdr<0)) then
    close(unit=50)
    open(unit=50, file=filename, status='old', action='read', iostat=flag, &
        & access='stream', form='unformatted', convert='big_endian')
    read(50, iostat=flag) head
    if ((head%nvhdr>6) .or. (head%nvhdr<0)) then
        flag = 6
        write(0, *) filename, " not in sac format ", head%nvhdr
        close(unit=50)
        return
    endif
end if
close(unit=50)
end subroutine sacio_readhead

!
!   sacio_readsac
!
!   Description: Read SAC binary file
!   Input:
!       character(len=80) :: filename   filename to be read
!   Output:
!       type(sachead)     :: head       SAC header to be filled
!       real, dimension(:):: data       SAC data to be filled
!       integer           :: flag       Error code
!
subroutine sacio_readsac(filename, head, data, flag)
implicit none
character(len=*), intent(in) :: filename
real, allocatable, dimension(:), intent(out) :: data  ! Dynamic array, intent(inout) has been changed to intent(out) by fxl
type(sachead), intent(out) :: head
integer, intent(inout) :: flag

! if (allocated(data)) then
!     deallocate(data)
! end if

open(unit=50, file=filename, status='old', action='read', iostat=flag, &
        & access='stream', form='unformatted', convert='little_endian')
if (flag /= 0) then
    flag = 1
    write(0, *) "Unable to open file ", filename
    close(unit=50)
    return
end if

read(50, iostat=flag) head
if (flag /= 0) then
    flag = 2
    write(0, *) "Error in reading SAC header ", filename
    close(unit=50)
    return
end if

if ((head%nvhdr>6) .or. (head%nvhdr<0))then
    close(unit=50)
    open(unit=50, file=filename, status='old', action='read', iostat=flag, &
        & access='stream', form='unformatted', convert='big_endian')
    read(50, iostat=flag) head
    if ((head%nvhdr>6) .or. (head%nvhdr<0)) then
        flag = 6
        write(0, *) filename, " not in sac format ", head%nvhdr
        close(unit=50)
        return
    endif
end if

! WRITE(*,*) head%npts

allocate(data(1:head%npts), stat=flag)
if (flag /= 0)then
    flag = 3
    write(0, *) "Not enough memory for ", filename
    return
end if

read(50, iostat=flag) data
if (flag /= 0) then
   flag = 4
   write(0, *) "Error in reading SAC data", filename
   return
end if
close(unit=50)
end subroutine sacio_readsac

!
!   sacio_writesac
!
!   Description: Write SAC binary file
!   Input:
!       character(len=80) :: filename   filename to be read
!       type(sachead)     :: head       SAC header to be written
!       real, dimension(:):: data       SAC data to be written
!   Output:
!       integer           :: flag       Error code
!
subroutine sacio_writesac(filename, head, data, flag)
implicit none
character(len=*), intent(in) :: filename
real, dimension(:), intent(in) :: data                 !不定结构数组做形参
type(sachead), intent(in) :: head
integer, intent(inout) :: flag

open(unit=50, file=filename, status='replace', action='write', &
    & iostat=flag, access='stream', form='unformatted')
if (flag /= 0) then
    flag = 1
    write(0, *) "Unable to open file ", filename
    close(unit=50)
    return
end if

write(50, iostat=flag) head
if (flag /= 0) then
    flag = 5
    write(0, *) "Error in writing SAC file ", filename
    close(unit=50)
    return
end if

write(50, iostat=flag) data(1:head%npts)      ! data has been changed to data(1:head%npts)
if (flag /= 0) then
   flag = 5
   write(0, *) "Error in writing SAC file ", filename
   close(unit=50)
   return
end if
close(unit=50)
end subroutine sacio_writesac

!
!   sacio_readsac_cut
!
!   Description: Read SAC binary file with cut option
!
!   Input:
!       character(len=80) :: filename   filename to be read
!       integer           :: tmark      time marker in SAC header
!                                            -5 -> b;
!                                            -3 -> o;
!                                            -2 -> a;
!                                            0-9 -> tn;
!       real              :: t0         begin time is tmark + t0
!       real              :: t1         begin time is tmark + t1
!   Output:
!       type(sachead)     :: head       SAC header to be written
!       real, dimension(:):: data       SAC data to be written
!       integer           :: flag       Error code
!
subroutine sacio_readsac_cut(filename, head, data, tmark, t0, t1, flag)
implicit none
character(len=*), intent(in) :: filename
type(sachead), intent(inout) :: head
real, allocatable, dimension(:), intent(inout) :: data
integer, intent(in) :: tmark
real, intent(in) :: t0, t1
integer :: n0, n1
integer, intent(inout) :: flag
real :: tref

open(unit=50, file=filename, status='old', action='read', iostat=flag, &
        & access='stream', form='unformatted', convert='little_endian')
if (flag /= 0) then
    flag = 1
    write(0, *) "Unable to open file ", filename
    close(unit=50)
    return
endif

read(50, iostat=flag) head
if (flag /= 0) then
    flag = 2
    write(0, *) "Error in reading SAC header ", filename
    close(unit=50)
    return
endif

if ((head%nvhdr>6) .or. (head%nvhdr<0))then
    close(unit=50)
    open(unit=50, file=filename, status='old', action='read', iostat=flag, &
        & access='stream', form='unformatted', convert='big_endian')
    read(50, iostat=flag) head
    if ((head%nvhdr>6) .or. (head%nvhdr<0)) then
        flag = 6
        write(0, *) filename, " not in sac format ", head%nvhdr
        close(unit=50)
        return
    endif
end if

select case(tmark)
    case (-5)
        tref = head%b
    case (-3)
        tref = head%o
    case (-2)
        tref = head%a
    case (0)
        tref = head%t0
    case (1)
        tref = head%t1
    case (2)
        tref = head%t2
    case (3)
        tref = head%t3
    case (4)
        tref = head%t4
    case (5)
        tref = head%t5
    case (6)
        tref = head%t6
    case (7)
        tref = head%t7
    case (8)
        tref = head%t8
    case (9)
        tref = head%t9
    case default
        flag = 7
        write(0, *) "illegal time mark ", tmark
        close(unit=50)
        return
end select

if (tref == -12345.) then
    flag = 8
    write(0, *) "Time mark undefined in ", filename
    close(unit=50)
    return
end if

n0 = floor((tref + t0 - head%b) / head%delta)
n1 = floor((tref + t1 - head%b) / head%delta)
if (n0<0 .or. n1>head%npts) then
    flag = 9
    write(8, *) "Cut window outside of time range ", filename
    close(unit=50)
    return
end if
head%npts = n1 - n0 + 1
head%b = t0 + tref
head%e = t1 + tref

allocate(data(1:head%npts), stat=flag)
if (flag /= 0) then
    flag = 3
    write(0, *) "Not enough memory for ", filename
    close(unit=50)
    return
end if

read(50, rec=632+n0*4+1, iostat=flag) data
if (flag /= 0 ) then
    flag = 4
    write(0, *) "Error in reading SAC data", filename
    close(unit=50)
    return
end if

close(unit=50)
end subroutine sacio_readsac_cut

!
!   sacio_nullhead
!
!   Description: Change a SAC header to undefined
!   Input & Output:
!       type(sachead)     :: head       SAC header to be written
!
subroutine sacio_nullhead(head)
implicit none
type(sachead), intent(inout) :: head

head%delta     = SAC_rnull
head%depmin    = SAC_rnull
head%depmax    = SAC_rnull
head%scale     = SAC_rnull
head%odelta    = SAC_rnull

head%b         = SAC_rnull
head%e         = SAC_rnull
head%o         = SAC_rnull
head%a         = SAC_rnull
head%internal1 = SAC_rnull

head%t0        = SAC_rnull
head%t1        = SAC_rnull
head%t2        = SAC_rnull
head%t3        = SAC_rnull
head%t4        = SAC_rnull
head%t5        = SAC_rnull
head%t6        = SAC_rnull
head%t7        = SAC_rnull
head%t8        = SAC_rnull
head%t9        = SAC_rnull
head%f         = SAC_rnull

head%resp0     = SAC_rnull
head%resp1     = SAC_rnull
head%resp2     = SAC_rnull
head%resp3     = SAC_rnull
head%resp4     = SAC_rnull
head%resp5     = SAC_rnull
head%resp6     = SAC_rnull
head%resp7     = SAC_rnull
head%resp8     = SAC_rnull
head%resp9     = SAC_rnull

head%stla      = SAC_rnull
head%stlo      = SAC_rnull
head%stel      = SAC_rnull
head%stdp      = SAC_rnull

head%evla      = SAC_rnull
head%evlo      = SAC_rnull
head%evel      = SAC_rnull
head%evdp      = SAC_rnull
head%mag       = SAC_rnull

head%user0     = SAC_rnull
head%user1     = SAC_rnull
head%user2     = SAC_rnull
head%user3     = SAC_rnull
head%user4     = SAC_rnull
head%user5     = SAC_rnull
head%user6     = SAC_rnull
head%user7     = SAC_rnull
head%user8     = SAC_rnull
head%user9     = SAC_rnull

head%dist      = SAC_rnull
head%az        = SAC_rnull
head%baz       = SAC_rnull
head%gcarc     = SAC_rnull
head%internal2 = SAC_rnull
head%internal3 = SAC_rnull
head%depmen    = SAC_rnull
head%cmpaz     = SAC_rnull
head%cmpinc    = SAC_rnull
head%xminimum  = SAC_rnull
head%xmaximum  = SAC_rnull
head%yminimum  = SAC_rnull
head%ymaximum  = SAC_rnull
head%unused1   = SAC_rnull
head%unused2   = SAC_rnull
head%unused3   = SAC_rnull
head%unused4   = SAC_rnull
head%unused5   = SAC_rnull
head%unused6   = SAC_rnull
head%unused7   = SAC_rnull
head%nzyear    = SAC_inull
head%nzjday    = SAC_inull
head%nzhour    = SAC_inull
head%nzmin     = SAC_inull
head%nzsec     = SAC_inull
head%nzmsec    = SAC_inull
head%nvhdr     = 6
head%norid     = SAC_inull
head%nevid     = SAC_inull
head%npts      = SAC_inull
head%internal4 = SAC_inull
head%nwfid     = SAC_inull
head%nxsize    = SAC_inull
head%nysize    = SAC_inull
head%unused8   = SAC_inull
head%iftype    = SAC_inull
head%idep      = SAC_inull
head%iztype    = SAC_inull
head%unused9   = SAC_inull
head%iinst     = SAC_inull
head%istreg    = SAC_inull
head%ievreg    = SAC_inull
head%ievtyp    = SAC_inull
head%iqual     = SAC_inull
head%isynth    = SAC_inull
head%imagtyp   = SAC_inull
head%imagsrc   = SAC_inull
head%unused10  = SAC_inull
head%unused11  = SAC_inull
head%unused12  = SAC_inull
head%unused13  = SAC_inull
head%unused14  = SAC_inull
head%unused15  = SAC_inull
head%unused16  = SAC_inull
head%unused17  = SAC_inull
head%leven     = SAC_inull
head%lpspol    = SAC_inull
head%lovrok    = SAC_inull
head%lcalda    = SAC_inull
head%unused18  = SAC_inull
head%kstnm     = SAC_cnull
head%kevnm     = SAC_cnull
head%khole     = SAC_cnull
head%ko        = SAC_cnull
head%ka        = SAC_cnull
head%kt0       = SAC_cnull
head%kt1       = SAC_cnull
head%kt2       = SAC_cnull
head%kt3       = SAC_cnull
head%kt4       = SAC_cnull
head%kt5       = SAC_cnull
head%kt6       = SAC_cnull
head%kt7       = SAC_cnull
head%kt8       = SAC_cnull
head%kt9       = SAC_cnull
head%kf        = SAC_cnull
head%kuser0    = SAC_cnull
head%kuser1    = SAC_cnull
head%kuser2    = SAC_cnull
head%kcmpnm    = SAC_cnull
head%knetwk    = SAC_cnull
head%kdatrd    = SAC_cnull
head%kinst     = SAC_cnull
end subroutine sacio_nullhead

!
!   sacio_newhead
!
!   Description: Create a ready-to-use header for evenly-spaced time series SAC file
!   Input:
!       real              :: dt         data sampling interval
!       integer           :: npts       data samples
!       real              :: b0         begin time
!   Output:
!       type(sachead)     :: head       SAC header to be created
!       integer           :: flag       Error code
!
subroutine sacio_newhead(head, dt, npts, b0)
implicit none
type(sachead), intent(out) :: head
real, intent(in) :: dt
integer, intent(in) :: npts
real, intent(in) :: b0

call sacio_nullhead(head)
head%npts  = npts
head%delta = dt
head%b = b0
head%o = 0.
head%e = b0 + (npts-1)*head%delta
head%iztype = 11    ! IO=11
head%iftype = 1     ! ITIME=1
head%leven = 1      ! TRUE=1
end subroutine sacio_newhead

end module sac_io
