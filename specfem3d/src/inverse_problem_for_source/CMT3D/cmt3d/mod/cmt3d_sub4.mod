GFORTRAN module version '0' created from cmt3d_sub4.f90 on Mon Sep 21 15:59:12 2020
MD5:caabcdb5a124f16e2f52f8fcdcdba754 -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () ()
() () () () ())

()

()

()

()

(2 '__convert_r4_r8' '(intrinsic)' '__convert_r4_r8' 1 ((PROCEDURE
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN FUNCTION ELEMENTAL PURE) (
REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
3 'calc_rot_matrix' 'cmt3d_sub4' 'calc_rot_matrix' 1 ((PROCEDURE
UNKNOWN-INTENT MODULE-PROC DECL UNKNOWN SUBROUTINE) (UNKNOWN 0 0 0
UNKNOWN ()) 4 0 (5 6 7) () 0 () () () 0 0)
8 'cmt3d_constants' 'cmt3d_constants' 'cmt3d_constants' 1 ((MODULE
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN) (UNKNOWN 0 0 0 UNKNOWN ())
0 0 () () 0 () () () 0 0)
9 'cmt3d_sub4' 'cmt3d_sub4' 'cmt3d_sub4' 1 ((MODULE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN) (UNKNOWN 0 0 0 UNKNOWN ()) 0 0 () () 0 ()
() () 0 0)
10 'debug' 'cmt3d_constants' 'debug' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (LOGICAL 4 0 0 LOGICAL ()) 0 0 () (
CONSTANT (LOGICAL 4 0 0 LOGICAL ()) 0 1) () 0 () () () 0 0)
11 'eps2' 'cmt3d_constants' 'eps2' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ()) 0 0 () (
CONSTANT (REAL 8 0 0 REAL ()) 0 '0.28f5c28f5c28f6@-1') () 0 () () () 0 0)
12 'eps5' 'cmt3d_constants' 'eps5' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ()) 0 0 () (
CONSTANT (REAL 8 0 0 REAL ()) 0 '0.a7c5ac471b4788@-4') () 0 () () () 0 0)
13 'huge' '(intrinsic)' 'huge' 1 ((PROCEDURE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN FUNCTION) (UNKNOWN 0 0 0 UNKNOWN ()) 0 0 () () 13 () ()
() 0 0)
14 'ioinv' 'cmt3d_constants' 'ioinv' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '30') () 0 () () () 0 0)
15 'iopar' 'cmt3d_constants' 'iopar' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '40') () 0 () () () 0 0)
16 'iowin' 'cmt3d_constants' 'iowin' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '35') () 0 () () () 0 0)
17 'ndatamax' 'cmt3d_constants' 'ndatamax' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '30000') () 0 () () () 0 0)
18 'nm' 'cmt3d_constants' 'nm' 1 ((PARAMETER UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (CONSTANT (
INTEGER 4 0 0 INTEGER ()) 0 '6') () 0 () () () 0 0)
19 'nml' 'cmt3d_constants' 'nml' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '9') () 0 () () () 0 0)
20 'nparmax' 'cmt3d_constants' 'nparmax' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '11') () 0 () () () 0 0)
21 'nrecmax' 'cmt3d_constants' 'nrecmax' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1200') () 0 () () () 0 0)
22 'nregions' 'cmt3d_constants' 'nregions' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '10') () 0 () () () 0 0)
23 'nwinmax' 'cmt3d_constants' 'nwinmax' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1800') () 0 () () () 0 0)
24 'pi' 'cmt3d_constants' 'pi' 1 ((PARAMETER UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ()) 0 0 () (CONSTANT (REAL 8 0 0
REAL ()) 0 '0.3243f6a8885a30@1') () 0 () () () 0 0)
25 'r_earth' 'cmt3d_constants' 'r_earth' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (INTEGER 4 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '6371') () 0 () () () 0 0)
26 'ref_dist' 'cmt3d_constants' 'ref_dist' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 4 0 0 REAL ()) 0 0 () (
CONSTANT (REAL 4 0 0 REAL ()) 0 '0.6400000@2') () 0 () () () 0 0)
27 'rotate_cmt' 'cmt3d_sub4' 'rotate_cmt' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN SUBROUTINE) (UNKNOWN 0 0 0 UNKNOWN ()) 28 0 (
29 30 31 32 33) () 0 () () () 0 0)
34 'scale_ctime' 'cmt3d_constants' 'scale_ctime' 1 ((PARAMETER
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ())
0 0 () (CONSTANT (REAL 8 0 0 REAL ()) 0 '0.10000000000000@1') () 0 () ()
() 0 0)
35 'scale_delta' 'cmt3d_constants' 'scale_delta' 1 ((PARAMETER
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ())
0 0 () (CONSTANT (REAL 8 0 0 REAL ()) 0 '0.41893780000000@-2') () 0 () ()
() 0 0)
36 'scale_depth' 'cmt3d_constants' 'scale_depth' 1 ((PARAMETER
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ())
0 0 () (CONSTANT (REAL 8 0 0 REAL ()) 0 '0.10000000000000@1') () 0 () ()
() 0 0)
37 'scale_hdur' 'cmt3d_constants' 'scale_hdur' 1 ((PARAMETER
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ())
0 0 () (CONSTANT (REAL 8 0 0 REAL ()) 0 '0.10000000000000@1') () 0 () ()
() 0 0)
38 'scale_moment' 'cmt3d_constants' 'scale_moment' 1 ((PARAMETER
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 8 0 0 REAL ())
0 0 () (CONSTANT (REAL 8 0 0 REAL ()) 0 '0.21e19e00000000@19') () 0 () ()
() 0 0)
39 'small' 'cmt3d_constants' 'small' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (REAL 4 0 0 REAL ()) 0 0 () (
CONSTANT (REAL 4 0 0 REAL ()) 0 '-0.ffffff0@32') () 0 () () () 0 0)
29 'cmt_par' '' 'cmt_par' 28 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN
UNKNOWN DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 EXPLICIT (
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0 0
INTEGER ()) 0 '11')) 0 () () () 0 0)
30 'npar' '' 'npar' 28 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN DUMMY)
(INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
31 'elat0' '' 'elat0' 28 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN
DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
32 'elon0' '' 'elon0' 28 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN
DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
33 'direction' '' 'direction' 28 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN DUMMY) (INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
5 'elon' '' 'elon' 4 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN DUMMY) (
REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
6 'elat' '' 'elat' 4 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN DUMMY) (
REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
7 'rmat' '' 'rmat' 4 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN
DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (2 EXPLICIT (CONSTANT (
INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0
'3') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0
0 INTEGER ()) 0 '3')) 0 () () () 0 0)
)

('__convert_r4_r8' 0 2 'calc_rot_matrix' 0 3 'cmt3d_constants' 0 8
'cmt3d_sub4' 0 9 'debug' 0 10 'eps2' 0 11 'eps5' 0 12 'huge' 0 13 'ioinv'
0 14 'iopar' 0 15 'iowin' 0 16 'ndatamax' 0 17 'nm' 0 18 'nml' 0 19
'nparmax' 0 20 'nrecmax' 0 21 'nregions' 0 22 'nwinmax' 0 23 'pi' 0 24
'r_earth' 0 25 'ref_dist' 0 26 'rotate_cmt' 0 27 'scale_ctime' 0 34
'scale_delta' 0 35 'scale_depth' 0 36 'scale_hdur' 0 37 'scale_moment' 0
38 'small' 0 39)
