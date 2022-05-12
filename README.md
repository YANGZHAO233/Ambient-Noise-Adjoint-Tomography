This package is a python pipeline for Ambient Noise Adjoint Tomography based on SPECFEM3D_Cartesian.
The modules include ambient noise raw data processing (ANDP-2.3), initial model, forward computation, preprocessing, measurement, adjoint simulation, pre-conditioner, kernel smoothing and model updating.

The source code of SPECFEM3D_Cartesian is placed in 'specfem3d'.
01.py-18.py in 'ANAT' is the workflow for Ambient Noise Adjoint Tomography.
'fwd_test' is strongly recommended to run first to determine some hyperparameters.
'parameter_files' is a hyperparameter directory for SPECFEM3D_Cartesian.
There are some useful scripts for users in 'scripts'.

If you use this python package for your own research, please cite the following articles:

1）Zhao, Y., Guo, Z., Wang, K., & Yang, Y. J. (2021). A Large Magma Reservoir Beneath the Tengchong Volcano Revealed by Ambient Noise Adjoint Tomography. Journal of Geophysical Research: Solid Earth, 126(7), e2021JB022116.

2）Wang, K., Yang, Y., Basini, P., Tong, P., Tape, C., & Liu, Q. (2018). Refined crustal and uppermost mantle structure of southern California by ambient noise adjoint tomography. Geophysical Journal International, 215(2), 844-863.

3）Fan, X.L., Guo, Z., Zhao, Y., Chen, Q.F. (2022). Crust and uppermost mantle magma plumbing system beneath Changbaishan intraplate volcano, China/North Korea, revealed by ambient noise adjoint tomography. Geophysical Research Letters, under review.

If you have any questions, please contact zhaoyanginsane@foxmail.com.

Hope you enjoy using it.
