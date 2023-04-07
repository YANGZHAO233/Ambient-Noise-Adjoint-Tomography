Welcome to the Ambient Noise Adjoint Tomography python pipeline based on SPECFEM3D_Cartesian(https://geodynamics.org/resources/specfem3dcartesian).

This user-friendly package allows for easy implementation of Ambient Noise Adjoint Tomography and includes modules such as ambient noise raw data processing (ANDP-2.3), initial model, forward computation, preprocessing, measurement, adjoint simulation, pre-conditioner, kernel smoothing, and model updating. The source code of SPECFEM3D_Cartesian is provided in the 'specfem3d' folder, while the 'ANAT' directory contains 01.py-18.py, which is the workflow for Ambient Noise Adjoint Tomography. It is highly recommended to first run 'fwd_test' to determine necessary hyperparameters. 'parameter_files' stores these hyperparameters. Additionally, users will find some useful scripts in the 'scripts' directory.

If you use this python package for your research, kindly cite the following articles:   
1）Zhao, Y., Guo, Z., Wang, K., & Yang, Y. J. (2021). A Large Magma Reservoir Beneath the Tengchong Volcano Revealed by Ambient Noise Adjoint Tomography. Journal of Geophysical Research: Solid Earth, 126(7), e2021JB022116.    
2）Wang, K., Yang, Y., Basini, P., Tong, P., Tape, C., & Liu, Q. (2018). Refined crustal and uppermost mantle structure of southern California by ambient noise adjoint tomography. Geophysical Journal International, 215(2), 844-863.   
3）Fan, X., Guo, Z., Zhao, Y., & Chen, Q. F. (2022). Crust and uppermost mantle magma plumbing system beneath Changbaishan intraplate volcano, China/North Korea, revealed by ambient noise adjoint tomography. Geophysical Research Letters, e2022GL098308.

If you have any questions, please do not hesitate to contact zhaoyanginsane@foxmail.com.

Hope you enjoy using it.


                                                       ANAT Framework
![ANAT_Framework](https://user-images.githubusercontent.com/52820694/170216950-720bee8e-7299-4368-9622-e7651b69f4b0.png)
