Introduction
==============================

This is python version of `cmt3d <https://github.com/QuLogic/GRD_CMT3D>`_ package(originally developed by Qinya Liu and Carl Tape), which is used to provide more precise source solution in 3D earth model.

This pycmt3d uses the perturbed method to calculate the gradient of the source. Currently, 11 parameters inversion are supported: **[Mrr, Mtt, Mpp, Mrt, Mrp, Mtp, depth, longitude, latitude, time shift, half duration]**. The paper is published on `here <http://www.bssaonline.org/content/94/5/1748.abstract>`_. If you want to know the basic theory of this method, please read the paper. 

The pacakge runs currently on one core(no parallel support) because most of the time is spend on I/O but not calculation.

New features
###############################
1. Bootstrap statistical analysis.

  Bootstrap method is added in the inversion. So measurements will be randomly selected to ensemble the inversion. Thus we can evaluate the statistic feature of the source inversion, like mean value and standard deviation.

2. Seperation of Data and inversion shema.

  Data and invesion configuration are seperated now. So once data is loaded in, one can try different inversion shema without reloading the data

3. Reduce I/O traffic

  All data are read in the memory once and kept there till the end, which greatly reduce the I/O traffic. Therefore, memory issue should be taken care of when doing the inversion. You don't want memory overflow. However, for a modern cluster, this should not be a problem since it usually has 64GB memory on one node.

4. ASDF supported

  ASDF data I/O supported while also keep supports for the SAC data.

5. Plotting utils

  Add plotting methods to visulize the result.

Potential difference
####################
1. Cross-correlation measurements

   The cross-correlation measurements is inheritated from `pyflex <https://github.com/krischer/pyflex>`_ to make the measurements in the workflow consistent. It utilizes the numpy.correlate method.

2. The linear solver

  The linear solver in pycmt3d utilized numpy.linalg method.
