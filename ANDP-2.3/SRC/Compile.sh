#!/bin/bash

INST_DIR=/usr/local/bin

####################################
echo
echo "Enter into AFTAN..."
cd AFTAN
make cleanall
make
sudo make install
make cleanall
cd ../
####################################

####################################
echo
echo "Enter into AND_Driver..."
cd AND_Driver
make cleanall
make
sudo make install
make cleanall
cd ../
####################################

####################################
echo
echo "Enter into TF_PWS..."
cd TF_PWS
sudo gcc tf-PWS.c S-transform.c sacrw.c -lfftw3 -lm -o $INST_DIR/TF_PWS
cd ../
####################################


####################################
echo
echo "Compiling work FINISHED!"
