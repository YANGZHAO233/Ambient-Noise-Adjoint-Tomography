FC=ifort
FLAGS= -O3

all: utm2sph sph2utm
utm2sph:utm2sph.f90 utm_geo.o
	$(FC) $(FLAGS) utm2sph.f90 utm_geo.o -o utm2sph 
sph2utm:sph2utm.f90 utm_geo.o
	$(FC) $(FLAGS) sph2utm.f90 utm_geo.o -o sph2utm 
utm_geo.o: utm_geo.f90
	$(FC) $(FLAGS) $< -c
