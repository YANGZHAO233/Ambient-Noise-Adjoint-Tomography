%used for constructing initial model for adjoint tomography
%construct utm grid
clc;clear;

%transfer 01_simulation_region_geo.txt to 01_simulation_region_utm.csv using convert_lonlat2utm_3.pl
%x_origin_min=-164629.868  x_origin_max=551050.534
%y_origin_min=2598960.198  y_origin_max=3392215.956
%rounding decimals
xmin=-170000;xmax=550000;dx=10000;
ymin=2600000;ymax=3390000;dy=10000;

%number of grid points in each direction
nx=(xmax-xmin)/dx+1;
ny=(ymax-ymin)/dy+1;
nz=81;

%coordinates
x=xmin:dx:xmax;
y=ymin:dy:ymax;
z=-80000:1000:0;

%construct meshgrid
[X,Y,Z]=meshgrid(x,y,z);
data=zeros(ny*nx*nz,3);
ap=0;
for k=1:nz
    for i=1:ny
        for j=1:nx
            ap=ap+1;
            data(ap,1)=X(i,j,k);
            data(ap,2)=Y(i,j,k);
            data(ap,3)=Z(i,j,k);
        end
    end
end

%save to dat
fid=fopen('02_coordinates_utm.dat','wt');
[m,n]=size(data);
for i=1:1:m
    for j=1:1:n
        if j==n
            fprintf(fid,'%.2f\n',data(i,j));
        else
            fprintf(fid,'%.2f\t',data(i,j));
        end
    end
end
fclose(fid);

%use GlobalMapper or convert_utm2lonlat_3.pl to transfer the utm file to geo
