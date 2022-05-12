%%%  used for constructing initial checkboard model 
clc;clear;

%%  add anomaly to initial 1D model
%initial vs model source provided by zheng et al., 2016
[lon,lat,dep,vel]=textread('zheng+vs.txt','%f %f %f %f');

%range from 97-108,21-30,1-100
%dx=dy=0.1,dz=1
nnx=111;  
nny=91; 
nnz=100; 
mods_org=zeros(nny,nnx,nnz);
n1=0;
for i=1:nnz
    for j=nny:-1:1
        for m=1:nnx
            n1=n1+1;
            mods_org(j,m,i)=vel(n1);
        end
    end
end

%extend zheng to my model
xx=97:0.1:108;
yy=21:0.1:31;
zz=0:1:100;
[XX,YY,ZZ]=meshgrid(xx,yy,zz);
%extend zmin from 1 to 0
%extend ymax from 30 to 31
nny_ex=nny+10;
mods_temp=taper3d_copy(mods_org,nny_ex,nnx,nnz+1,nny,nnx,nnz,0,0,1);
%objective model meshgrid
dx=0.02;dy=0.02;dz=5;
xmin=98;xmax=106;
ymin=23;ymax=31;
zmin=0;zmax=80;
x=xmin:dx:xmax;
y=ymin:dy:ymax;
z=zmin:dz:zmax;
nx=(xmax-xmin)/dx+1;
ny=(ymax-ymin)/dy+1;
nz=(zmax-zmin)/dz+1;
[X,Y,Z]=meshgrid(x,y,z);
mods_3d=interp3(XX,YY,ZZ,mods_temp,X,Y,Z);

mods=mods_3d; %3d_initial model
temp_1d=mean(mean(mods_3d,1),2);
for i=1:nz
    mods(:,:,i)=temp_1d(:,:,i);%1d initial model
end

modp=mods*1.73;

%vel to den
%Maceira 2009
alph=6.3; %velocity critical value
ksai=0.5; %weghit
ro1=(modp+2.40)/3.125;%Birch 1961
ro2=quadratic_equation(2.64,-7.55,6.86-modp);%Nafe and Drake 1963
phi=pi/2*(1+tanh(ksai*(modp-alph)))/2;
modrho=(cos(phi)).^2.*ro2+(sin(phi)).^2.*ro1;

tempp=modp;
temps=mods;
temprho=modrho;

%input checkboard anomaly
[lon,lat,vel]=textread('perturb_10percent.dat','%f %f %f');
anomaly=zeros(ny,nx);
n1 = 0;
for j=1:ny
    for i=1:nx
            n1=n1+1;
            anomaly(j,i)=vel(n1);
    end
end
for k=1:nz
    mods(:,:,k) = mods(:,:,k).*(1+anomaly);
end

ap=0;
data=zeros(ny*nx*nz,6);
for k=nz:-1:1
    for i=1:ny
        for j=1:nx
            ap=ap+1;
            data(ap,1)=X(i,j,k);
            data(ap,2)=Y(i,j,k);
            data(ap,3)=Z(i,j,k)*(-1000);
            data(ap,4)=modp(i,j,k)*1000;
            data(ap,5)=mods(i,j,k)*1000;
            data(ap,6)=modrho(i,j,k)*1000;
        end
    end
end

fid=fopen('3d_model_geo.dat','wt');
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

%% grd output for horizontal and vertical direction
tempss=(mods-temps)./temps;
tempsss=tempss(:,:,11);
gmin=min(min(tempsss));
gmax=max(max(tempsss));
fp=fopen('z50_checkboard_anomaly_10percent.grd','wt');
fprintf(fp,'DSAA\n');
fprintf(fp,'%d %d\n',nx,ny);
fprintf(fp,'%g %g\n',xmin,xmax);
fprintf(fp,'%g %g\n',ymin,ymax);
fprintf(fp,'%g %g\n',gmin,gmax);
for i=1:ny
    for j=1:nx
        fprintf(fp,'%g ',tempsss(i,j));
    end
    fprintf(fp,'\n');
end
fclose(fp);

modsss=mods(:,:,11);
gmin=min(min(modsss));
gmax=max(max(modsss));
xmin=x(1);xmax=x(end);
ymin=y(1);ymax=y(end);
fp=fopen('z50_checkboard_anomaly.grd','wt');
fprintf(fp,'DSAA\n');
fprintf(fp,'%d %d\n',nx,ny);
fprintf(fp,'%g %g\n',xmin,xmax);
fprintf(fp,'%g %g\n',ymin,ymax);
fprintf(fp,'%g %g\n',gmin,gmax);
for i=1:ny
    for j=1:nx
        fprintf(fp,'%g ',modsss(i,j));
    end
    fprintf(fp,'\n');
end
fclose(fp);

ap=0;
data=zeros(1*nx*nz,6);
i=176;
for k=nz:-1:1
%     for i=1:ny
        for j=1:nx
            ap=ap+1;
            data(ap,1)=X(i,j,k);
            data(ap,2)=Y(i,j,k);
            data(ap,3)=Z(i,j,k)*(-1);
            data(ap,4)=modp(i,j,k)*1000;
            data(ap,5)=tempss(i,j,k);
            data(ap,6)=modrho(i,j,k)*1000;
        end
%     end
end

fid=fopen('26.5_10percent_geo.dat','wt');
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