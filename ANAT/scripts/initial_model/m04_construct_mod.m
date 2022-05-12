clear;clear;
m_coord=load('02_coordinates_geo.txt','r');
%data list
x=m_coord(:,1);y=m_coord(:,2);z=m_coord(:,3);

%nx,ny,nz are calculated in m01_utm_coordinates.m
nx=73;ny=80;nz=81;

%reshape to 3D grid
X=reshape(x,[nx,ny,nz]);X=permute(X,[2 1 3]);
Y=reshape(y,[nx,ny,nz]);Y=permute(Y,[2 1 3]);
Z=reshape(z,[nx,ny,nz]);Z=permute(Z,[2 1 3]);

%initial vs model source provided by zheng et al., 2016
[lon,lat,dep,vel]=textread('zheng+vs.txt','%f %f %f %f');

%construct initial 3D vs model
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

%construct the objective vs model
xx=97:0.1:108;
yy=21:0.1:31;
zz=0:1000:100000;
[XX,YY,ZZ]=meshgrid(xx,yy,zz);
ZZ=-ZZ;
%extend zmin from 1000 to 0
%extend ymax from 30 to 31
nny_ex=nny+10;
mods_temp=taper3d_copy(mods_org,nny_ex,nnx,nnz+1,nny,nnx,nnz,0,0,1);

%object model
mods=interp3(XX,YY,ZZ,mods_temp,X,Y,Z);
modp=mods*1.73;

%velocity to density
%Maceira 2009
alph=6.3; %velocity critical value
ksai=0.5; %weghit
ro1=(modp+2.40)/3.125;%Birch 1961
ro2=quadratic_equation(2.64,-7.55,6.86-modp);%Nafe and Drake 1963
phi=pi/2*(1+tanh(ksai*(modp-alph)))/2;
modrho=(cos(phi)).^2.*ro2+(sin(phi)).^2.*ro1;


%save to utm file
m_coord_utm=load('02_coordinates_utm.dat','r');
data=zeros(ny*nx*nz,6);
X_utm=reshape(m_coord_utm(:,1),[nx,ny,nz]);X_utm=permute(X_utm,[2 1 3]);
Y_utm=reshape(m_coord_utm(:,2),[nx,ny,nz]);Y_utm=permute(Y_utm,[2 1 3]);
Z_utm=reshape(m_coord_utm(:,3),[nx,ny,nz]);Z_utm=permute(Z_utm,[2 1 3]);

ap=0;
for k=1:nz
    for i=1:ny
        for j=1:nx
            ap=ap+1;
            data(ap,1)=X_utm(i,j,k);
            data(ap,2)=Y_utm(i,j,k);
            data(ap,3)=Z_utm(i,j,k);
            data(ap,4)=modp(i,j,k)*1000;
            data(ap,5)=mods(i,j,k)*1000;
            data(ap,6)=modrho(i,j,k)*1000;
        end
    end
end

fid=fopen('tomography_model.xyz','wt');
[m,n]=size(data);
for i=1:1:m
    for j=1:1:n
        if j==n
            fprintf(fid,'%.0f\n',data(i,j));
        else
            fprintf(fid,'%.0f\t',data(i,j));
        end
    end
end
fclose(fid);


%2d grid validation for geo
ap=0;
data=zeros(ny*nx*1,6);
for k=61
    for i=1:ny
        for j=1:nx
            ap=ap+1;
            data(ap,1)=X(i,j,k);
            data(ap,2)=Y(i,j,k);
            data(ap,3)=Z(i,j,k);
            data(ap,4)=modp(i,j,k)*1000;
            data(ap,5)=mods(i,j,k)*1000;
            data(ap,6)=modrho(i,j,k)*1000;
        end
    end
end

fid=fopen('validation/20km_model_geo.dat','wt');
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


%% 2d grid validation for utm
% ap=0;
% data=zeros(ny*nx*1,6);
% for k=71
%     for i=1:ny
%         for j=1:nx
%             ap=ap+1;
%             data(ap,1)=X_utm(i,j,k);
%             data(ap,2)=Y_utm(i,j,k);
%             data(ap,3)=Z_utm(i,j,k);
%             data(ap,4)=modp(i,j,k)*1000;
%             data(ap,5)=mods(i,j,k)*1000;
%             data(ap,6)=modrho(i,j,k)*1000;
%         end
%     end
% end
% 
% fid=fopen('validation/10km_model_utm.dat','wt');
% [m,n]=size(data);
% for i=1:1:m
%     for j=1:1:n
%         if j==n
%             fprintf(fid,'%.2f\n',data(i,j));
%         else
%             fprintf(fid,'%.2f\t',data(i,j));
%         end
%     end
% end
% fclose(fid);
