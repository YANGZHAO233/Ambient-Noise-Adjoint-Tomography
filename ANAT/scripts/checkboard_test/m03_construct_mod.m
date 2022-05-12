clear;clear;
m_coord=load('02_coordinates_geo.dat','r');
x=m_coord(:,1);y=m_coord(:,2);z=m_coord(:,3);
nx=73;ny=80;nz=81;
X=reshape(x,[nx,ny,nz]);X=permute(X,[2 1 3]);
Y=reshape(y,[nx,ny,nz]);Y=permute(Y,[2 1 3]);
Z=reshape(z,[nx,ny,nz]);Z=permute(Z,[2 1 3]);

[lon,lat,dep,vp,vs,rho]=textread('3d_model_geo.dat','%f %f %f %f %f %f');

%original vs model provided by perturb model
nnx=401;  
nny=401; 
nnz=17; 
modp_org=zeros(nny,nnx,nnz);
mods_org=zeros(nny,nnx,nnz);
modrho_org=zeros(nny,nnx,nnz);
n1=0;
for i=1:nnz
    for j=1:nny
        for m=1:nnx
            n1=n1+1;
            modp_org(j,m,i)=vp(n1);
            mods_org(j,m,i)=vs(n1);
            modrho_org(j,m,i)=rho(n1);
        end
    end
end

%construct the objective vs model
xx=98:0.02:106;
yy=23:0.02:31;
zz=-80000:5000:0;
[XX,YY,ZZ]=meshgrid(xx,yy,zz);

modp=interp3(XX,YY,ZZ,modp_org,X,Y,Z);
mods=interp3(XX,YY,ZZ,mods_org,X,Y,Z);
modrho=interp3(XX,YY,ZZ,modrho_org,X,Y,Z);

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
            data(ap,4)=modp(i,j,k);
            data(ap,5)=mods(i,j,k);
            data(ap,6)=modrho(i,j,k);
        end
    end
end

fid=fopen('tomography_model.xyz_10percent','wt');
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


%% 2d grid validation for geo
% ap=0;
% data=zeros(ny*nx*1,6);
% for k=36
%     for i=1:ny
%         for j=1:nx
%             ap=ap+1;
%             data(ap,1)=X(i,j,k);
%             data(ap,2)=Y(i,j,k);
%             data(ap,3)=Z(i,j,k);
%             data(ap,4)=modp(i,j,k)*1000;
%             data(ap,5)=mods(i,j,k)*1000;
%             data(ap,6)=modrho(i,j,k)*1000;
%         end
%     end
% end
% 
% fid=fopen('validation/45km_model_geo.dat','wt');
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


%% 2d grid validation for utm
% ap=0;
% data=zeros(ny*nx*1,6);
% for k=11
%     for i=1:ny
%         for j=1:nx
%             ap=ap+1;
%             data(ap,1)=X_utm(i,j,k);
%             data(ap,2)=Y_utm(i,j,k);
%             data(ap,3)=Z_utm(i,j,k);
%             data(ap,4)=modp(i,j,k);
%             data(ap,5)=mods(i,j,k);
%             data(ap,6)=modrho(i,j,k);
%         end
%     end
% end
% 
% fid=fopen('10km_model_utm_10percent.dat','wt');
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
