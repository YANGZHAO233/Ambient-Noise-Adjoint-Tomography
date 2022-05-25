%% This script is modified from Xiaotao Yang, JGR (2020)
%% Do the clearing
close all;
clear;
clc;
%% Parameter settings
% vmodelfile='M06.nc';
smoothsize=[3, 3, 9]; % larger size value leads to smoother images, [y-axis smoothing, x-axis smoothing, z-axis smoothing]
% smoothsize=[13, 21, 1];
isovalue1=4.29;
isovalue2=3.77;
isovalue3=3.8;
facecolor1 = [1 0.8 0]; % yellow
facecolor2 = [0 0 1];   % bllu
facecolor3 = [1 0 1];   % red
xlim=[100.5, 102.3];
ylim=[25.5, 27.3];
zlim1=[30, 57.5];
zlim2=[0, 35.5];
zlim3=[0, 60];
zlim=[0, 50];
viewpt = [-25, 12];
%% Read netCDF Vs model file
% ncdisp(vmodelfile);
% x=ncread(vmodelfile,'X');
% y=ncread(vmodelfile,'Y');
% z=ncread(vmodelfile,'');
% vs=ncread(vmodelfile,'vs');

[lon,lat,dep,vel]=textread('voxler.dat','%f %f %f %f');
%range from 99-105,24-30,0-80
%dx=dy=0.05,dz=1
xmin=99;xmax=105;ymin=24;ymax=30;zmin=0;zmax=80;
dx=0.05;dy=0.05;dz=1;
x=xmin:dx:xmax;
y=ymin:dy:ymax;
z=zmin:dz:zmax;
% [x,y,z]=meshgrid(xx,yy,zz);
nnx=121;  
nny=121; 
nnz=81; 
vs=zeros(nny,nnx,nnz);
n1=0;
for i=1:nnz
    for j=1:nny
        for m=1:nnx
            n1=n1+1;
            vs(j,m,i)=vel(n1);
        end
    end
end

vs(abs(vs)>10)=nan; % deal with NaN values.
temp=permute(vs,[2,1,3]);
vs2=smooth3(permute(vs,[2,1,3]),'box',smoothsize); % transpose the lat and lon, and do the smoothing
%% Compute the iso-surface and iso-cap
lonidx=find(x>=xlim(1) & x<=xlim(2));
latidx1=find(y>=ylim(1) & y<=ylim(2));
latidx2=find(y>=ylim(1) & y<=ylim(2));
latidx3=find(y>=ylim(1) & y<=ylim(2));
depidx1=find(z>=zlim1(1) & z<=zlim1(2));
depidx2=find(z>=zlim2(1) & z<=zlim2(2));
depidx3=find(z>=zlim3(1) & z<=zlim3(2));
depidxmin1=min(depidx1);
depidxmin2=min(depidx2);
depidxmin3=min(depidx3);
depidxmax1=max(depidx1);
depidxmax2=max(depidx2);
depidxmax3=max(depidx3);
% test=vs2(latidx1,lonidx,depidxmin1:depidxmax1);

isosurf1=isosurface(x(lonidx),y(latidx1),z(depidxmin1:depidxmax1),vs2(latidx1,lonidx,depidxmin1:depidxmax1),isovalue1);
isosurf2=isosurface(x(lonidx),y(latidx2),z(depidxmin2:depidxmax2),vs2(latidx2,lonidx,depidxmin2:depidxmax2),isovalue2);
isosurf3=isosurface(x(lonidx),y(latidx3),z(depidxmin3:depidxmax3),vs2(latidx3,lonidx,depidxmin3:depidxmax3),isovalue3);
%% plot isosurfaces
figure('position',[200 200 1280 720],'color','k');
axis([xlim(1) xlim(2) ylim(1) ylim(2) zlim(1) zlim(2)]);
box on;
ax = gca;
ax.BoxStyle = 'full';
daspect([1 cosd(mean(ylim)) 35]);   % Control data unit length along each axis
xlabel('Longitude','fontsize',16,'color','w');
ylabel('Latitude','fontsize',16,'color','w');
zlabel('Depth (km)','fontsize',16,'color','w');
set(gca,'Fontsize',14,'XTick',xlim(1):0.4:xlim(2),'YTick',ylim(1):0.4:ylim(2),'ZTick',zlim(1):10:zlim(2)); 
ax.XColor='w';
ax.YColor='w';
ax.ZColor='w';
set(gca,'Color','k');
set(gca,'ZDir','reverse');
str=get(gca,'Xticklabel');
strtxt=strcat(str,'?');
set(gca,'Xticklabel',strtxt);
str=get(gca,'Yticklabel');
strtxt=strcat(str,'?');
set(gca,'Yticklabel',strtxt);
% patch(isosurf1,'FaceColor',facecolor1,'EdgeColor','none');
patch(isosurf2,'FaceColor',facecolor2,'EdgeColor','none');
% patch(isosurf3,'FaceColor',facecolor3,'EdgeColor','none');
view(viewpt);
camlight(-45,12);
lighting gouraud;
print(gcf,'plot3d_Vs.pdf','-dpdf', '-fillpage');
set(gcf, 'InvertHardCopy', 'off');  % In order to save the backgroud color
print(gcf,'plot3d_Vs2.pdf','-dpdf', '-fillpage');
