% 3D 扩编(XY方向复制，Z方向充零)
% 作者：崔亚彤，1023955850@qq.com，2016年3月1日19:22:43
% 中国地质大学(北京)地球物理与信息技术学院

%**************************************************************************
function gt=taper3d_copy(g,nny,nnx,nnz,ny,nx,nz,nyi,nxi,nzi) %nny nnx nnz extend point
%ny nx nz oringinal point
%nyi nxi nzi left add point
gt=zeros(nny,nnx,nnz); 
gt((nyi+1):(nyi+ny),(nxi+1):(nxi+nx),(nzi+1):(nzi+nz))=g;
for m=1:nyi
    gt(m,:,:)=gt(1+nyi,:,:);
end
for h=(nyi+ny+1):nny
    gt(h,:,:)=gt(ny+nyi,:,:);
end
for e=1:nxi
    gt(:,e,:)=gt(:,1+nxi,:);
end
for f=(nxi+nx+1):nnx
    gt(:,f,:)=gt(:,nxi+nx,:);
end
for j=1:nzi
    gt(:,:,j)=gt(:,:,1+nzi);
end
for k=(nzi+nz+1):nnz
    gt(:,:,k)=gt(:,:,nz+nzi);
end