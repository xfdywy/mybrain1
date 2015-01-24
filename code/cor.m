clc
clear
load  Huaxi_Nor_TC
[time,naoqu,people]=size(Huaxi_Nor_TC);
ncorr=ones(naoqu,naoqu,people)*(-1);
for p=1:people
    
    sam=  Huaxi_Nor_TC(:,:,p);  
    ncorr(:,:,p)=corrcoef(sam);    
    
end
save ncorr
clc
clear
load('Huaxi_Sch_TC.mat')
[time,naoqu,people]=size(Huaxi_Sch_TC);
pcorr=ones(naoqu,naoqu,people)*(-1);
for p=1:people
        sam=  Huaxi_Sch_TC(:,:,p);  
    pcorr(:,:,p)=corrcoef(sam);    
    
end
save pcorr