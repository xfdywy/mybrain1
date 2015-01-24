for i=10:20

 figure1 = figure;

% Create axes
axes1 = axes('Parent',figure1);
view(axes1,[-90.5 90]);
grid(axes1,'on');
hold(axes1,'all');
        surf(Huaxi_Nor_TC(:,:,i));
    filename=int2str(i);
    saveas(gcf,filename,'png')
end
    