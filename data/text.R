
A<-readMat("Huaxi_Sch_TC.mat")
A<-A[[1]]
b1<-A[,1,14]
b2<-A[,1,17]
p<-ggplot()
q1<-geom_line(aes(x=1:length(b1),y=b1),colour="red")
q2<-geom_line(aes(x=1:length(b2),y=b2))
p+q1+q2
 

pdf("myp.pdf")
for(i in 1:90){
  b1<-A[,i,1]
  p<-ggplot()
  q1<-geom_line(aes(x=1:length(b1),y=b1),colour="red") 
  print(p+q1)
}
dev.off()

