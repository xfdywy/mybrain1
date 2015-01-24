#########头文件####
setwd("../data")
getwd()
library(R.matlab)
library(corrplot)
library(ggplot2)
library(reshape2)
library(e1071)
library(adabag)
library(randomForest) 
library(nnet)
########从mat文件中调入数据#######
ncor_mat<-readMat("ncorr.mat")
names(ncor_mat)
ncor<-ncor_mat$ncorr
dim(ncor)


pcor_mat<-readMat("pcorr.mat")
names(pcor_mat)
pcor<-pcor_mat$pcorr
dim(pcor)

###########做z变换#########
ncorz <- 0.5*log((1+ ncor)/(1-ncor)) 
pcorz <- 0.5*log((1+ pcor)/(1-pcor)) 
qqnorm(ncorz[a<-sample(90,1),b<-sample(90,1),])
qqline(ncorz[a,b,])                              ##检验分布的正态性
shapiro.test(pcorz[a,b,])
######画相关系数图到pdf中####

pdf("nresult.pdf")
for(i in 1:ncor_mat$people){
  corrplot(ncor[,,i])}
dev.off()


pdf("presult.pdf")
for(i in 1:pcor_mat$people){
  corrplot(pcor[,,i])}
dev.off()

#####画相关系数图到png中####
for(i in 1:pcor_mat$people){
  name<-paste('./pplot/',i,'myp.png')
  png(name)
  corrplot(pcor[,,i])
  dev.off()}

for(i in 1:2){
  name<-paste('./nplot/',i,'myn.png')
  png(name)
  corrplot(pcor[,,i])
  dev.off()}

############做相关性分析########
# list1<-matrix(0,nrow=100,ncol=90)
# for (i in 1:100){
#   list1[i,which(abs(ncor[1,,i])>0.6)]<-1 
# }
# sumlist<-(apply(list1,2,sum))
# sumlist
#########画箱线图#####
for (i in 2:90){
  print(i)
  totest<-list(a<-ncorz[1,i,],b<-pcorz[1,i,])
  totest1<-melt(totest,value.name="corr")
  names(totest1)<-c("corr","group")
  summary(totest1)
  
  
  name<-paste('./nplot1/',i,'box.png')
  png(name)
  p<-ggplot(totest1)
  q1<-geom_boxplot(aes(x=as.factor(group),y=corr,
                       colour=as.factor(group)),
                   notch=T,outlier.colour="red")
  q2<-geom_point(aes(x=as.factor(group),y=corr))
  q3<-ggtitle(name)
  q4<-ylim(-1.5,1.5)
  print(p+q1+q2+q3+q4)
  dev.off()
}

#####做检验#####
pvalue_w<-matrix(NA,nrow=90,ncol=90)
pvalue_t<-matrix(NA,nrow=90,ncol=90)
for(j in 1:dim(ncorz)[1]){
  for (i in 1:(dim(ncorz)[2])){
    if(i==j)
      pvalue_t[j,i] <- -1
    else{
      totest<-list(a<-ncorz[j,i,],b<-pcorz[j,i,])
      
      print(i)
      tresult<-  t.test(x=totest[[1]],totest[[2]],var.equal=F)
      print(i)
      #wresult<-wilcox.test(totest[[1]],totest[[2]],paired=F,exact=T)
      #  pvalue_w[i,1] <- wresult$p.value
      pvalue_t[j,i] <- tresult$p.value
    }
  }
}
pvalue_t->pvalue_result
pset<-5*10^-6
pvalue_t[pvalue_t>pset]<-1
pvalue_t <- abs(pvalue_t-1)
pvalue_t[pvalue_t>pset]<-1
corrplot(pvalue_t,is.corr=F)
number_p<-(length(which(pvalue_result<pset))-90)/2###通过检验的p值个数


pvalue_result->pvalue_test
res<-which(pvalue_test<pset,T)
res<-data.frame(res)
res<-res[res$col>res$row,]
res
fix(pvalue_test)




totest<-list(a<-ncorz[15,67,],b<-pcorz[15,67,])
totest1<-melt(totest,value.name="corr")
names(totest1)<-c("corr","group")
p<-ggplot(totest1)
q1<-geom_boxplot(aes(x=as.factor(group),y=corr,
                     colour=as.factor(group)),
                 notch=T,outlier.colour="red")
q2<-geom_point(aes(x=jitter(as.numeric(group),amount=0.02),y=corr))
p+q1+q2


###########把每个人的特征抽出来######
####病人##
df<-data.frame()
pcor_f<-melt(pcor,varnames=c('naoqux','naoquy','people'),value.name='cor')
head(pcor_f)
for(i in 1:dim(res)[1]){
  temp<-pcor_f[pcor_f$naoqux==res[i,1]&pcor_f$naoquy==res[i,2],]
  head(temp)
  df<-rbind(df,temp)  
}
pcor_f<-df
pcor_f$naoqux<-as.factor(pcor_f$naoqux)
pcor_f$naoquy<-as.factor(pcor_f$naoquy)
pcor_f<-dcast(pcor_f,people ~ naoqux + naoquy,value.var='cor')
pcor_f$label<-0
####正常人###
df<-data.frame()
ncor_f<-melt(ncor,varnames=c('naoqux','naoquy','people'),value.name='cor')
head(ncor_f)
for(i in 1:dim(res)[1]){
  
  temp<-ncor_f[ncor_f$naoqux==res[i,1]&ncor_f$naoquy==res[i,2],]
  head(temp)
  df<-rbind(df,temp)  
}
ncor_f<-df
ncor_f$naoqux<-as.factor(ncor_f$naoqux)
ncor_f$naoquy<-as.factor(ncor_f$naoquy)
ncor_f<-dcast(ncor_f,people ~ naoqux + naoquy,value.var='cor')
ncor_f$label<-1
####做成一个矩阵###
cor_f<-rbind(pcor_f,ncor_f)
names(cor_f)->name
names(cor_f)<-c('people','x1','x2','x3','x4','x5','x6','x7','x8','x9',
                'x10','x11','x12','x13','x14','label')
cor_f$label<-as.factor(cor_f$label)
sr=matrix(NA,nrow=10,ncol=length(seq(0.01,3,0.02)))

for (i in 1:10){
  k=1
  for( j in seq(0.01,3,0.02)){
mean=0
totrain<-c(sample(1:dim(pcor_f)[1],0.8*dim(pcor_f)[1],replace=F),
           sample((dim(pcor_f)[1]+1):(dim(pcor_f)[1]+dim(ncor_f)[1]),
                  0.8*dim(ncor_f)[1],replace=F))
totest<-setdiff(1:dim(cor_f)[1] , totrain)
#totest<-sample(1:dim(cor_f)[1],0.85*dim(cor_f)[1],replace=F)
cor_f_train<- cor_f[totrain,]
cor_f_test<- cor_f[totest,]
#########分类算法###########
###svm###
svm.fit<-svm(label ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x1+x14,
             data=cor_f_train,
             kernel='radial',gamma=0.31,cost=40)
#svm.predictions <- ifelse(predict(svm.fit,newdata=cor_f_test) > 0.5, 1, 0)
svm.predictions<-predict(svm.fit,newdata=cor_f_test)
cor_f_test$sp<-svm.predictions
n<-dim(cor_f_test)[1]
nn<-dim((cor_f_test[cor_f_test$sp==cor_f_test$label,]))[1]
nn/n
mean(with(cor_f_test, svm.predictions == label))
cor_f_test$sp<-predict(svm.fit,newdata=cor_f_test)
plot(cor_f_test$label,cor_f_test$sp)
k=1+k
}

}


sr

###adaboosting###


a <- boosting(label ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x1+x14,data=cor_f_train)
pre = predict.boosting(a, cor_f_test)
pre$error

l<-glm(label ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14,data=cor_f_train,family=quasi)
summary(l)
y.res<-resid(l)
y.fit<-predict(l)
cor_f_train$lp<-ifelse(y.fit>0.5,1,0)

mean(with(cor_f_train, lp == label))


plot(cor_f_train$lp,jitter(cor_f_train$label))

plot(y.fit,abs(y.res))
plot(cor_f_train$x1,y.res)


#####随机森林###
model.forest <- randomForest(label ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14,data=cor_f_train)
pre.forest = predict(model.forest, cor_f_test)

plot(pre.forest, cor_f_test$label)
mean(with(cor_f_test, pre.forest == label))

  ######神经网络###
totrain<-c(sample(1:dim(pcor_f)[1],0.9*dim(pcor_f)[1],replace=F),
           sample((dim(pcor_f)[1]+1):(dim(pcor_f)[1]+dim(ncor_f)[1]),
                  0.9*dim(ncor_f)[1],replace=F))
totest<-setdiff(1:dim(cor_f)[1] , totrain)
#totest<-sample(1:dim(cor_f)[1],0.85*dim(cor_f)[1],replace=F)
cor_f_train<- cor_f[totrain,]
cor_f_test<- cor_f[totest,]
nnet.fit<-nnet(label ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x1+x14,
               data=cor_f_train,size=20,maxit=20)
nnet.predictions<-predict(nnet.fit,newdata=cor_f_test,type="class")
table(nnet.predictions,cor_f_test$label)
mean(with(cor_f_test, nnet.predictions == label))
cor_f_test$sp<-nnet.predictions
n<-dim(cor_f_test)[1]
nn<-dim((cor_f_test[cor_f_test$sp==cor_f_test$label,]))[1]
nn/n

######分析结果#####
pp<-predict(svm.fit,cor_f_train[cor_f_train$label==0,])
qq<-predict(svm.fit,cor_f_train[cor_f_train$label==1,])
aa<-ggplot()
aa1<-geom_point(aes(y=pp,x=1:length(pp)),colour='red')
aa2<-geom_point(aes(y=as.numeric(qq)+0.1,x=1:length(qq)))
aa+aa1+aa2


pp1<-predict(svm.fit,cor_f_test[cor_f_test$label==0,])
qq1<-predict(svm.fit,cor_f_test[cor_f_test$label==1,])
aa<-ggplot()
aa1<-geom_point(aes(y=pp1,x=1:length(pp1)),colour='red')
aa2<-geom_point(aes(y=(as.numeric(qq1)+0.1),x=1:length(qq1)))
aa+aa1+aa2






pp<-predict(l,cor_f_train[cor_f_train$label==0,])
qq<-predict(l,cor_f_train[cor_f_train$label==1,])
aa<-ggplot()
aa1<-geom_point(aes(y=pp,x=1:length(pp)),colour='red')
aa2<-geom_point(aes(y=qq,x=1:length(qq)))
aa+aa1+aa2

pp1<-predict(l,cor_f_test[cor_f_test$label==0,])
qq1<-predict(l,cor_f_test[cor_f_test$label==1,])
aa<-ggplot()
aa1<-geom_point(aes(y=pp1,x=1:length(pp1)),colour='red')
aa2<-geom_point(aes(y=qq1,x=1:length(qq1)))
aa+aa1+aa2

bb<-cor_f_test[cor_f_test$label==1,]
bbb<-bb[which(qq1==0),]

aba<-cor_f_test[!(cor_f_test$people %in% bbb$people),]

