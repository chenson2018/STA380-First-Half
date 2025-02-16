library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn) ## knn library


attach(Boston)
n = dim(Boston)[1]

plot(lstat,medv)

train = data.frame(lstat,medv)
test = data.frame(lstat,medv)
ind = order(test[,1])
test =test[ind,]

MSE = NULL

kk = c(2,10,50,100,150,200,250,300,400,505)

for(i in kk){

near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)

MSE = c(MSE,aux)

plot(lstat,medv,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
lines(test[,1],near$fitted,col=2,lwd=2)
cat ("Press [enter] to continue")
line <- readline()
}


plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

near = kknn(medv~lstat,train,test,k=20,kernel = "rectangular")

for(i in seq(1,505,by=100)){
ii = near$C[i,1:20]
plot(lstat,medv,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
lines(test[,1],near$fitted,col=2,lwd=2)
abline(v=test[i,1],col=2,lty=2)
points(lstat[ii],medv[ii],pch=19,col="blue")
cat ("Press [enter] to continue")
line <- readline()
}

######################################
## OUT-OF-SAMPLE Prediction
######################################

train = data.frame(lstat,medv)
test = data.frame(lstat,medv)

tr = sample(1:506,400)

train = train[tr,]
test = test[-tr,]

out_MSE = NULL

for(i in 2:350){

near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)

out_MSE = c(out_MSE,aux)
}


best = which.min(out_MSE)

plot(log(1/(2:350)),sqrt(out_MSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
text(log(1/best),sqrt(out_MSE[best])+0.3,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)


near = kknn(medv~lstat,train,test,k=42,kernel = "rectangular")

ind = order(test[,1])
plot(lstat,medv,main=paste("k=",42),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#########################################
# leave-one-out cross validation (LOOCV)#
#########################################

train = data.frame(lstat,medv)
test = data.frame(lstat,medv)


out_MSE = matrix(0,n,100)

for(j in 1:n){


train_i = train[-j,]
test_i = test[j,]

for(i in 1:100){

near = kknn(medv~lstat,train_i,test_i,k=i,kernel = "rectangular")
aux = mean((test_i[,2]-near$fitted)^2)

out_MSE[j,i] = aux
}
cat(j,'\n')
}

mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)




#################################
# k-fold cross validation       #
#################################
kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){

if(n0<length(set)){val = sample(set,n0)}
if(n0>=length(set)){val=set}

train_i = train[-val,]
test_i = test[val,]

for(i in 1:100){

near = kknn(medv~lstat,train_i,test_i,k=i,kernel = "rectangular")
aux = mean((test_i[,2]-near$fitted)^2)

out_MSE[j,i] = aux
}

used = union(used,val)
set = (1:n)[-used]

cat(j,'\n')

}

mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)





#######################################
#### ******* Forensic Glass ****** ####
#######################################


data(fgl) ## loads the data into R; see help(fgl)
attach(fgl)

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6),cex.lab=1.4)



## for illustration, consider the RIxAl plane
## use 200 training points to find nearest neighbors for 14
train <- sample(1:214,200)
x <- scale(fgl[,c(4,1)]) 
gtype <- fgl$type
ng <- length(gtype)


nearest1 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=1)
nearest5 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=5)
data.frame(gtype[-train],nearest1,nearest5)

## plot them to see how it worked
par(mfrow=c(1,1))
plot(x[train,], col=gtype[train], cex=.8, main="5-nearest neighbor",pch=1)
lixo = (1:ng)[-train]
points(x[172,1],x[172,2], pch=18, col=nearest5[(1:14)[lixo==172]], cex=2)
for(i in 1:14){
points(x[lixo[i],1],x[lixo[i],2], pch=18, col="orange", cex=2)
points(x[lixo[i],1],x[lixo[i],2], pch=18, col=nearest1[i], cex=2)
}








###########################
## Classification Example##
###########################

NBA = read.csv("NBAspread.csv")

attach(NBA)
n = nrow(NBA)

par(mfrow=c(1,2))
hist(NBA$spread[favwin==1], col=5, main="", xlab="spread")
hist(NBA$spread[favwin==0], add=TRUE, col=6)
legend("topright", legend=c("favwin=1", "favwin=0"), fill=c(5,6), bty="n")
boxplot(NBA$spread ~ NBA$favwin, col=c(6,5), horizontal=TRUE, ylab="favwin", xlab="spread")


nbareg = glm(favwin~spread-1, family=binomial)

par(mfrow=c(2,2))

plot(spread,favwin,type="n",main="linear regression")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)
abline(lsfit(spread,favwin,intercept=FALSE),lwd=2,lty=2)


ind=order(spread)
plot(spread[ind],nbareg$fitted[ind], typ="l", col=1, lwd=2, lty=2,ylim=c(0,1), xlab="spread", ylab="favwin",main="logistic regression")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)


train = data.frame(spread,as.factor(favwin))
test = data.frame(spread,as.factor(favwin))
ind = order(test[,1])
test =test[ind,]

near = kknn(as.factor(favwin)~spread,train,test,k=5,kernel = "rectangular")
plot(spread,favwin,type="n",main="knn(5)")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)
lines(test[,1],near$prob[,2],col=1,lwd=2,lty=2)


near = kknn(as.factor(favwin)~spread,train,test,k=20,kernel = "rectangular")
plot(spread,favwin,type="n",main="knn(20)")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)
lines(test[,1],near$prob[,2],col=1,lwd=2,lty=2)

##########################
#### California knn      #
##########################

library(maps)


ca <- read.csv("CAhousing.csv")
logMedVal <- log(ca$medianHouseValue)

n=dim(ca)[1]

ind = sample(1:n,1000)

Y = logMedVal[ind]
CAdata = ca[ind,]


train = data.frame(Y,CAdata$latitude,CAdata$longitude)
test = data.frame(Y,CAdata$latitude,CAdata$longitude)


near = kknn(Y~.,train,test,k=10,kernel = "rectangular")


res = Y - near$fitted
nclr = 10
plotclr = colorRampPalette(c("cyan","magenta"))(nclr)
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
predbreaks = seq(min(Y),max(Y),length=nclr)
residbreaks = seq(min(res),max(res),length=nclr) # borders of resid color bins
residmap <- function(e){
  return(plotclr[cut(drop(e), residbreaks)]) ## cut sorts into bins
}
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}


par(mfrow=c(1,2))
## preds
map('state', 'california')
mtext("fitted values (k=10)",cex=2) 
points(test[,3:2], col=predmap(near$fitted), pch=19, cex=1)
map('state', 'california')
mtext("Residuals (k=10)",cex=2) 
points(test[,3:2], col=residmap(res), pch=19, cex=1)

n = dim(CAdata)[1]

kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){

if(n0<length(set)){val = sample(set,n0)}
if(n0>=length(set)){val=set}

train_i = train[-val,]
test_i = test[val,]

for(i in 1:100){

near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
aux = mean((test_i[,1]-near$fitted)^2)

out_MSE[j,i] = aux
}

used = union(used,val)
set = (1:n)[-used]

cat(j,'\n')
}





mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")




Income = scale(CAdata$medianIncome)*sd(CAdata$latitude)

train = data.frame(Y,CAdata$latitude,CAdata$longitude,Income)
test = data.frame(Y,CAdata$latitude,CAdata$longitude,Income)


near = kknn(Y~.,train,test,k=best,kernel = "rectangular")


res = Y - near$fitted
nclr = 10
plotclr = colorRampPalette(c("cyan","magenta"))(nclr)
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
predbreaks = seq(min(Y),max(Y),length=nclr)
residbreaks = seq(min(res),max(res),length=nclr) # borders of resid color bins
residmap <- function(e){
  return(plotclr[cut(drop(e), residbreaks)]) ## cut sorts into bins
}
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}


par(mfrow=c(1,2))
## preds
map('state', 'california')
mtext("fitted values (k=9)",cex=2) 
points(test[,3:2], col=predmap(near$fitted), pch=19, cex=1)
map('state', 'california')
mtext("Residuals (k=9)",cex=2) 
points(test[,3:2], col=residmap(res), pch=19, cex=1)












