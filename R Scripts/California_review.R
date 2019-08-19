##########################
#### California knn      #
##########################
library(kknn)
ca <- read.csv("CAhousing.csv")
logMedVal <- log(ca$medianHouseValue)

n=dim(ca)[1]

ind = sample(1:n,1000)

Y = logMedVal[ind]
CAdata = ca[ind,]


#### Cross validation
#####################

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



### California, regression.

#######################################################
#  California Housing Example (Model Selection)
#######################################################
rm(list=ls())

library(glmnet)


ca <- read.csv("CAhousing.csv")
logMedVal <- log(ca$medianHouseValue)

ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals
n = dim(ca)[1]
tr = sample(1:n,5000)


## create a full matrix of interactions (only necessary for linear model)
## do the normalization only for main variables.
XXca <- model.matrix(~.*longitude*latitude, data=data.frame(scale(ca)))[,-1]
CAdata = data.frame(logMedVal,XXca)


null = lm(logMedVal~1, data=CAdata[tr,])
full = lm(logMedVal~., data=CAdata[tr,])

regForward = step(null, scope=formula(full), direction="forward", k=log(length(tr)))
regBack = step(full, direction="backward", k=log(length(tr)))
regForward = step(null, scope=formula(full), direction="both", k=log(length(tr)))


XXca = scale(XXca)


Lasso.Fit = glmnet(XXca[tr,],logMedVal[tr])
Ridge.Fit = glmnet(XXca[tr,],logMedVal[tr],alpha=0)

par(mfrow=c(1,2))
plot(Lasso.Fit)
plot(Ridge.Fit)

CV.L = cv.glmnet(XXca[tr,], logMedVal[tr],alpha=1)
CV.R = cv.glmnet(XXca[tr,], logMedVal[tr],alpha=0)

LamR = CV.R$lambda.1se
LamL = CV.L$lambda.1se


par(mfrow=c(1,2))
plot(log(CV.R$lambda),sqrt(CV.R$cvm),main="Ridge CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamR),lty=2,col=2,lwd=2)
plot(log(CV.L$lambda),sqrt(CV.L$cvm),main="LASSO CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamL),lty=2,col=2,lwd=2)


coef.R = predict(CV.R,type="coefficients",s=LamR)
coef.L = predict(CV.L,type="coefficients",s=LamL)

par(mfrow=c(1,1))
plot(abs(coef.R[2:20]),abs(coef.L[2:20]),ylim=c(0,1),xlim=c(0,1))
abline(0,1)












