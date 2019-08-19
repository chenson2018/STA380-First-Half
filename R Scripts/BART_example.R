library(MASS)
x = Boston[,1:13] #rm=number of rooms and lstat= percent lower status
y = Boston$medv # median value
head(cbind(x,y))


library(BART) #BART package
set.seed(99) #MCMC, so set the seed
nd=200 # number of kept draws
burn=50 # number of burn in draws
bf = wbart(x,y,nskip=burn,ndpost=nd)



lmf = lm(y~.,data.frame(x,y))
fitmat = cbind(y,bf$yhat.train.mean,lmf$fitted.values)
colnames(fitmat)=c("y","BART","Linear")
cor(fitmat)



n=length(y) #total sample size
set.seed(14) #
ii = sample(1:n,floor(.75*n)) # indices for train data, 75% of data
xtrain=x[ii,]; ytrain=y[ii] # training data
xtest=x[-ii,]; ytest=y[-ii] # test data
cat("train sample size is ",length(ytrain)," and test sample size is ",length(ytest),"\n")

set.seed(99)
bf_train = wbart(xtrain,ytrain)
yhat = predict(bf_train,as.matrix(xtest))

yhat.mean = apply(yhat,2,mean)



plot(ytest,yhat.mean)
abline(0,1,col=2)












