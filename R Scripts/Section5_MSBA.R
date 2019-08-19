
##################################################
### Section 1. The Neural Net Model for Numeric Y
###read in the data
zag = read.csv("zagat.csv",header=T)
summary(zag)


###standardize the x's
minv = rep(0,3)
maxv = rep(0,3)
zagsc = zag
for(i in 1:3) {
minv[i] = min(zag[[i]])
maxv[i] = max(zag[[i]])
zagsc[[i]] = (zag[[i]]-minv[i])/(maxv[i]-minv[i])
}

### nn library
library(nnet)

###fit nn with just one x=food
set.seed(99)
znn = nnet(price~food,zagsc,size=3,decay=.1,linout=T)


###get fits, print summary,  and plot fit
fznn = predict(znn,zagsc)
plot(zagsc$food,zagsc$price)
oo = order(zagsc$food)
lines(zagsc$food[oo],fznn[oo],col="red",lwd=2)
abline(lm(price~food,zagsc)$coef)

summary(znn) # what does this mean?

### try 5 units
set.seed(99)
znn = nnet(price~food,zagsc,size=5,decay=.1,linout=T)
print(summary(znn))

### all three x's
znn = nnet(price~.,zagsc,size=5,decay=.1,linout=T)
fznn = predict(znn,zagsc)
zlm = lm(price~.,zagsc)
fzlm = predict(zlm,zagsc)
temp = data.frame(y=zagsc$price,fnn=fznn,flm=fzlm)
pairs(temp)
print(cor(temp))


##################################################
## Size and Decay


### try four different fits

set.seed(14)
znn1 = nnet(price~food,zagsc,size=3,decay=.5,linout=T)
znn2 = nnet(price~food,zagsc,size=3,decay=.00001,linout=T)
znn3 = nnet(price~food,zagsc,size=50,decay=.5,linout=T)
znn4 = nnet(price~food,zagsc,size=50,decay=.00001,linout=T)
temp = data.frame(price = zagsc$price, food = zagsc$food)
znnf1 = predict(znn1,temp)
znnf2 = predict(znn2,temp)
znnf3 = predict(znn3,temp)
znnf4 = predict(znn4,temp)

### plot the fits

par(mfrow=c(2,2))
plot(zagsc$food,zagsc$price)
lines(zagsc$food[oo],znnf1[oo],lwd=2)
title("size=3, decay=.5")
plot(zagsc$food,zagsc$price)
lines(zagsc$food[oo],znnf2[oo],lwd=2)
title("size=3, decay=.00001")
plot(zagsc$food,zagsc$price)
lines(zagsc$food[oo],znnf3[oo],lwd=2)
title("size = 50, decay = .5")
plot(zagsc$food,zagsc$price)
lines(zagsc$food[oo],znnf4[oo],lwd=2)
title("size = 50, decay = .00001")

##################################################
### Iterative Fitting and Random Starting Values

### you can control the number of iterations
set.seed(99)
znn3 = nnet(price~food,zagsc,size=50,decay=.5,linout=T)
znn3 = nnet(price~food,zagsc,size=50,decay=.5,linout=T,maxit=20)
znn3 = nnet(price~food,zagsc,size=50,decay=.5,linout=T,maxit=1000)

znnf3 = predict(znn3,temp)
par(mfrow=c(1,1))
plot(zagsc$food,zagsc$price)
lines(zagsc$food[oo],znnf3[oo],lwd=2)


### starting values are random !!!
## you need not converge to the same place

set.seed(23)
temp = nnet(price~food,zagsc,size=2,decay=.001)
summary(temp)

temp = nnet(price~food,zagsc,size=2,decay=.001)
summary(temp)

set.seed(23)
temp = nnet(price~food,zagsc,size=2,decay=.001)
summary(temp)


##################################################
### How Does it Work?

### How does it work
## Zagat fit
print(summary(znn))
x = zagsc$food
y = zagsc$price

z1 = 4.35 -0.24 *x
z2 = -7.42 +21.41*x
z3 = -9.93 +13.28*x

f1 = 12.09*exp(z1)/(1+exp(z1))
f2 = 10.7*exp(z2)/(1+exp(z2))
f3 = 22.74*exp(z3)/(1+exp(z3))

oo = order(x)
plot(x,y-12.33)

lines(x[oo],f1[oo],col=2) 
lines(x[oo],f2[oo],col=3) 
lines(x[oo],f3[oo],col=4) 
lines(x[oo],(f1+f2+f3)[oo],col=5)

###how would you fit a bump

set.seed(23)
x = runif(1000)
x = sort(x)
y = exp(-80*(x-.5)*(x-.5)) + .05*rnorm(1000)
plot(x,y)
df = data.frame(y=y,x=x)

plot(x,y)
sz = 3
#Try various decay values.
for(i in 1:20) {
   nnsim = nnet(y~x,df,size=sz,decay = 1/2^i,linout=T,maxit=1000)
   simfit = predict(nnsim,df)
   lines(x,simfit,col=i,lwd=3)
   print(i)
   readline()
}

set.seed(99)
nnsim = nnet(y~x,df,size=3,decay=1/2^12,linout=T,maxit=1000)
thefit = predict(nnsim,df)
plot(x,y)
lines(x,thefit,col="blue",lwd=3,cex.axis=1.5,cex.lab=1.5)


z1 =  5.26 - 13.74*x
z2 = -6.58 + 13.98*x
z3 = -9.67 + 17.87*x

F = function(x) {return(exp(x)/(1+exp(x)))}

f1 = 2.21*F(z1)
f2 = 7.61*F(z2)
f3 = -5.40*F(z3)

 
rx=range(x)
ry = range(c(f1,f2,f3,y))
plot(rx,ry,type="n",xlab="x",ylab="fit",cex.axis=2,cex.lab=2)
points(x,y)
lines(x,f1,col=1,lwd=2)
lines(x,f2,col=2,lwd=2)
lines(x,f3,col=3,lwd=2)
lines(x,-2.05+f1+f2+f3,col=4,lwd=4)









