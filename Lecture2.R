install.packages("fda")
library(fda)
#provide directory of the RData
?setwd
# use "/" instead of "\"
setwd("/Users/cao/Dropbox/Teaching/FDA/SummerCourse2018/R")

# daily temperature in Vancouver
names(CanadianWeather)
CanadianWeather$place
y = CanadianWeather$dailyAv[,'Vancouver','Temperature.C']
quartz() # To open a new graph windows in Mac computer
# windows() if you have a windows computer
mar.default <- c(5,4,4,2) + 0.1
# sets the bottom, left, top and right margins respectively of 
# the plot region in number of lines of text.
par(mar = c(8, 8, 4, 2))
plot(1:365,y,col=2,xlab='day',ylab='temperature',main='Vancouver',cex.axis=2,cex.lab=3,lwd=3,font = 3)
?plot

# Now a fourier basis
?create.fourier.basis
# the number of fourier basis functions has to be odd;
daybasis365 <- create.fourier.basis(c(1, 365), nbasis = 5)
quartz() # open a new graph window in R in a Mac laptop.
par(mfrow=c(1,1))
plot(daybasis365,lwd=4)

Phi = eval.basis(1:365,daybasis365) # basis matrix
dim(Phi)

chat = solve(t(Phi)%*%Phi)%*%(t(Phi)%*%y)
chat
yhat = Phi%*%chat

quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)
lines(1:365,yhat,lwd=2,col=4)

# We can evaluate the fitted curve at any time points in [1,365]
tstar = 3.3

phistar = eval.basis(tstar,daybasis365) # evaluate the basis functions at tstar
phistar

ystar = phistar%*%chat # the value of the fitted curve at tstar
ystar

# Now we want to look at a spline basis

knots = cumsum(c(1,31,28,31,30,31,30,31,31,30,31,30,30))
knots
bbasis1 = create.bspline.basis(rangeval=c(1,365),breaks=knots,norder=4)
length(knots) # the number of knots
norder = 4 # the order of polynomial functions = degree +1
length(knots)-2+norder # the number of basis functions
quartz() # open a new graph window in R in a Mac laptop.
par(mfrow=c(1,1))
plot(bbasis1,lwd=4)



Phi = eval.basis(1:365,bbasis1) # basis matrix
dim(Phi)

chat = solve(t(Phi)%*%Phi)%*%(t(Phi)%*%y)
chat
yhat = Phi%*%chat

quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)
lines(1:365,yhat,lwd=2,col=4)

# We can evaluate the fitted curve at any time points in [0,365]
tstar = 3.3

phistar = eval.basis(tstar,bbasis1) # evaluate the basis functions at tstar
phistar

ystar = phistar%*%chat # the value of the fitted curve at tstar
ystar

# By-product - Estimate derivatives
dPhi = eval.basis(1:365,bbasis1,Lfdobj=1) # basis matrix
dim(dPhi)

dyhat = dPhi%*%chat
quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,dyhat,type='l',lwd=2,col=2,cex=1.5,xlab='Day',ylab='Derivative of Temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)

d2Phi = eval.basis(1:365,bbasis1,Lfdobj=2) # basis matrix
dim(d2Phi)

d2yhat = d2Phi%*%chat
quartz()
par(mar = c(8, 8, 4, 2))
plot(1:365,d2yhat,type='l',lwd=2,col=2,cex=1.5,xlab='Day',
     ylab='Second Derivative of Temperature',cex.lab=1.5,
     main='Vancouver',cex.axis=1.5)
