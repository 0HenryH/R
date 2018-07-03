#start Vancouver Temperature

#import package "fda"
library(fda)
#names(CanadianWeather)
#summary(CanadianWeather)

#set y
y = CanadianWeather$dailyAv[,"Vancouver","Temperature.C"]

#plot the temperature data
windows()
plot(1:365,y,xlab = "Day", ylab = "Temperature", main = "Vancouver")

#create Fourier Basis
FourierBasis = create.fourier.basis(rangeval = c(1,365),nbasis = 13)
#plot the basis function
#windows()
#plot(FourierBasis)

phi = eval.basis(1:365,FourierBasis)
dim(phi)

#calculate chat and yhat(the estimations at observation points)
chat = solve(t(phi)%*%phi)%*%(t(phi)%*%y)
chat
yhat = phi %*% chat
yhat

#look at the temperature on one point which is not observation point 
phit = eval.basis(3.5,FourierBasis)
y0 = phit %*% chat
y0

#B-Spline Basis
#first we create B-spline Basis
day = c(1,31,28,31,30,31,30,31,31,30,31,30,30)
knots = cumsum(day)
knots
BBasis = create.bspline.basis(rangeval = c(1,365),breaks = knots, norder = 4)
windows()
plot(BBasis)

#calculate the matrix Phi
Phi = eval.basis(1:365,BBasis)
dim(Phi)

#prediction on observation points 
chat = solve(t(Phi)%*%Phi)%*%(t(Phi)%*%y)
chat
yhat = Phi%*%chat
#draw
windows()
plot(1:365,y,xlab = "day", ylab = "Temperature", main = "Vancouver",col = 2)
lines(1:365,yhat,col = 1)

#look at the first derivative
dPhi = eval.basis(1:365,BBasis,Lfdobj = 1)
y2hat = dPhi %*% chat
windows()
plot(1:365,y2hat,col = 1)

#end Vancouver Temperature


#evaluate which number of Fourier Basis is better
for(i in c(7,13,15,23,91,111))
{
  Fbasis = create.fourier.basis(rangeval = c(1,365), i)
  mat = eval.basis(1:365,Fbasis)
  d1mat = eval.basis(1:365,Fbasis,Lfdobj = 1)
  d2mat = eval.basis(1:365,Fbasis,Lfdobj = 2)
  chat = solve(t(mat)%*%mat)%*%(t(mat)%*%y)
  yhat = mat%*%chat
  d1 = d1mat %*% chat
  d2 = d2mat %*% chat
  windows()
  plot(1:365,d1,main = "d1",type = "l")
  windows()
  plot(1:365,d2,main = "d2",type = "l")
  readline(prompt = "press Enter to continue")
}

#calculate the confidence interval
library(psych) #why import this??
sig2 = sum((y-yhat)^2)/(365-13) #J = 13, Number of Fourier Basis
Sig2 = sig2 * phi %*% solve(t(phi)%*%phi) %*% t(phi)
Off = 2 * sqrt(diag(Sig2))
Off
windows()
plot(1:365,y,col = 2,xlab = "day",ylab = "tem.",main = "Vancouver")
lines(1:365,yhat,col = 4)
lines(1:365,yhat-Off,lty = 2,col = 4)
lines(1:365,yhat+Off,lty = 2,col = 4)





