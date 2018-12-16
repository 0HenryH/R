library('fda')

y = CanadianWeather$dailyAv[,'Vancouver','Precipitation.mm']

for(i in c(7,13,21,31,51,101,181)){
	
  daybasis365 <- create.fourier.basis(c(0, 365),i)
  bvals = eval.basis(1:365,daybasis365)
  
  # Evaluate the first derivative of the basis functions at 1,2,...,365
  d1bvals = eval.basis(1:365,daybasis365,Lfdobj=1)
  
  # Evaluate the second derivative of the basis functions at 1,2,...,365
  d2bvals = eval.basis(1:365,daybasis365,Lfdobj=2)
  
	X = bvals

	yhat = X%*%solve(t(X)%*%X)%*%(t(X)%*%y)
	# Estimate the second derivative of f(t)
	d2yhat = d2bvals%*%solve(t(X)%*%X)%*%(t(X)%*%y)

	windows()
	par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
	title = paste(i,' Basis Functions',sep='')
	plot(1:365,y,col=2,cex=1.5,xlab='day',ylab='precipitation',cex.lab=1.5,
		main=title,cex.axis=1.5)
	lines(1:365,yhat,lwd=2,col=4)
	windows()
	par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
	plot(1:365,d2yhat,col=4,type='l',lwd=2,cex.lab=1.5,cex.axis=1.5,
		ylab = 'D2 precipitation',xlab='day',main=title)
	readline(prompt="Press [enter] to continue")
}
# I'll choose 13 basis functions and look at standard errors

daybasis365 <- create.fourier.basis(c(0, 365),13)
bvals = eval.basis(1:365,daybasis365)
tbvals = bvals
S = tbvals%*%solve( t(tbvals)%*%tbvals )%*%t(tbvals)
yhat = S%*%y
#install.packages('psych')
library(psych)
tr(S)
err = (y-yhat)
sig = sum( (y-yhat)^2 )/(365-13)

# Variance-covariance matrix of fhat
Sig = sig*S%*%t(S)

# 2 * standard error of fitted curve
off = 2*sqrt(diag(Sig))

windows()
plot(1:365,y,col=2,xlab='day',ylab='precipitation',cex.lab=1.5,cex.axis=1.5,main='Vancouver')
lines(1:365,yhat,col=4,lwd=2)
lines(1:365,yhat+off,col=4,lwd=2,lty=2)
lines(1:365,yhat-off,col=4,lwd=2,lty=2)

