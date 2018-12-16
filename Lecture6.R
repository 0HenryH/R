library('fda')

#data loaded in Lecture3.R
#The first argument is basis function, second is to define the roughness penalty(here is second derivative),third is the penalty coefficiency
#fdPar create an object which store these definations

fdParobj = fdPar(daybasis365,Lfdobj=int2Lfd(2),lambda=1e4)

precfd = smooth.basis(1:365,y,fdParobj)

#ggplot(precfd,aes(x = 1:365,y = precfd$fd)) + geom_point()
windows()
plot(1:365,y,col=2,xlab='day',ylab='precipitation',main='Vancouver',
	cex.lab=1.5,cex.axis=1.5)
lines(precfd$fd,lwd=2,col=4) #why lines can deal with data of class fdSmooth?

#what is this?
infMat = bvals%*%precfd$y2cMap

matplot(infMat[,c(20,180,300)],type='l',ylab='influence',xlab='day',
	main='Observations 20, 180, 300',cex.lab=1.5,cex.axis=1.5,lwd=2)

precfd = smooth.basis(1:365,y,harmLfd)
harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
harmfdvals = eval.fd(1:365,precfd$fd,harmLfd)

windows()
plot(1:365,harmfdvals,type='l',lwd=2,col=4,xlab='day',ylab='Harmonic Acceleration',
	main ='Vancouver',cex.lab=1.5,cex.axis=1.5)




lambdas = exp(seq(-1,22,by=4))

gcvs = rep(0,length(lambdas))
dfs = rep(0,length(lambdas))
ocvs = rep(0,length(lambdas))
errs = rep(0,length(lambdas))

for(i in 1:length(lambdas)){
	fdParobj = fdPar(daybasis365,Lfdobj=int2Lfd(2),lambda=lambdas[i])
	precfd = smooth.basis(1:365,y,fdParobj)

	dfs[i] = precfd$df
	gcvs[i] = precfd$gcv
	errs[i] = precfd$SSE

	hatmat = bvals%*%precfd$y2cMap

	ocvs[i] = mean( (y-eval.fd(1:365,precfd$fd))^2/(1-diag(hatmat))^2 )


	pngname = paste('figs/vancprec_smooth',log(lambdas[i]),'.png',sep='')
	png(pngname)
	plot(1:365,y,col=2,xlab='day',ylab='Precipitation',main='Vancouver',
		cex.lab=1.5,cex.axis=1.5)
	lines(precfd$fd,lwd=2,col=4)
	dev.off()
}


par(mfrow=c(2,2))
l = log(lambdas)
plot(l,dfs,type='l',col=2,xlab='log lambda',ylab='df',cex.lab=1.5,cex.axis=1.5)
plot(l,errs,type='l',col=2,xlab='log lambda',ylab='SSE',cex.lab=1.5,cex.axis=1.5)
plot(l,ocvs,type='l',col=2,xlab='log lambda',ylab='OCV',cex.lab=1.5,cex.axis=1.5)
plot(l,gcvs,type='l',col=2,xlab='log lambda',ylab='GCV',cex.lab=1.5,cex.axis=1.5)


i = 17
fdParobj = fdPar(daybasis365,Lfdobj=int2Lfd(2),lambda=lambdas[i])
precfd = smooth.basis(1:365,y,fdParobj)

plot(1:365,y,col=2,xlab='day',ylab='precipitation',cex.lab=1.5,cex.axis=1.5,
	main="Vancouver")
lines(precfd$fd,col=4,lwd=2)

abline(v=190,col=4)
abline(v=230,col=4)
abline(v=210,col=2)


### Now a couple of probes

y2cMap = precfd$y2cMap

dfvals = eval.fd(1:365,precfd$fd,1)
dbvals = eval.basis(1:365,daybasis365,Lfdobj=1)

dfvar = diag(dbvals%*%y2cMap%*%t(y2cMap)%*%t(dbvals))

plot(1:365,dfvals,type='l',col=4,lwd=2,xlab='day',ylab='D precipitation',
	cex.lab=1.5,cex.axis=1.5,ylim=c(-0.07,0.12))
lines(1:365,dfvals+2*sqrt(dfvar),lty=2,lwd=2,col=4)
lines(1:365,dfvals-2*sqrt(dfvar),lty=2,lwd=2,col=4)
abline(h=0,col=2)



pbvals = eval.basis(c(190,210,230),daybasis365)
pbvals = c(0.5,-1,0.5)%*%pbvals
dpvar = diag(pbvals%*%y2cMap%*%t(y2cMap)%*%t(pbvals))

probeval = c(0.5,-1,0.5)%*%eval.fd(c(190,210,230),precfd$fd)

probeval
2*sqrt(dpvar)




