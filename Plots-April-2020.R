###  January 7, 2023
###  The analysis and programs should be redone
### The .RCData file has changed!

remove(list=ls())

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 


load(".RCData")


t.1.0<-which(n==1 )



t.2.0<-which((n==2) )



t.3.0<-which((n==3) )



t.4.0<-which((n==4) )



t.5.0<-which((n==5) )



t.6.0<-which((n==6) )



t.7.0<-which((n==7) )




t.8.0<-which((n==8) )



t.9.0<-which(n==9)


t.10.0<-which(n==10)


t.11.0<-which(n==11)


t.12.0<-which(n==12)


t.13.0<-which(n==13)


t.14.0<-which(n==14)


t.15.0<-which(n==15)


t.16.0<-which(n==16)


t.17.0<-which(n==17)


t.18.0<-which(n==18)


t.19.0<-which(n==19)


t.20.0<-which(n==20)


t.21.0<-which(n==21)


t.22.0<-which(n==22)


t.25.0<-which(n==25)


t.26.0<-which(n==26)


t.27.0<-which(n==27)


t.31.0<-which(n==31)


t.32.0<-which(n==32)


t.35.0<-which(n==35)


t.37.0<-which(n==37)


t.39.0<-which(n==39)


t.41.0<-which(n==41)


t.47.0<-which(n==47)


t.55.0<-which(n==55)




picked.areas<-c(t.1.0[1],  t.2.0[1], t.3.0[1], t.4.0[1],  t.5.0[1], t.6.0[1], t.7.0[1],   t.8.0[1], t.9.0[1], t.10.0[1], t.11.0[1], t.12.0[1], t.13.0[1], t.14.0[1], t.15.0[1], t.16.0[1], t.17.0[1], t.18.0[1], t.19.0[1], t.19.0[1], t.20.0[1], t.21.0[1], t.22.0[1], t.25.0[1], t.26.0[1], t.27.0[1], t.31.0[1], t.32.0[1], t.35.0[1], t.37.0[1], t.39.0[1], t.41.0[1], t.47.0[1], t.55.0[1])

t<-which(is.na(picked.areas))

if(length(t)>0) picked.areas<-picked.areas[-t]


##################################
##################################

min.plot<-min(c(asthma_ratio[picked.areas], theta_areas_Bayes_estimates[picked.areas], theta_areas_constrained_Bayes_estimates_kl[picked.areas], theta_areas_constrained_Bayes_estimates_sl[picked.areas], theta_areas_constrained_Bayes_estimates[picked.areas], theta_areas_Bayes_estimates_sl[picked.areas], theta_areas_Bayes_estimates_kl[picked.areas]))-0.1

max.plot<-max(c(asthma_ratio[picked.areas], theta_areas_Bayes_estimates[picked.areas], theta_areas_constrained_Bayes_estimates_kl[picked.areas], theta_areas_constrained_Bayes_estimates_sl[picked.areas], theta_areas_constrained_Bayes_estimates[picked.areas], theta_areas_Bayes_estimates_sl[picked.areas], theta_areas_Bayes_estimates_kl[picked.areas]))+0.1

areas<-seq(1, length(picked.areas), 1)

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/") 


setEPS()

postscript("estimates.eps")

quartz(width=10, height=10)
op <- par(family = "Times", lwd=1, cex= 1)

     plot(areas, asthma_ratio[picked.areas], xlab="Sample Units in Areas", ylab="", type='l', 
     lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.3, xaxt="n", cex.axis=1.5, lty=2, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="black")
     
     lines(areas, theta_areas_Bayes_estimates[picked.areas], xlab=" ", ylab=" "
, type='l', 
     lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.4, cex.axis=1.5, lty=3, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="red")
     
     
     lines(areas, theta_areas_Bayes_estimates_kl[picked.areas], xlab=" ", ylab=" ", type='l', lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.4, cex.axis=1.5, lty=6, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="blue")
     
     
     lines(areas, theta_areas_Bayes_estimates_sl[picked.areas], xlab=" ", ylab=" ", type='l', lwd=2, pch=20,   cex=2, family="Times",
           cex.lab=1.4, cex.axis=1.5, lty=5, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="darkgoldenrod1")
     
     

	###  legend("topright", c(expression(paste(y)), expression(paste(delta^{CHEB})),  expression(paste(delta^{HEB})), expression(paste(delta[KL]^{CHEB})), expression(paste(delta[SE]^{CHEB}))), col=c("darkred", "black", "darkblue", "darkgreen", "orange"), lty=c(2, 6, 3), x.intersp=2, y.intersp= 2.5) 
	
	
	axis(1, at=seq(1, length(picked.areas)), labels=n[picked.areas], las=2)
	

	setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")
	
	
  dev.print(pdf, 'estimates.pdf', width=10, height=6, paper='special')


dev.off()

##################################
##################################

min.plot<-min(c(asthma_ratio[picked.areas], theta_areas_Bayes_estimates[picked.areas], theta_areas_constrained_Bayes_estimates_kl[picked.areas], theta_areas_constrained_Bayes_estimates_sl[picked.areas], theta_areas_constrained_Bayes_estimates[picked.areas], theta_areas_Bayes_estimates_sl[picked.areas], theta_areas_Bayes_estimates_kl[picked.areas]))-0.1

max.plot<-max(c(asthma_ratio[picked.areas], theta_areas_Bayes_estimates[picked.areas], theta_areas_constrained_Bayes_estimates_kl[picked.areas], theta_areas_constrained_Bayes_estimates_sl[picked.areas], theta_areas_constrained_Bayes_estimates[picked.areas], theta_areas_Bayes_estimates_sl[picked.areas], theta_areas_Bayes_estimates_kl[picked.areas]))+0.1

areas<-seq(1, length(picked.areas), 1)

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/") 


setEPS()

postscript("constrained-estimates.eps")

quartz(width=10, height=10)
op <- par(family = "Times", lwd=1, cex= 1)

plot(areas, theta_areas_constrained_Bayes_estimates[picked.areas], xlab="Sample Units in Areas", ylab="", type='l', 
     lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.3, xaxt="n", cex.axis=1.5, lty=2, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="red")


lines(areas, theta_areas_constrained_Bayes_estimates_kl[picked.areas], xlab=" ", ylab=" ", type='l', lwd=2, pch=20,   cex=2, family="Times",
      cex.lab=1.4, cex.axis=1.5, lty=6, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="blue")


lines(areas, theta_areas_constrained_Bayes_estimates_sl[picked.areas], xlab=" ", ylab=" ", type='l', lwd=2, pch=20,   cex=2, family="Times",
      cex.lab=1.4, cex.axis=1.5, lty=5, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot), col="darkgoldenrod1")


###  legend("topright", c(expression(paste(y)), expression(paste(delta^{CHEB})),  expression(paste(delta^{HEB})), expression(paste(delta[KL]^{CHEB})), expression(paste(delta[SE]^{CHEB}))), col=c("darkred", "black", "darkblue", "darkgreen", "orange"), lty=c(2, 6, 3), x.intersp=2, y.intersp= 2.5) 


axis(1, at=seq(1, length(picked.areas)), labels=n[picked.areas], las=2)


setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")


dev.print(pdf, 'constrained-estimates.pdf', width=10, height=6, paper='special')


dev.off()

##################   Add the risk of the response variable to different parts!!!!  
     
     min.plot<-min((c(average.second.order.unbiased.estimator.risk.a.FH[picked.areas], average.constrained.second.order.unbiased.estimator.risk.a.FH[picked.areas], risk.response.FH.estimation[picked.areas])))-0.005

     max.plot<-max((c(average.second.order.unbiased.estimator.risk.a.FH[picked.areas], average.constrained.second.order.unbiased.estimator.risk.a.FH[picked.areas], risk.response.FH.estimation[picked.areas])))+0.005

areas<-seq(1, length(picked.areas), 1)

setEPS()

postscript("risk_of_estimators.eps")

quartz(width=10,height=8)

op <- par(family = "Times", lwd=1)

     plot(areas, (risk.response.FH.estimation[picked.areas]), xlab="Sample Units in Areas", ylab="", type='l', 
     lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.3, xaxt="n", cex.axis=1.5, lty=2, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(min.plot, max.plot), col="darkred")
     
     
     
     lines(areas, (average.second.order.unbiased.estimator.risk.a.FH[picked.areas]), xlab=" ", ylab=" "
, type='l', 
     lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.4, cex.axis=1.5, lty=3, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(min.plot, max.plot), col="darkblue")


         
    # plot(areas, average.second.order.unbiased.estimator.risk.a.PR, xlab="(b)", ylab=expression(paste(hat(R)(theta^{HEB},theta))), type='l', 
     # lwd=5, pch=20,   cex=2, family="Times",
     # cex.lab=1.5, cex.axis=1.5, lty=5, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot))
     
     
      lines(areas, (average.constrained.second.order.unbiased.estimator.risk.a.FH[picked.areas]), xlab=" ", ylab=" ",        type='l', lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.4, cex.axis=1.5, lty=6, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(min.plot, max.plot), col="black")
     
	###  legend("topright", c(expression(paste(y)), expression(paste(delta^{CHEB})),  expression(paste(delta^{HEB}))), col=c("darkred", "black", "darkblue"), lty=c(2, 6, 3)) 
	
	
	
	axis(1, at=seq(1, length(picked.areas)), labels=n[picked.areas], las=2)

	setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")
	

    dev.print(pdf, 'risk_of_estimators.pdf', width=10, height=6, paper='special')


dev.off()

     
     # plot(areas, average.constrained.second.order.unbiased.estimator.risk.a.PR, xlab="(c)", ylab=expression(paste(hat(R)(theta^{CHEB},theta))),        type='l', lwd=5, pch=20,   cex=2, family="Times",
     # cex.lab=1.5, cex.axis=1.5, lty=5, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot))


##################################
##################################


min.plot<-min((c(avearge.jackknife.unweighted.FH.s.o.a[picked.areas], average.jackknife.unweighted.FH.s.o.c.a[picked.areas])))-0.005

max.plot<-max((c(avearge.jackknife.unweighted.FH.s.o.a[picked.areas], average.jackknife.unweighted.FH.s.o.c.a[picked.areas])))+0.005

areas<-seq(1, length(picked.areas), 1)


         
# # # #      plot(areas, avearge.jackknife.unweighted.PR.s.o.a, xlab="(a)", ylab=expression(paste(hat(R)[J](theta^{HEB},theta))), type='l', 
     # # lwd=5, pch=20,   cex=2, family="Times",
     # # cex.lab=1.6, cex.axis=1.5, lty=5, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot))
     
     
     # # plot(areas, average.jackknife.unweighted.PR.s.o.c.a, xlab="(b)", ylab=expression(paste(hat(R)[J](theta^{CHEB},theta))),        type='l', lwd=5, pch=20,   cex=2, family="Times",
     # # cex.lab=1.6, cex.axis=1.5, lty=5, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(0, max.plot))


setEPS()

postscript("jackknife_risk_of_estimators.eps")


quartz(width=10,height=8)
op <- par(family = "Times", lwd=1)


  plot(areas,(avearge.jackknife.unweighted.FH.s.o.a[picked.areas]), xlab="Sample Units in Areas", ylab="", type='l', 
     lwd=2, pch=20,   cex=2, family="Times",
     cex.lab=1.3, xaxt="n",  cex.axis=1.5, lty=3, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(min.plot, max.plot), col="darkblue")
     
     
        
      lines(areas,(average.jackknife.unweighted.FH.s.o.c.a[picked.areas]), xlab=" ", ylab=" ",        type='l', lwd=1.5, pch=20,   cex=2, family="Times",
     cex.lab=1.4, cex.axis=1.5, lty=6, mgp=c(2.2, 1.2, 0), mar=c(5, 4, 4, 2) + 0.1, ylim=c(min.plot, max.plot), col="black")
     
	###  legend("topright", c(expression(paste(delta^{CHEB})), expression(paste(delta^{HEB}))),  col=c("black", "darkblue"), lty=c(6, 3), y.intersp=2.5) 
	
	axis(1, at=seq(1, length(picked.areas)), labels=n[picked.areas], las=2)
	
	setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")
	
	dev.print(pdf, 'jackknife_risk_of_estimators.pdf', width=10, height=6, paper='special')
	
	
dev.off()


###########################################################################

###  WRONG! we have only a single observation from each area!


error.term.cb <- log_asthma_ratio - log(theta_areas_constrained_Bayes_estimates)

error.term.b <- log_asthma_ratio - log(theta_areas_Bayes_estimates)


# x.cb <- rnorm(length(error.term.cb), 0, sd(error.term.cb))

# x.b <- rnorm(length(error.term.b), 0, sd(error.term.b))

x.cb <- rnorm(length(error.term.cb), 0, sqrt(FH.estimator.a))

x.b <- rnorm(length(error.term.b), 0, sqrt(FH.estimator.a))


ks.test(error.term.cb, x.cb)

ks.test(error.term.b, x.b)


ks.test(error.term.cb, "pnorm", 0, sd(error.term.cb))

ks.test(error.term.b, "pnorm", 0, sd(error.term.b))

### it could not work here, in practice, what \tau^2 means is the variance of \delta^HEB or the variance of \delta^CHEB. 

setEPS()

postscript("normality_of_error.eps")


quartz(width=10,height=8)

op <- par(mfrow=c(2, 1), family = "Times", lwd=2, mar = c(5,5,4,2) + 0.1)


qqplot(x.cb, error.term.cb, xlab=expression(paste("Random variables from ", N(0, sigma[hat(epsilon)[CB]]))), ylab=expression(paste("Observed error terms using ", hat(phi)[CHEB])), main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim=c(-.4, 0.5), ylim=c(- 0.4, 0.5))

abline(a=0, b=1, lwd=2, col="darkred", lty=3)




qqplot(x.b, error.term.b, xlab=expression(paste("Random variables from ", N(0, sigma[hat(epsilon)[B]]))), ylab=expression(paste("Observed error terms using ", hat(phi)[HEB])), main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim=c(-.4, 0.5), ylim=c(- 0.4, 0.5))

abline(a=0, b=1, lwd=2, col="darkred", lty=3)

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")


dev.print(pdf, 'normality_of_error.pdf', width=10, height=6, paper='special')


dev.off()




##################################################################################################################################################################################################################

set.seed(14)

error.term.cb <- log(theta_areas_constrained_Bayes_estimates) - covariate.matrix %*% beta.boot.a.FH

error.term.b <- log(theta_areas_Bayes_estimates) - covariate.matrix %*% beta.boot.a.FH

### in the estimation of  FH.estimator.a, we have a unique solution while 
### I like to have different estimator in general.   

x.cb <- rnorm(length(error.term.cb), 0, sqrt(FH.estimator.a))

x.b <- rnorm(length(error.term.b), 0, sqrt(FH.estimator.a))



ks.test(error.term.cb, x.cb)

ks.test(error.term.b, x.b)





setEPS()

postscript("normality_of_error_regression.eps")


quartz(width=10,height=8)

op <- par(mfrow=c(2, 1), family = "Times", lwd=2, mar = c(5,5,4,2) + 0.1)



### qqplot(x.cb, error.term.cb, xlab=expression(paste("Random variables from ", N(0, hat(tau)[FH]^2))), ylab=expression(paste("Observed error terms using ", delta^{CHEB})), main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim= c(- 1.5, 1.5), ylim= c(- 1.5, 1.5))

qqplot(x.cb, error.term.cb, xlab=expression(paste("Random variables from ", N(0, hat(tau)[FH]^2))), ylab="", main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim= c(- 1.5, 1.5), ylim= c(- 1.5, 1.5))


abline(a=0, b=1, lwd=2, col="darkred", lty=3)




###  qqplot(x.b, error.term.b, xlab=expression(paste("Random variables from ", N(0, hat(tau)[FH]^2))), ylab=expression(paste("Observed error terms using ", delta^{HEB})), main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim=c(- 1.5, 1.5), ylim=c(- 1.5, 1.5))

qqplot(x.b, error.term.b, xlab=expression(paste("Random variables from ", N(0, hat(tau)[FH]^2))), ylab=" ", main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim=c(- 1.5, 1.5), ylim=c(- 1.5, 1.5))

abline(a=0, b=1, lwd=2, col="darkred", lty=3)


setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")

dev.print(pdf, 'normality_of_error_regression.pdf', width=10, height=6, paper='special')


dev.off()





setEPS()

postscript("normality_of_error_regression_hist.eps")


quartz(width=10,height=8)

op <- par(mfrow=c(2, 1), family = "Times", lwd=2, mar = c(5,5,4,2) + 0.1)



hist(error.term.cb, xlab= expression(paste("Observed error terms using ", delta^{CHEB})), freq= F, main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim= c(- 1.5, 1.5), border= "black", col= "gray")


curve(dnorm(x, 0, sqrt(FH.estimator.a)), col="darkred", add= T)


hist(error.term.b, xlab= expression(paste("Observed error terms using ", delta^{HEB})), freq= F, main="", lwd=2, pch=20,   cex=1.2, family="Times", cex.lab=1.2, cex.axis=1.2, lty=6, xlim= c(- 1.5, 1.5), border= "black", col= "gray")


curve(dnorm(x, 0, sqrt(FH.estimator.a)), col="darkred", add= T)


setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/")

dev.print(pdf, 'normality_of_error_regression_hist.pdf', width=10, height=6, paper='special')


dev.off()




##########################################################################################################################################################################################

remove(list=ls())

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 


load(".RCData")

library(xtable)



t.1.0<-which(n==1 )



t.2.0<-which((n==2) )



t.3.0<-which((n==3) )



t.4.0<-which((n==4) )



t.5.0<-which((n==5) )



t.6.0<-which((n==6) )



t.7.0<-which((n==7) )




t.8.0<-which((n==8) )



t.9.0<-which(n==9)


t.10.0<-which(n==10)


t.11.0<-which(n==11)


t.12.0<-which(n==12)


t.13.0<-which(n==13)


t.14.0<-which(n==14)


t.15.0<-which(n==15)


t.16.0<-which(n==16)


t.17.0<-which(n==17)


t.18.0<-which(n==18)


t.19.0<-which(n==19)


t.20.0<-which(n==20)


t.21.0<-which(n==21)


t.22.0<-which(n==22)


t.25.0<-which(n==25)


t.26.0<-which(n==26)


t.27.0<-which(n==27)


t.31.0<-which(n==31)


t.32.0<-which(n==32)


t.35.0<-which(n==35)


t.37.0<-which(n==37)


t.39.0<-which(n==39)


t.41.0<-which(n==41)


t.47.0<-which(n==47)


t.55.0<-which(n==55)




picked.areas<-c(t.1.0[1],  t.2.0[1], t.3.0[1], t.4.0[1],  t.5.0[1], t.6.0[1], t.7.0[1],   t.8.0[1], t.9.0[1], t.10.0[1], t.11.0[1], t.12.0[1], t.13.0[1], t.14.0[1], t.15.0[1], t.16.0[1], t.17.0[1], t.18.0[1], t.19.0[1], t.19.0[1], t.20.0[1], t.21.0[1], t.22.0[1], t.25.0[1], t.26.0[1], t.27.0[1], t.31.0[1], t.32.0[1], t.35.0[1], t.37.0[1], t.39.0[1], t.41.0[1], t.47.0[1], t.55.0[1])

t<-which(is.na(picked.areas))

if(length(t)>0) picked.areas<-picked.areas[-t]


length.picked.areas <-length(picked.areas)


relative.Bias.FH<-c(as.integer(n[picked.areas]), covariate.matrix[picked.areas, 2], ((agg.average.second.order.unbiased.estimator.risk.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.avearge.jackknife.unweighted.FH.s.o.a[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100,((agg.average.constrained.second.order.unbiased.estimator.risk.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.FH.s.o.c.a[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.risk.of.response.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100)


Relative.Bias.FH<-matrix(relative.Bias.FH, nrow= length(picked.areas), ncol=7, byrow=F)

# Relative.Bias.FH<-Relative.Bias.FH*100

xtable(Relative.Bias.FH)

###########################################################################################################################

agg.mse.constrained.second.order.unbiased.estimator.risk.a.FH<-NULL

agg.mse.jackknife.unweighted.FH.s.o.a<-NULL

agg.mse.second.order.unbiased.estimator.risk.a.FH<-NULL

agg.mse.jackknife.unweighted.FH.s.o.c.a<-NULL

agg.mse.of.response.a.FH<-NULL

for(i in 1: number.of.areas){
	
	   agg.mse.second.order.unbiased.estimator.risk.a.FH[i]<-mean((agg.second.order.unbiased.estimator.risk.a.FH[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
	
	
	   agg.mse.jackknife.unweighted.FH.s.o.a[i]<-mean((agg.jackknife.unweighted.FH.s.o.a[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
	
		
		agg.mse.constrained.second.order.unbiased.estimator.risk.a.FH[i]<-mean((agg.constrained.second.order.unbiased.estimator.risk.a.FH[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
		
		
		agg.mse.jackknife.unweighted.FH.s.o.c.a[i]<-mean((agg.jackknife.unweighted.FH.s.o.c.a[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
		
		agg.mse.of.response.a.FH[i]<-mean((agg.loss.of.response.a.FH[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)

}




relative.mse.FH<-c(as.integer(n[picked.areas]), covariate.matrix[picked.areas, 2], (agg.mse.second.order.unbiased.estimator.risk.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.FH.s.o.a[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100,(agg.mse.constrained.second.order.unbiased.estimator.risk.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.FH.s.o.c.a[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.of.response.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100)


relative.mse.FH<-matrix(relative.mse.FH, nrow= length(picked.areas), ncol=7, byrow=F)

# Relative.Bias.FH<-Relative.Bias.FH*100

xtable(relative.mse.FH)

###########################################################################################################################






relative.Bias.PR<-c(as.integer(n[picked.areas]), covariate.matrix[picked.areas, 2], ((agg.average.second.order.unbiased.estimator.risk.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.avearge.jackknife.unweighted.PR.s.o.a[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100,((agg.average.constrained.second.order.unbiased.estimator.risk.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.PR.s.o.c.a[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.risk.of.response.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100)


Relative.Bias.PR<-matrix(relative.Bias.PR, nrow= length(picked.areas), ncol=7, byrow=F)

# Relative.Bias.PR<-Relative.Bias.PR*100

xtable(Relative.Bias.PR)


###########################################################################################################################



agg.mse.jackknife.unweighted.PR.s.o.a<-NULL

agg.mse.second.order.unbiased.estimator.risk.a.PR<-NULL

agg.mse.jackknife.unweighted.PR.s.o.c.a<-NULL

agg.mse.constrained.second.order.unbiased.estimator.risk.a.PR<-NULL

agg.mse.of.response.a.PR<-NULL

for(i in 1: number.of.areas){
	
	   agg.mse.second.order.unbiased.estimator.risk.a.PR[i]<-mean((agg.second.order.unbiased.estimator.risk.a.PR[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
	
	
	   agg.mse.jackknife.unweighted.PR.s.o.a[i]<-mean((agg.jackknife.unweighted.PR.s.o.a[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
	
		
		agg.mse.constrained.second.order.unbiased.estimator.risk.a.PR[i]<-mean((agg.constrained.second.order.unbiased.estimator.risk.a.PR[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
		
		
		agg.mse.jackknife.unweighted.PR.s.o.c.a[i]<-mean((agg.jackknife.unweighted.PR.s.o.c.a[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
		
		agg.mse.of.response.a.PR[i]<-mean((agg.loss.of.response.a.PR[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)

}




relative.mse.PR<-c(as.integer(n[picked.areas]), covariate.matrix[picked.areas, 2], (agg.mse.second.order.unbiased.estimator.risk.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.PR.s.o.a[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100,(agg.mse.constrained.second.order.unbiased.estimator.risk.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.PR.s.o.c.a[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.of.response.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100)


relative.mse.PR<-matrix(relative.mse.PR, nrow= length(picked.areas), ncol=7, byrow=F)

# Relative.Bias.PR<-Relative.Bias.PR*100

xtable(relative.mse.PR)


###########################################################################################################################


agg.mse.jackknife.unweighted.PR.approx.o.a<-NULL

agg.mse.approx.estimator.risk.a.PR <-NULL

agg.mse.jackknife.unweighted.PR.approx.o.c.a<-NULL

agg.mse.constrained.approx.estimator.risk.a.PR<-NULL


for(i in 1: number.of.areas){
	
	   agg.mse.approx.estimator.risk.a.PR[i]<-mean((agg.approximated.risk.a.PR[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
	
	
	   agg.mse.jackknife.unweighted.PR.approx.o.a[i]<-mean((agg.jackknife.unweighted.PR.a.a[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
	
		
		agg.mse.constrained.approx.estimator.risk.a.PR[i]<-mean((agg.approximated.constrained.risk.a.PR[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
		
		
		agg.mse.jackknife.unweighted.PR.approx.o.c.a[i]<-mean((agg.jackknife.unweighted.PR.a.c.a[i, 1:tracking.counter]-agg.risk.of.theta.a.PR[i])^2)
		

}








# # Relative.Bias.and.Mse.PR<-c(as.integer(n[picked.areas]), covariate.matrix[picked.areas, 2], ((agg.average.approximated.risk.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.PR.a.a[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.constrained.approximated.risk.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.PR.a.c.a[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, (agg.mse.approx.estimator.risk.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.PR.approx.o.a[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100,(agg.mse.constrained.approx.estimator.risk.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.PR.approx.o.c.a[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100)



Relative.Bias.and.Mse.PR<-c(((agg.average.approximated.risk.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.PR.a.a[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.constrained.approximated.risk.a.PR[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.PR.a.c.a[picked.areas]/agg.risk.of.theta.a.PR[picked.areas])-1)*100, (agg.mse.approx.estimator.risk.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.PR.approx.o.a[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100,(agg.mse.constrained.approx.estimator.risk.a.PR[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.PR.approx.o.c.a[picked.areas]/(agg.risk.of.theta.a.PR[picked.areas])^2)*100)





Relative.Bias.and.Mse.PR <-matrix(Relative.Bias.and.Mse.PR, nrow= length(picked.areas), ncol=8, byrow=F)



xtable(Relative.Bias.and.Mse.PR)



###########################################################################################################################


agg.mse.jackknife.unweighted.FH.approx.o.a<-NULL

agg.mse.approx.estimator.risk.a.FH <-NULL

agg.mse.jackknife.unweighted.FH.approx.o.c.a<-NULL

agg.mse.constrained.approx.estimator.risk.a.FH<-NULL


for(i in 1: number.of.areas){
	
	   agg.mse.approx.estimator.risk.a.FH[i]<-mean((agg.approximated.risk.a.FH[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
	
	
	   agg.mse.jackknife.unweighted.FH.approx.o.a[i]<-mean((agg.jackknife.unweighted.FH.a.a[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
	
		
		agg.mse.constrained.approx.estimator.risk.a.FH[i]<-mean((agg.approximated.constrained.risk.a.FH[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
		
		
		agg.mse.jackknife.unweighted.FH.approx.o.c.a[i]<-mean((agg.jackknife.unweighted.FH.a.c.a[i, 1:tracking.counter]-agg.risk.of.theta.a.FH[i])^2)
		

}








# # Relative.Bias.and.Mse.FH<-c(as.integer(n[picked.areas]), covariate.matrix[picked.areas, 2], ((agg.average.approximated.risk.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.FH.a.a[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.constrained.approximated.risk.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.FH.a.c.a[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, (agg.mse.approx.estimator.risk.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.FH.approx.o.a[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100,(agg.mse.constrained.approx.estimator.risk.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.FH.approx.o.c.a[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100)



Relative.Bias.and.Mse.FH<-c(((agg.average.approximated.risk.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.FH.a.a[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.constrained.approximated.risk.a.FH[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, ((agg.average.jackknife.unweighted.FH.a.c.a[picked.areas]/agg.risk.of.theta.a.FH[picked.areas])-1)*100, (agg.mse.approx.estimator.risk.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.FH.approx.o.a[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100,(agg.mse.constrained.approx.estimator.risk.a.FH[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100, (agg.mse.jackknife.unweighted.FH.approx.o.c.a[picked.areas]/(agg.risk.of.theta.a.FH[picked.areas])^2)*100)





Relative.Bias.and.Mse.FH <-matrix(Relative.Bias.and.Mse.FH, nrow= length(picked.areas), ncol=8, byrow=F)



xtable(Relative.Bias.and.Mse.FH)


