remove(list=ls())

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023")



 pdf(file = "/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one-plus-MJJ-com-Revision-May-2023/Loss-Justification.pdf",   # The directory you want to save the file in
     width = 10, # The width of the plot in inches
     height = 8)
 
 

theta=c(0.5, 1, 2, 3)
D<-seq(0, 8, by=0.001)

par(mfrow=c(2, 2), mar=c(4, 2, 1, 1), family="Times", lwd=2)

t<-theta[1]
SEL<-function( d) {(d-t)^2}
KL<-function(d){t*((d/t)- log(d/t) -1 ) }
JT<-function(d) {d+ t^2/d - 2*t}



plot(D, SEL(D), type="l", lwd=1.5, xlab=expression(delta[i]), col="red", lty=2, xlim=c(0, 5), ylim=c(0, 8), ylab=expression(L(theta[0], delta)))
curve(KL, add=TRUE, col="blue", lty=3, lwd=2)
curve(JT, add=TRUE, col="black", lty=1, lwd=2.5)
legend("topright", c("SEL", "KL", "New Loss"), lty=c(2, 3, 1), lwd=c(1.5, 2, 2.5), col=c("red", "blue", "black") )
text(t+0.4,6, expression(theta[i]~"="~ 0.5) )
abline(v=t)




t<-theta[2]
SEL<-function( d) {(d-t)^2}
KL<-function(d){t*((d/t)- log(d/t) -1 ) }
JT<-function(d) {d+ t^2/d - 2*t}


plot(D, SEL(D), type="l", lwd=1.5, xlab=expression(delta[i]), col="red", lty=2, xlim=c(0, 5), ylim=c(0, 8), ylab=expression(L(theta[0], delta)))
curve(KL, add=TRUE, col="blue", lty=3, lwd=2)
curve(JT, add=TRUE, col="black", lty=1, lwd=2.5)
text(t+0.4,6, expression(theta[i]~"="~ 1.0) )
abline(v=t)



t<-theta[3]
SEL<-function( d) {(d-t)^2}
KL<-function(d){t*((d/t)- log(d/t) -1 ) }
JT<-function(d) {d+ t^2/d - 2*t}



plot(D, SEL(D), type="l", lwd=1.5, xlab=expression(delta[i]), col="red", lty=2, xlim=c(0, 5), ylim=c(0, 8), ylab=expression(L(theta[0], delta)))
curve(KL, add=TRUE, col="blue", lty=3, lwd=2)
curve(JT, add=TRUE, col="black", lty=1, lwd=2.5)
text(t+0.4,6, expression(theta[i]~"="~ 2) )
abline(v=t)


t<-theta[4]
SEL<-function( d) {(d-t)^2}
KL<-function(d){t*((d/t)- log(d/t) -1 ) }
JT<-function(d) {d+ t^2/d - 2*t}



plot(D, SEL(D), type="l", lwd=1.5, xlab=expression(delta[i]), col="red", lty=2, xlim=c(0, 5), ylim=c(0, 8), ylab=expression(L(theta[0], delta)))
curve(KL, add=TRUE, col="blue", lty=3, lwd=2)
curve(JT, add=TRUE, col="black", lty=1, lwd=2.5)
text(t+0.4,6, expression(theta[i]~"="~ 3) )
abline(v=t)




 dev.off()