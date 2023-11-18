path<-getwd()

library("xtable")


count<-0


Rep<-10000

number.of.areas <- 156

covariate.matrix<-matrix(0, nrow=number.of.areas, ncol=1)

covariate.matrix[,1]<-1

agg.loss.of.response.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)


agg.loss.of.theta.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.a.PR.kl<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.a.PR.sl<-matrix(0, nrow=number.of.areas, ncol=Rep)


agg.risk.of.response.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.loss.of.response.b.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)


agg.risk.of.theta.c.b.a.PR <- matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.c.b.a.PR.kl <- matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.c.b.a.PR.sl <- matrix(0, nrow=number.of.areas, ncol=1)


agg.risk.of.theta.c.b.a.FH <- matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.c.b.a.FH.kl <- matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.c.b.a.FH.sl <- matrix(0, nrow=number.of.areas, ncol=1)


  
agg.loss.of.theta.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.a.PR.kl<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.a.PR.sl<-matrix(0, nrow=number.of.areas, ncol=Rep)


agg.loss.of.theta.c.b.a.PR <- matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.c.b.a.PR.kl <- matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.c.b.a.PR.sl <- matrix(0, nrow=number.of.areas, ncol=Rep)



agg.risk.of.response.b.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.b.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.PR.estimator.a<-matrix(0, nrow=1, ncol=Rep)

agg.PR.estimator.b<-matrix(0, nrow=1, ncol=Rep)

agg.loss.PR.estimator.a<-matrix(0, nrow=1, ncol=Rep)

agg.loss.PR.estimator.b<-matrix(0, nrow=1, ncol=Rep)

agg.mse.PR.estimator.a<-matrix(0, nrow=1, ncol=1)

agg.mse.PR.estimator.b<-matrix(0, nrow=1, ncol=1)


agg.loss.of.response.a.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)


agg.loss.of.theta.a.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.a.FH.kl<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.a.FH.sl<-matrix(0, nrow=number.of.areas, ncol=Rep)


agg.loss.of.theta.c.b.a.FH <- matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.c.b.a.FH.kl <- matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.c.b.a.FH.sl <- matrix(0, nrow=number.of.areas, ncol=Rep)


agg.risk.of.response.a.FH<-matrix(0, nrow=number.of.areas, ncol=1)


agg.risk.of.theta.a.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.a.FH.kl<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.a.FH.sl<-matrix(0, nrow=number.of.areas, ncol=1)



agg.risk.of.theta.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.a.PR.kl<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.a.PR.sl<-matrix(0, nrow=number.of.areas, ncol=1)


agg.loss.of.response.b.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.loss.of.theta.b.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.risk.of.response.b.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.risk.of.theta.b.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.FH.estimator.a<-matrix(0, nrow=1, ncol=Rep)

agg.FH.estimator.b<-matrix(0, nrow=1, ncol=Rep)

agg.loss.FH.estimator.a<-matrix(0, nrow=1, ncol=Rep)

agg.loss.FH.estimator.b<-matrix(0, nrow=1, ncol=Rep)

agg.mse.FH.estimator.a<-matrix(0, nrow=1, ncol=1)

agg.mse.FH.estimator.b<-matrix(0, nrow=1, ncol=1)

beta<-matrix(1, nrow=ncol(covariate.matrix), ncol=1)

agg.second.order.unbiased.estimator.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.second.order.unbiased.estimator.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.average.second.order.unbiased.estimator.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.second.order.unbiased.estimator.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.approximated.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.approximated.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=1)


agg.second.order.unbiased.estimator.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.second.order.unbiased.estimator.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.average.second.order.unbiased.estimator.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.second.order.unbiased.estimator.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.approximated.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.approximated.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=1)


agg.constrained.second.order.unbiased.estimator.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.constrained.second.order.unbiased.estimator.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.constrained.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.constrained.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.average.constrained.second.order.unbiased.estimator.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.constrained.second.order.unbiased.estimator.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.constrained.second.order.unbiased.estimator.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.constrained.second.order.unbiased.estimator.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.constrained.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.approximated.constrained.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=Rep)

agg.average.constrained.second.order.unbiased.estimator.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.constrained.second.order.unbiased.estimator.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.constrained.approximated.risk.a.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.constrained.approximated.risk.b.FH<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.constrained.approximated.risk.a.PR<-matrix(0, nrow=number.of.areas, ncol=1)

agg.average.constrained.approximated.risk.b.PR<-matrix(0, nrow=number.of.areas, ncol=1)


agg.jackknife.unweighted.PR.s.o.a<-agg.jackknife.unweighted.PR.s.o.b<-agg.jackknife.unweighted.FH.s.o.a<-agg.jackknife.unweighted.FH.s.o.b<-agg.jackknife.unweighted.PR.s.o.c.a<-agg.jackknife.unweighted.PR.s.o.c.b<-agg.jackknife.unweighted.FH.s.o.c.a<-agg.jackknife.unweighted.FH.s.o.c.b<-agg.jackknife.unweighted.PR.a.a<-agg.jackknife.unweighted.PR.a.b<-agg.jackknife.unweighted.FH.a.a<-agg.jackknife.unweighted.FH.a.b<-agg.jackknife.unweighted.PR.a.c.a<-agg.jackknife.unweighted.PR.a.c.b<-agg.jackknife.unweighted.FH.a.c.a<-agg.jackknife.unweighted.FH.a.c.b<-matrix(0, nrow=number.of.areas, ncol=Rep)




agg.l<-0

tracking.counter<-1


list.problamatic <- c(8, 9)

tracking.counter<-1

for(f in 1:26){
  
  if (f %in% list.problamatic ==FALSE){
  
  path<-paste("/afs/crc.nd.edu/user/e/etorkash/Private/scale-loss-function/Scale-Loss-Function-March-2020/SL-",f,"/",sep="")
  
  load(file=paste(path,".RData",sep=""))
  
  k<-length(t)-1
  
  
  agg.loss.of.response.a.PR[, tracking.counter:(tracking.counter+k)]<-loss.of.response.a.PR[, t]
  

  
  agg.loss.of.theta.a.PR[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.a.PR[, t]
  
  agg.loss.of.theta.a.PR.kl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.a.PR.kl[, t]
  
  agg.loss.of.theta.a.PR.sl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.a.PR.sl[, t]
  
  

  
  agg.PR.estimator.a[tracking.counter:(tracking.counter+k)]<-PR.estimator.a[t]
  

  agg.loss.PR.estimator.a[tracking.counter:(tracking.counter+k)]<-loss.PR.estimator.a[t]
  

  agg.second.order.unbiased.estimator.risk.a.PR[, tracking.counter:(tracking.counter+k)]<-second.order.unbiased.estimator.risk.a.PR[, t]
  

  agg.approximated.risk.a.PR[, tracking.counter:(tracking.counter+k)]<-approximated.risk.a.PR[, t]
  

  agg.approximated.constrained.risk.a.PR[, tracking.counter:(tracking.counter+k)]<-approximated.constrained.risk.a.PR[, t]
  

  agg.approximated.constrained.risk.a.FH[, tracking.counter:(tracking.counter+k)]<-approximated.constrained.risk.a.FH[, t]
  

  agg.constrained.second.order.unbiased.estimator.risk.a.PR[, tracking.counter:(tracking.counter+k)]<-constrained.second.order.unbiased.estimator.risk.a.PR[, t]
  

  agg.constrained.second.order.unbiased.estimator.risk.a.FH[, tracking.counter:(tracking.counter+k)]<-constrained.second.order.unbiased.estimator.risk.a.FH[, t]
  
  
  
  agg.loss.of.response.a.FH[, tracking.counter:(tracking.counter+k)]<-loss.of.response.a.FH[, t]
  

  
  agg.loss.of.theta.a.FH[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.a.FH[, t]
  
  agg.loss.of.theta.a.FH.kl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.a.FH.kl[, t]
  
  agg.loss.of.theta.a.FH.sl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.a.FH.sl[, t]
  
  
  agg.loss.of.theta.c.b.a.PR[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.c.b.a.PR[, t]
  
  agg.loss.of.theta.c.b.a.PR.kl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.c.b.a.PR.kl[, t]
  
  agg.loss.of.theta.c.b.a.PR.sl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.c.b.a.PR.sl[, t]
  
  
  
  agg.loss.of.theta.c.b.a.FH[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.c.b.a.FH[, t]
  
  agg.loss.of.theta.c.b.a.FH.kl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.c.b.a.FH.kl[, t]
  
  agg.loss.of.theta.c.b.a.FH.sl[, tracking.counter:(tracking.counter+k)]<-loss.of.theta.c.b.a.FH.sl[, t]
  
  

  agg.FH.estimator.a[tracking.counter:(tracking.counter+k)]<-FH.estimator.a[t]
  

  agg.loss.FH.estimator.a[tracking.counter:(tracking.counter+k)]<-loss.FH.estimator.a[t]
  

  agg.second.order.unbiased.estimator.risk.a.FH[, tracking.counter:(tracking.counter+k)]<-second.order.unbiased.estimator.risk.a.FH[, t]
  

  agg.approximated.risk.a.FH[, tracking.counter:(tracking.counter+k)]<-approximated.risk.a.FH[, t]
  

  
  agg.jackknife.unweighted.PR.s.o.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.PR.s.o.a[, t]
  

  agg.jackknife.unweighted.FH.s.o.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.FH.s.o.a[, t]
  

  agg.jackknife.unweighted.PR.s.o.c.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.PR.s.o.c.a[, t]
  

  agg.jackknife.unweighted.FH.s.o.c.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.FH.s.o.c.a[, t]
  

  agg.jackknife.unweighted.PR.a.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.PR.a.a[, t]
  

  agg.jackknife.unweighted.FH.a.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.FH.a.a[, t]
  

  agg.jackknife.unweighted.PR.a.c.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.PR.a.c.a[, t]
  

  agg.jackknife.unweighted.FH.a.c.a[, tracking.counter:(tracking.counter+k)]<-jackknife.unweighted.FH.a.c.a[, t]
  

  
  
  
  tracking.counter<-tracking.counter+k+1
  
  
  cat(f)
  
  
  } 

    
}

tracking.counter<-tracking.counter-1


agg.avearge.jackknife.unweighted.PR.s.o.a<-agg.avearge.jackknife.unweighted.PR.s.o.b<-agg.avearge.jackknife.unweighted.FH.s.o.a<-agg.avearge.jackknife.unweighted.FH.s.o.b<-agg.average.jackknife.unweighted.PR.s.o.c.a<-agg.average.jackknife.unweighted.PR.s.o.c.b<-agg.average.jackknife.unweighted.FH.s.o.c.a<-agg.average.jackknife.unweighted.FH.s.o.c.b<-agg.average.jackknife.unweighted.PR.a.a<-agg.average.jackknife.unweighted.PR.a.b<-agg.average.jackknife.unweighted.FH.a.a<-agg.average.jackknife.unweighted.FH.a.b<-agg.average.jackknife.unweighted.PR.a.c.a<-agg.average.jackknife.unweighted.PR.a.c.b<-agg.average.jackknife.unweighted.FH.a.c.a<-agg.average.jackknife.unweighted.FH.a.c.b<-NULL


for(i in 1: number.of.areas){
  
  
  agg.avearge.jackknife.unweighted.PR.s.o.a[i]<-mean(agg.jackknife.unweighted.PR.s.o.a[i, 1:tracking.counter])
  

  agg.avearge.jackknife.unweighted.FH.s.o.a[i]<-mean(agg.jackknife.unweighted.FH.s.o.a[i, 1:tracking.counter])
  

  agg.average.jackknife.unweighted.PR.s.o.c.a[i]<-mean(agg.jackknife.unweighted.PR.s.o.c.a[i, 1:tracking.counter])
  

  agg.average.jackknife.unweighted.FH.s.o.c.a[i]<-mean(agg.jackknife.unweighted.FH.s.o.c.a[i, 1:tracking.counter])
  

  agg.average.jackknife.unweighted.PR.a.a[i]<-mean(agg.jackknife.unweighted.PR.a.a[i, 1:tracking.counter])
  

  agg.average.jackknife.unweighted.FH.a.a[i]<-mean(agg.jackknife.unweighted.FH.a.a[i, 1:tracking.counter])
  

  agg.average.jackknife.unweighted.PR.a.c.a[i]<-mean(agg.jackknife.unweighted.PR.a.c.a[i, 1:tracking.counter])
  

  agg.average.jackknife.unweighted.FH.a.c.a[i]<-mean(agg.jackknife.unweighted.FH.a.c.a[i, 1:tracking.counter])
  

  
  
  agg.average.constrained.second.order.unbiased.estimator.risk.a.PR[i]<-mean(agg.constrained.second.order.unbiased.estimator.risk.a.PR[i, 1:tracking.counter])
  

  agg.average.constrained.second.order.unbiased.estimator.risk.a.FH[i]<-mean(agg.constrained.second.order.unbiased.estimator.risk.a.FH[i, 1:tracking.counter])
  

  agg.average.constrained.approximated.risk.a.PR[i]<-mean(agg.approximated.constrained.risk.a.PR[i, 1:tracking.counter])
  

  agg.average.constrained.approximated.risk.a.FH[i]<-mean(agg.approximated.constrained.risk.a.FH[i, 1:tracking.counter])
  

  agg.average.second.order.unbiased.estimator.risk.a.FH[i]<-mean(agg.second.order.unbiased.estimator.risk.a.FH[i, 1:tracking.counter])
  

  agg.average.second.order.unbiased.estimator.risk.a.PR[i]<-mean(agg.second.order.unbiased.estimator.risk.a.PR[i, 1:tracking.counter])
  

  agg.average.approximated.risk.a.FH[i]<-mean(agg.approximated.risk.a.FH[i, 1:tracking.counter])
  

  agg.average.approximated.risk.a.PR[i]<-mean(agg.approximated.risk.a.PR[i, 1:tracking.counter])
  

  agg.mse.FH.estimator.a<-mean(agg.loss.FH.estimator.a[1:tracking.counter])
  

  agg.mse.PR.estimator.a<-mean(agg.loss.PR.estimator.a[1:tracking.counter])
  



  
  agg.risk.of.theta.a.PR[i]<-mean(agg.loss.of.theta.a.PR[i, 1:tracking.counter])
  
  agg.risk.of.theta.a.PR.kl[i]<-mean(agg.loss.of.theta.a.PR.kl[i, 1:tracking.counter])
  
  agg.risk.of.theta.a.PR.sl[i]<-mean(agg.loss.of.theta.a.PR.sl[i, 1:tracking.counter])
  
  
  agg.risk.of.theta.c.b.a.PR[i]<-mean(agg.loss.of.theta.c.b.a.PR[i, 1:tracking.counter])
  
  agg.risk.of.theta.c.b.a.PR.kl[i]<-mean(agg.loss.of.theta.c.b.a.PR.kl[i, 1:tracking.counter])
  
  agg.risk.of.theta.c.b.a.PR.sl[i]<-mean(agg.loss.of.theta.c.b.a.PR.sl[i, 1:tracking.counter])
  
  
  
  agg.risk.of.theta.a.FH[i]<-mean(agg.loss.of.theta.a.FH[i, 1:tracking.counter])
  
  agg.risk.of.theta.a.FH.kl[i]<-mean(agg.loss.of.theta.a.FH.kl[i, 1:tracking.counter])
  
  agg.risk.of.theta.a.FH.sl[i]<-mean(agg.loss.of.theta.a.FH.sl[i, 1:tracking.counter])
  
  
  agg.risk.of.theta.c.b.a.FH[i]<-mean(agg.loss.of.theta.c.b.a.FH[i, 1:tracking.counter])
  
  agg.risk.of.theta.c.b.a.FH.kl[i]<-mean(agg.loss.of.theta.c.b.a.FH.kl[i, 1:tracking.counter])
  
  agg.risk.of.theta.c.b.a.FH.sl[i]<-mean(agg.loss.of.theta.c.b.a.FH.sl[i, 1:tracking.counter])
  
  
  
  

  agg.risk.of.response.a.FH[i]<-mean(agg.loss.of.response.a.FH[i, 1:tracking.counter])
  

  agg.risk.of.response.a.PR[i]<-mean(agg.loss.of.response.a.PR[i, 1:tracking.counter])
  

  
}



setwd("/afs/crc.nd.edu/user/e/etorkash/Private/scale-loss-function/Scale-Loss-Function-March-2020/Results")

save(list = ls(all=TRUE), file = ".RCData")


date() 





risk.FH<-c(agg.risk.of.response.a.FH, agg.risk.of.response.b.FH, agg.risk.of.theta.a.FH, agg.risk.of.theta.b.FH, agg.average.second.order.unbiased.estimator.risk.a.FH, agg.average.constrained.second.order.unbiased.estimator.risk.a.FH, agg.average.second.order.unbiased.estimator.risk.b.FH, agg.average.constrained.second.order.unbiased.estimator.risk.b.FH, agg.average.approximated.risk.a.FH, agg.average.constrained.approximated.risk.a.FH, agg.average.approximated.risk.b.FH, agg.average.constrained.approximated.risk.b.FH )

Risk.FH<-matrix(risk.FH, nrow= number.of.areas, ncol=12, byrow=F)

xtable(Risk.FH)

Relative.Bias.FH<-c((agg.average.second.order.unbiased.estimator.risk.a.FH/agg.risk.of.theta.a.FH)-1, (agg.average.constrained.second.order.unbiased.estimator.risk.a.FH/agg.risk.of.theta.a.FH)-1, (agg.average.second.order.unbiased.estimator.risk.b.FH/agg.risk.of.theta.b.FH)-1, (agg.average.constrained.second.order.unbiased.estimator.risk.b.FH/agg.risk.of.theta.b.FH)-1, (agg.average.approximated.risk.a.FH/agg.risk.of.theta.a.FH)-1, (agg.average.constrained.approximated.risk.a.FH/agg.risk.of.theta.a.FH)-1, (agg.average.approximated.risk.b.FH/agg.risk.of.theta.b.FH)-1, (agg.average.constrained.approximated.risk.b.FH/agg.risk.of.theta.b.FH)-1)

RB.FH<-matrix(Relative.Bias.FH, nrow=number.of.areas, ncol=8, byrow=F)

xtable(RB.FH)


Relative.Bias.Jackknife.FH<-c((agg.avearge.jackknife.unweighted.FH.s.o.a/agg.risk.of.theta.a.FH)-1, (agg.average.jackknife.unweighted.FH.s.o.c.a/agg.risk.of.theta.a.FH)-1,(agg.avearge.jackknife.unweighted.FH.s.o.b/agg.risk.of.theta.b.FH)-1,  (agg.average.jackknife.unweighted.FH.s.o.c.b/agg.risk.of.theta.b.FH)-1, (agg.average.jackknife.unweighted.FH.a.a/agg.risk.of.theta.a.FH)-1, (agg.average.jackknife.unweighted.FH.a.c.a/agg.risk.of.theta.a.FH)-1, (agg.average.jackknife.unweighted.FH.a.b/agg.risk.of.theta.b.FH)-1,  (agg.average.jackknife.unweighted.FH.a.c.b/agg.risk.of.theta.b.FH)-1)

RB.J.U.FH<-matrix(Relative.Bias.Jackknife.FH, nrow=number.of.areas, ncol=8, byrow=F)

xtable(RB.J.U.FH)


risk.PR<-c(agg.risk.of.response.a.PR, agg.risk.of.response.b.PR, agg.risk.of.theta.a.PR, agg.risk.of.theta.b.PR, agg.average.second.order.unbiased.estimator.risk.a.PR, agg.average.constrained.second.order.unbiased.estimator.risk.a.PR,  agg.average.second.order.unbiased.estimator.risk.b.PR, agg.average.constrained.second.order.unbiased.estimator.risk.b.PR, agg.average.approximated.risk.a.PR, agg.average.constrained.approximated.risk.a.PR,  agg.average.approximated.risk.b.PR, agg.average.constrained.approximated.risk.b.PR)

Risk.PR<-matrix(risk.PR, nrow= number.of.areas, ncol=12, byrow=F)

xtable(Risk.PR)

Relative.Bias.PR<-c((agg.average.second.order.unbiased.estimator.risk.a.PR/agg.risk.of.theta.a.PR)-1, (agg.average.constrained.second.order.unbiased.estimator.risk.a.PR/agg.risk.of.theta.a.PR)-1, (agg.average.second.order.unbiased.estimator.risk.b.PR/agg.risk.of.theta.b.PR)-1, (agg.average.constrained.second.order.unbiased.estimator.risk.b.PR/agg.risk.of.theta.b.PR)-1, (agg.average.approximated.risk.a.PR/agg.risk.of.theta.a.PR)-1, (agg.average.constrained.approximated.risk.a.PR/agg.risk.of.theta.a.PR)-1, (agg.average.approximated.risk.b.PR/agg.risk.of.theta.b.PR)-1, (agg.average.constrained.approximated.risk.b.PR/agg.risk.of.theta.b.PR)-1)

RB.PR<-matrix(Relative.Bias.PR, nrow=number.of.areas, ncol=8, byrow=F)

xtable(RB.PR)


Relative.Bias.Jackknife.PR<-c((agg.avearge.jackknife.unweighted.PR.s.o.a/agg.risk.of.theta.a.PR)-1, (agg.average.jackknife.unweighted.PR.s.o.c.a/agg.risk.of.theta.a.PR)-1,(agg.avearge.jackknife.unweighted.PR.s.o.b/agg.risk.of.theta.b.PR)-1,  (agg.average.jackknife.unweighted.PR.s.o.c.b/agg.risk.of.theta.b.PR)-1, (agg.average.jackknife.unweighted.PR.a.a/agg.risk.of.theta.a.PR)-1, (agg.average.jackknife.unweighted.PR.a.c.a/agg.risk.of.theta.a.PR)-1, (agg.average.jackknife.unweighted.PR.a.b/agg.risk.of.theta.b.PR)-1,  (agg.average.jackknife.unweighted.PR.a.c.b/agg.risk.of.theta.b.PR)-1)

RB.J.U.PR<-matrix(Relative.Bias.Jackknife.PR, nrow=number.of.areas, ncol=8, byrow=F)

xtable(RB.J.U.PR)



FH.and.PR.estimators.performances<-c(agg.mse.FH.estimator.a, agg.mse.FH.estimator.b, agg.mse.PR.estimator.a, agg.mse.PR.estimator.b)

FH.and.PR.estimators.performances<-matrix(FH.and.PR.estimators.performances)

xtable(FH.and.PR.estimators.performances)






