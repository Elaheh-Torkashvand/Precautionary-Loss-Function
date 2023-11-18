remove(list=ls())

setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 

load(".RCData")


# # # load(".RCBData")

# # # install.packages("robustbase", dependencies= T)

library(robustbase)


q.theta.areas.Bayes.estimates <- quantile(theta_areas_Bayes_estimates, prob = seq(0, 1, length = 11), type = 5)


area.max.theta.areas.Bayes.estimates <- which(theta_areas_Bayes_estimates >=  q.theta.areas.Bayes.estimates[8])



area.min.theta.areas.Bayes.estimates  <- which(theta_areas_Bayes_estimates <= q.theta.areas.Bayes.estimates[4])


area.elements.heb.max <- NULL

for(i in 1: length(area.max.theta.areas.Bayes.estimates)){
	
	l <- area.max.theta.areas.Bayes.estimates[i]
	
	if(l==1){
		
		a<-1
		
		b<-n[l]
		
		area<-areas.modified[a:b]
		
	}
	
	if(l>1){
		
		a<-sum(n[1:(l-1)])+1
		
		b<-sum(n[1:l])
		
		area<-areas.modified[a:b]
		
	}
	
	# # # # print("###################### Count #######################")
					
	# # # # print(i)

    # # # # print("ratio of asthma"); print(mean(asthma[area]==1))
    
    # # # # print("ratio of gender"); print(mean(gender[area]==2))
    
    # # # # print("ratio of heritage"); print(mean(heritage_asthma[area]==1))	
	
		
	area.elements.heb.max<-c(area.elements.heb.max, area)
	
	}
	
	
    
	n[area.max.theta.areas.Bayes.estimates]
	
	sum(heritage_asthma[area.elements.heb.max])
	
	
	race[area.elements.heb.max]
	
	
	summary(bmi)
	
	summary(bmi[area.elements.heb.max])
	
	
	summary(age)
	
	summary(age[area.elements.heb.max])

	
	summary(vitamin.c)
	
	summary(vitamin.c[area.elements.heb.max])
	
	
		
	summary(vitamin.d)
	
	summary(vitamin.d[area.elements.heb.max])
	
	
		
	summary(smoking_status)
	
	summary(smoking_status[area.elements.heb.max])
	

	
	
	
	
	
	
###########################################################################################################################################
	

	
area <- NULL	
	
area.elements.heb.min <- NULL

for(i in 1: length(area.min.theta.areas.Bayes.estimates)){
	
	l <- area.min.theta.areas.Bayes.estimates[i]
	
	if(l==1){
		
		a<-1
		
		b<-n[l]
		
		area<-areas.modified[a:b]
		
	}
	
	if(l>1){
		
		a<-sum(n[1:(l-1)])+1
		
		b<-sum(n[1:l])
		
		area<-areas.modified[a:b]
		
	}
	
	# # # # print("###################### Count #######################")
					
	# # # # print(i)

    # # # # print("ratio of asthma"); print(mean(asthma[area]==1))
    
    # # # # print("ratio of gender"); print(mean(gender[area]==2))
    
    # # # # print("ratio of heritage"); print(mean(heritage_asthma[area]==1))	
	
		
	area.elements.heb.min<-c(area.elements.heb.min, area)
	
	}
	

    
	n[area.min.theta.areas.Bayes.estimates]
	
	sum(heritage_asthma[area.elements.heb.min])

    
    summary(bmi)
	
	summary(bmi[area.elements.heb.min])
	
	
	summary(age)
	
	summary(age[area.elements.heb.min])
	
	
	summary(vitamin.c)
	
	summary(vitamin.c[area.elements.heb.min])
	
	
		
	summary(vitamin.d)
	
	summary(vitamin.d[area.elements.heb.min])
	
	
		
	summary(smoking_status)
	
	summary(smoking_status[area.elements.heb.min])
	



# # # setwd("/Users/elahehtorkashvand/Desktop/scale-loss-function/SLF-Paper-Revision-2/")

	setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 
	


   setEPS()

   
   postscript("area_theta_heb_properties_age_bmi.eps")

   
   quartz(width=10, height=10)

  
   op <- par(mfrow=c(1, 2), family = "Times", lwd=1.5)


 
  box.entities.residual<-matrix(c(bmi[area.elements.heb.max], bmi[area.elements.heb.min]))


  t.test(bmi[area.elements.heb.max], bmi[area.elements.heb.min])
  
  
  data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 names = c(rep("High Rate", length(bmi[area.elements.heb.max])), rep("Low Rate", length(bmi[area.elements.heb.min]))))
                 
  adjbox(clusters ~ names, data = data.box.residual.theta, ylab="BMI", col=c("darkred", "darkblue"))




  box.entities.residual<-matrix(c(age[area.elements.heb.max], age[area.elements.heb.min]))

  
  t.test(age[area.elements.heb.max], age[area.elements.heb.min])
  

  data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 names = c(rep("High Rate", length(age[area.elements.heb.max])), rep("Low Rate", length(age[area.elements.heb.min]))))
                 
adjbox(clusters ~ names, data = data.box.residual.theta, ylab="Age", col=c("darkred", "darkblue"))


setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one/")


 dev.print(pdf, 'area_theta_heb_properties_age_bmi.pdf', width=10, height=6, paper='special')


        dev.off()





  
   setEPS()

   
   postscript("area_theta_heb_properties_vitamin_c_and_d.eps")

   
   quartz(width=10, height=10)

  
   op <- par(mfrow=c(1, 2), family = "Times", lwd=1.5)
   
         # # # op <- par(family = "Times", lwd=1.5)



   
  box.entities.residual<-matrix(c(vitamin.c[area.elements.heb.max], vitamin.c[area.elements.heb.min]))

  t.test(vitamin.c[area.elements.heb.max], vitamin.c[area.elements.heb.min])
  

  data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 names = c(rep("High Rate", length(vitamin.c[area.elements.heb.max])), rep("Low Rate", length(vitamin.c[area.elements.heb.min]))))
                 
  adjbox(clusters ~ names, data = data.box.residual.theta, ylab="Vitamin C", col=c("darkred", "darkblue"))



   
  box.entities.residual<-matrix(c(vitamin.d[area.elements.heb.max], vitamin.d[area.elements.heb.min]))


  t.test(vitamin.d[area.elements.heb.max], vitamin.d[area.elements.heb.min])
  
  
  data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 names = c(rep("High Rate", length(vitamin.d[area.elements.heb.max])), rep("Low Rate", length(vitamin.d[area.elements.heb.min]))))
                 
  adjbox(clusters ~ names, data = data.box.residual.theta, ylab="Vitamin D", col=c("darkred", "darkblue"))




 
  setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one/")


  dev.print(pdf, 'area_theta_heb_properties_vitamin_c_and_d.pdf', width=10, height=6, paper='special')


  dev.off()


#################################################################################
#################################################################################  
# # # heritage
  
# 
#   setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 
#   
#   
#   
#   setEPS()
#   
#   
#   postscript("area_theta_heb_properties_heritage.eps")
#   
#   
#   quartz(width=10, height=10)
#   
#   
#   op <- par(family = "Times", lwd=1.5)
#   
# 
#   
#   box.entities.residual<-matrix(c(heritage_asthma[area.elements.heb.max], heritage_asthma[area.elements.heb.min]))
#   
#   
#   data.box.residual.theta <- data.frame(clusters = box.entities.residual,
#                                         names = c(rep("High Rate", length(heritage_asthma[area.elements.heb.max])), rep("Low Rate", length(heritage_asthma[area.elements.heb.min]))))
#   
#   adjbox(clusters ~ names, data = data.box.residual.theta, ylab="Heritage", col=c("darkred", "darkblue"))
#   
#   
#   
#   setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one/")
#   
#   
#   dev.print(pdf, 'area_theta_heb_properties_heritage.pdf', width=10, height=6, paper='special')
#   
#   
#   dev.off()
#   
#   
  
  
  
  setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 
  
  
  
  setEPS()
  
  
  postscript("area_theta_heb_properties_heritage.eps")
  
  
  quartz(width=10, height=10)
  
  
  op <- par(family = "Times", lwd=1.5)
  
  
  
  box.entities.residual<-matrix(c(exp(covariate.matrix[area.max.theta.areas.Bayes.estimates, 2]), exp(covariate.matrix[area.min.theta.areas.Bayes.estimates, 2])))
  
  t.test(exp(covariate.matrix[area.max.theta.areas.Bayes.estimates, 2]), exp(covariate.matrix[area.min.theta.areas.Bayes.estimates, 2]))
  
  data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                                        names = c(rep("High Rate", length(covariate.matrix[area.max.theta.areas.Bayes.estimates, 2])), rep("Low Rate", length(covariate.matrix[area.min.theta.areas.Bayes.estimates, 2]))))
  
  adjbox(clusters ~ names, data = data.box.residual.theta, ylab="Heritage", col=c("darkred", "darkblue"))
  
  
  
  setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/SLF-Paper-Revision-JRSS-A-one/")
  
  
  dev.print(pdf, 'area_theta_heb_properties_heritage.pdf', width=10, height=6, paper='special')
  
  
  dev.off()
  
  

##################################################################################################################################################################################################################

remove(list=ls())

        setwd("/Users/elahehtorkashvand/Documents/scale-loss-function/Data-2020/") 
        
load(".RCData")



q.theta.areas.constrained.Bayes.estimates <- quantile(theta_areas_constrained_Bayes_estimates, prob = seq(0, 1, length = 11), type = 5)



area.max.theta.areas.cons.Bayes.estimates <- which(theta_areas_constrained_Bayes_estimates  >= q.theta.areas.constrained.Bayes.estimates[10])



area.min.theta.areas.cons.Bayes.estimates  <- which(theta_areas_constrained_Bayes_estimates <= q.theta.areas.constrained.Bayes.estimates[2])




	
area <- NULL	
	
area.elements.cheb.min <- NULL

for(i in 1: length(area.min.theta.areas.cons.Bayes.estimates)){
	
	l <- area.min.theta.areas.cons.Bayes.estimates[i]
	
	if(l==1){
		
		a<-1
		
		b<-n[l]
		
		area<-areas.modified[a:b]
		
	}
	
	if(l>1){
		
		a<-sum(n[1:(l-1)])+1
		
		b<-sum(n[1:l])
		
		area<-areas.modified[a:b]
		
	}
	
	# # # # print("###################### Count #######################")
					
	# # # # print(i)

    # # # # print("ratio of asthma"); print(mean(asthma[area]==1))
    
    # # # # print("ratio of gender"); print(mean(gender[area]==2))
    
    # # # # print("ratio of heritage"); print(mean(heritage_asthma[area]==1))	
	
		
	area.elements.cheb.min<-c(area.elements.cheb.min, area)
	
	}
	

       
	n[area.min.theta.areas.cons.Bayes.estimates]
	
	sum(heritage_asthma[area.elements.cheb.min])


    summary(bmi)
	
	summary(bmi[area.elements.cheb.min])
	
	
	summary(age)
	
	summary(age[area.elements.cheb.min])
	
	
	
	summary(vitamin.c)
	
	summary(vitamin.c[area.elements.cheb.min])
	
	
		
	summary(vitamin.d)
	
	summary(vitamin.d[area.elements.cheb.min])
	
	
		
	summary(smoking_status)
	
	summary(smoking_status[area.elements.cheb.min])
	

	
	

	
area <- NULL	
	
area.elements.cheb.max <- NULL

for(i in 1: length(area.max.theta.areas.cons.Bayes.estimates)){
	
	l <- area.max.theta.areas.cons.Bayes.estimates[i]
	
	if(l==1){
		
		a<-1
		
		b<-n[l]
		
		area<-areas.modified[a:b]
		
	}
	
	if(l>1){
		
		a<-sum(n[1:(l-1)])+1
		
		b<-sum(n[1:l])
		
		area<-areas.modified[a:b]
		
	}
	
	# # # # print("###################### Count #######################")
					
	# # # # print(i)

    # # # # print("ratio of asthma"); print(mean(asthma[area]==1))
    
    # # # # print("ratio of gender"); print(mean(gender[area]==2))
    
    # # # # print("ratio of heritage"); print(mean(heritage_asthma[area]==1))	
	
		
	area.elements.cheb.max<-c(area.elements.cheb.max, area)
	
	}
	
       
	n[area.max.theta.areas.cons.Bayes.estimates]
	
	sum(heritage_asthma[area.elements.cheb.max])

	

  summary(bmi)
	
	summary(bmi[area.elements.cheb.max])
	
	
	
	summary(age)
	
	summary(age[area.elements.cheb.max])

	
	
	summary(vitamin.c)
	
	summary(vitamin.c[area.elements.cheb.max])
	
	# # # t <-
		
	summary(vitamin.d)
	
	summary(vitamin.d[area.elements.cheb.max])
	
	
		
	summary(smoking_status)
	
	summary(smoking_status[area.elements.cheb.max])
	

	
	







# # # # # # 


# # # setwd("/Users/elahehtorkashvand/Desktop/scale-loss-function/SLF-Paper-Revision-2/")


   # # # setEPS()

   
   # # # postscript("area_theta_heb_properties_age_bmi.eps")

   
   # # # quartz(width=10, height=10)

  
   # # # op <- par(mfrow=c(1, 2), family = "Times", lwd=1.5)


 
  # # # box.entities.residual<-matrix(c(bmi[area.elements.heb.max], bmi[area.elements.heb.min], bmi))


  # # # data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 # # # names = c(rep("High Rate", length(bmi[area.elements.heb.max])), rep("Low Rate", length(bmi[area.elements.heb.min])), rep("Total", length(bmi))))
                 
# # # boxplot(clusters ~ names, data = data.box.residual.theta, ylab="BMI", col=c("darkred", "darkblue", "purple"))




  # # # box.entities.residual<-matrix(c(age[area.elements.heb.max], age[area.elements.heb.min], age))


  # # # data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 # # # names = c(rep("High Rate", length(age[area.elements.heb.max])), rep("Low Rate", length(age[area.elements.heb.min])), rep("Total", length(age))))
                 
# # # boxplot(clusters ~ names, data = data.box.residual.theta, ylab="Age", col=c("darkred", "darkblue", "purple"))


 # # # dev.print(pdf, 'area_theta_heb_properties_age_bmi.pdf', width=10, height=6, paper='special')


        # # # dev.off()





  
   # # # setEPS()

   
   # # # postscript("area_theta_heb_properties_vitamin_c_and_d.eps")

   
   # # # quartz(width=10, height=10)

  
   # # # op <- par(mfrow=c(1, 2), family = "Times", lwd=1.5)


   
  # # # box.entities.residual<-matrix(c(vitamin.c[area.elements.heb.max], vitamin.c[area.elements.heb.min], vitamin.c))


  # # # data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 # # # names = c(rep("High Rate", length(vitamin.c[area.elements.heb.max])), rep("Low Rate", length(vitamin.c[area.elements.heb.min])), rep("Total", length(vitamin.c))))
                 
# # # boxplot(clusters ~ names, data = data.box.residual.theta, ylab="Vitamin C", col=c("darkred", "darkblue", "purple"))



   
  # # # box.entities.residual<-matrix(c(vitamin.d[area.elements.heb.max], vitamin.d[area.elements.heb.min], vitamin.d))


  # # # data.box.residual.theta <- data.frame(clusters = box.entities.residual,
                 # # # names = c(rep("High Rate", length(vitamin.d[area.elements.heb.max])), rep("Low Rate", length(vitamin.d[area.elements.heb.min])), rep("Total", length(vitamin.d))))
                 
# # # boxplot(clusters ~ names, data = data.box.residual.theta, ylab="Vitamin D", col=c("darkred", "darkblue", "purple"))




 


        # # # dev.print(pdf, 'area_theta_heb_properties_vitamin_c_and_d.pdf', width=10, height=6, paper='special')


        # # # dev.off()



