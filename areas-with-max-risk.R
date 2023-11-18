

### setwd("Desktop/scale-loss-function/data")

vitamin<-read.csv("DSQIDS_G.csv")

BP<-read.csv("Blood_Pressure.csv")

BM<-read.csv("Body_Measure.csv")

C_HDL<-read.csv("Chol_HDL.csv")

C_LDL<-read.csv("Chol_LDL.csv")

C_Total<-read.csv("CHOL_Total.csv")

DEMO<-read.csv("DEMO.csv")

MC<-read.csv("Medical_Condition.csv")

smoke<-read.csv("SMQFAM_G.csv")

###########################################
#####  Reading Variables #####

vitamin.d<-vitamin$dsqivd

t19<-which(!is.na(vitamin.d))   ####  length(t19)=3669

vitamin.c<-vitamin$dsqivc

t20<-which(!is.na(vitamin.c))   ####  length(t20)=3254

# # # sodium<-vitamin$dsqisodi

# # # t21<-which(!is.na(sodium))   ####  length(t20)= 764

gender<-DEMO$RIAGENDR

t1<-which(!is.na(gender))   #### length(t1)=9756

age<-DEMO$RIDAGEYR

t2<-which(!is.na(age))    ####  length(t2)=9756

race<-DEMO$RIDRETH3

t3<-which(!is.na(race))         ####  length(t3)=9756

bmi<-BM$BMXBMI

t7<-which(!is.na(bmi))    #####  length(t7)=8602

smoking_status<-smoke$SMD410

t14<-which(!is.na(smoking_status))      #####  length(t14)=9715

asthma<-MC$MCQ010

t17<-which(!is.na(asthma))            #######  length(t17)=9363

heritage_asthma<-MC$MCQ300B

t18<-which(!is.na(heritage_asthma))     ########   length(t18)=8161

########  Removaing NA's

####### Starting from the smallest values. The second smallest %in% smallest

####  Asthma disease

# # # t<-which(t20 %in% t21)

# # # index<-t20[t]


t<-which(t19 %in% t20)

index<-t19[t]


t<-which(t7 %in% index)

index<-t7[t]


t<-which(t18 %in% index)

index<-t18[t]

t<-which(t17 %in% index)

index<-t17[t]

t<-which(t14 %in% index)

index<-t14[t]

t<-t1 %in% index

index<-t1[t]

t<-t2 %in% index

index<-t2[t]

t<-t3 %in% index

index<-t3[t]

###########################################################
##################################  variables without NA's

vitamin.c<-vitamin.c[index]

vitamin.d<-vitamin.d[index]

sodium<-sodium[index]

gender<-gender[index]

age<-age[index]

race<-race[index]

bmi<-bmi[index]

smoking_status<-smoking_status[index]

asthma<-asthma[index]

heritage_asthma<-heritage_asthma[index]

##############################################
##################  Making small areas using age, smoking_status and race

q.vitamin.c<-quantile(vitamin.c, prob = seq(0, 1, length = 5), type = 5)

q.vitamin.d<-quantile(vitamin.d, prob = seq(0, 1, length = 5), type = 5)

q.sodium<-quantile(sodium, prob = seq(0, 1, length = 5), type = 5)

q.age<-quantile(age, prob = seq(0, 1, length = 5), type = 5)

q.bmi<-quantile(bmi, prob = seq(0, 1, length = 5), type = 5)


t<-NULL

t1<-c(1, 2, 3, 4, 6, 7)

t2<-c(1, 2, 9)

n<-NULL

count<-0

for(a1 in 1:4){   ### all turning
	
	for(a2 in 1:2){
		
		for(a3 in 1:4){
				
			for(a4 in 1: 6){
				
				for(a5 in 1:4){
					
					for(a6 in 1:4){
						
						# for(a7 in 1:3){
				
				l<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(l)>0){
					
					t<-c(t, l)
					
					count<-count+1										

					n[count]<-length(l)
					
                    
					   # }
					
					}
					
				if(a1==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

                
                if(a1==4 & a3==4){
					
					s<-which(smoking_status==a2 &  (bmi == q.bmi[(a3+1)]) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}
				
				
				if(a1==4 & a5==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & (vitamin.c == q.vitamin.c[a5+1]) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}


                				
				if(a1==4 & a6==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & (vitamin.d==q.vitamin.d[a6+1])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}

                              
                if(a1==4 & a3==4 & a5==4){
					
					s<-which(smoking_status==a2 &  (bmi == q.bmi[(a3+1)]) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & (vitamin.c==q.vitamin.c[(a5+1)]) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}
				
				               
                if(a1==4 & a3==4 & a6==4){
					
					s<-which(smoking_status==a2 &  (bmi == q.bmi[(a3+1)]) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & (vitamin.d==q.vitamin.d[(a6+1)])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}

				
				if(a1==4 & a5==4 & a6==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & (vitamin.c == q.vitamin.c[a5+1]) & (vitamin.d==q.vitamin.d[(a6+1)])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}
				

                				
				if(a1==4 & a3==4 & a5==4 & a6==4){
					
					s<-which(smoking_status==a2 &  (bmi== q.bmi[(a3+1)]) &  (q.age[(a1+1)]== age )   & (race==t1[a4]) & (vitamin.c == q.vitamin.c[a5+1]) & (vitamin.d==q.vitamin.d[(a6+1)])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}
				
					
				}
				
					
				if(a3==4){
					
					s<-which(smoking_status==a2 & (q.bmi[(a3+1)]== bmi )  & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}
					
					
				if(a3==4 & a5==4 ){
					
					s<-which(smoking_status==a2 & (q.bmi[(a3+1)]== bmi )  & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & (vitamin.c == q.vitamin.c[(a5+1)]) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}
					
					
					

					
				}
				
				
				if(a3==4 & a6==4){
					
					s<-which(smoking_status==a2 & (q.bmi[(a3+1)]== bmi )  & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & (vitamin.d==q.vitamin.d[(a6+1)])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}
					
					
					

					
				}
				
				
				if(a3==4 & a5==4 & a6==4){
					
					s<-which(smoking_status==a2 & (q.bmi[(a3+1)]== bmi )  & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & (q.vitamin.c[a5+1]==vitamin.c) & (vitamin.d==q.vitamin.d[a6+1])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}
					
					
					

					
				}




					
				}
				
									
				if(a5==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & (q.vitamin.c[(a5+1)]==vitamin.c) & ((q.vitamin.d[a6]<=vitamin.d)&(vitamin.d<q.vitamin.d[a6+1]))  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}
					
					if(a5==4 & a6==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & (q.vitamin.c[(a5+1)]==vitamin.c) &  (vitamin.d==q.vitamin.d[a6+1])  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}

					
				}
				
									
				if(a6==4){
					
					s<-which(smoking_status==a2 & ((q.bmi[a3]<= bmi ) & (bmi< q.bmi[(a3+1)])) & ((q.age[a1]<= age ) & (age< q.age[(a1+1)]))  & (race==t1[a4]) & ((q.vitamin.c[a5]<=vitamin.c)&(vitamin.c<q.vitamin.c[a5+1])) & (vitamin.d==q.vitamin.d[(a6+1)] )  )
				
				if(length(s)>0){
					
					t<-c(t, s)
					
					n[count]<-n[count]+length(s)
					
										
					}

					
				}
				


				
				
	
					
					
				}				
								
			}
			
		}
		
    }
    
  }
	
}



		

areas.modified<-NULL

n.modified<-NULL

l<-1

asthma_ratio<-NULL

covariate.matrix<-matrix(0, nrow=count, ncol=4)
 
for(i in 1:count){
	
	if(i==1){
		
		a<-1
		
		b<-n[i]
		
		area<-t[a:b]
		
	}
	
	if(i>1){
		
		a<-sum(n[1:(i-1)])+1
		
		b<-sum(n[1:i])
		
		area<-t[a:b]
		
	}
	
	print("###################### Count #######################")
					
	print(i)

    print("ratio of asthma"); print(mean(asthma[area]==1))
    
    print("ratio of gender"); print(mean(gender[area]==2))
    
    print("ratio of heritage"); print(mean(heritage_asthma[area]==1))	
	
	if(mean(asthma[area]==1)!=0) {
		
	areas.modified<-c(areas.modified, area)
	
	n.modified[l]<-n[i]
	
	asthma_ratio[l]<-mean(asthma[area]==1)
		
	covariate.matrix[l, 1]<-1
	
	covariate.matrix[l, 2]<-mean(bmi[area])
	
	covariate.matrix[l, 3]<-mean(gender[area]==2)
	
	covariate.matrix[l, 4]<-mean(heritage_asthma[area]==1)
		
	l<-l+1
	
	}
}







area.max<-which(average.second.order.unbiased.estimator.risk.a.PR==max(average.second.order.unbiased.estimator.risk.a.PR))



units.of.area.max<-NULL

for(i in 1:length(area.max)){
	
	
	if(area.max[i]==1){
		
		a<-1
		
		b<-n[area.max[i]]
		
		area<-areas.modified[a:b]
		
	}
	
	if(area.max[i]>1){
		
		a<-sum(n[1:(area.max[i]-1)])+1
		
		b<-sum(n[1:area.max[i]])
		
		area<-areas.modified[a:b]
		
	}

	units.of.area.max<-c(units.of.area.max, area)
	
	
}

smoking_status[units.of.area.max]

min(bmi[units.of.area.max])

max(bmi[units.of.area.max])

vitamin.c[units.of.area.max]

vitamin.d[units.of.area.max]

race[units.of.area.max]

age[units.of.area.max]