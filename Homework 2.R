total<-Reduce(merge,list(carcom16,lavoro,q16a,rper16))
#select observations with nord=1
total1 <- total[which(total$nord=='1'),]

#select variables
myvars <- c("nord","nquest","yl1", "sex", "studio","stupcf","stumcf","eta","acontrib","staciv","ireg","area3")
total2 <- total1[myvars]

y.schooling<-ifelse(total2$studio==1,0, 
                    ifelse(total2$studio==2,5, 
                           ifelse(total2$studio==3,8,
                                  ifelse(total2$studio==4,13,
                                         ifelse(total2$studio==5,16,
                                                ifelse(total2$studio==6,21,21))))))
                
m.schooling<-ifelse(total2$stumcf==1,0,
                    ifelse(total2$stumcf==2,5,
                           ifelse(total2$stumcf==3,8,
                                  ifelse(total2$stumcf==4,13,
                                         ifelse(total2$stumcf==5,16,
                                                ifelse(total2$stumcf==6,21,21))))))
             
f.schooling<-ifelse(total2$stupcf==1,0,
                    ifelse(total2$stupcf==2,5,
                           ifelse(total2$stupcf==3,8,
                                  ifelse(total2$stupcf==4,13,
                                         ifelse(total2$stupcf==5,16,
                                                ifelse(total2$stupcf==6,21,21))))))

total2$y.schooling<-y.schooling
total2$m.schooling<-m.schooling
total2$f.schooling<-f.schooling
total2
remvar <- names(total2) %in% c("studio", "stumcf", "stupcf") 
total3 <- total2[!remvar]


#correlation table
library(corrplot)

#to overcome missing values problems
total4<-cor(total3, use="pairwise.complete.obs")

#numerical and circle correlation tables
corrplot(total4,method="number")
corrplot(total4,method="circle")

library(psych)
dummy.male=dummy.code(total3$sex,1,na.rm=TRUE)
dummy.married=dummy.code(total3$staciv,1,na.rm=TRUE)

total3$dummy.male<-dummy.male
total3$dummy.married<-dummy.married
remvar <- names(total3) %in% c("sex", "staciv") 
total5 <- total3[!remvar]

#hourly wage
wage_h<-total5$yl1/1920

total5$wage_h<-wage_h
remvar <- names(total5) %in% c("yl1") 
total6 <- total5[!remvar]


total7 <- total6[which(total6$wage_h > 0),]
acontrib_sq<-(total7$acontrib)^2
total7$acontrib_sq<-acontrib_sq
mod<-lm(log(wage_h) ~ dummy.male+dummy.married+y.schooling+acontrib+acontrib_sq, total7)
summary(mod)

#heteroskedasticity test
library(lmtest)
bptest(mod)

dummy.center=factor(total7$area3==2)
dummy.south=factor(total7$area3==3)
total7$dummy.center<-dummy.center
total7$dummy.south<-dummy.south
mod_area<-lm(y.schooling~dummy.center+dummy.south,total7)
summary (mod_area)


#instrumental variables (m.schooling and f.schooling) estimation and endogeneity test 
library(AER)
mod_iv<-ivreg(log(wage_h) ~ y.schooling+dummy.male+dummy.married+acontrib+acontrib_sq
              | dummy.male+dummy.married+acontrib+acontrib_sq+m.schooling+f.schooling,data=total7)
summary(mod_iv, vcov=sandwich, diagnostics=TRUE)

               
#regional average hourly wage
avg_wage_h_area<-aggregate(total7[, 12], list(total7$ireg), mean)

#regional average variables 
avg_schooling_area<-aggregate(total7[,7],list(total7$ireg),mean)
avg_acontrib_area<-aggregate(total7[,4],list(total7$ireg),mean)
avg_acontrib2_area<-aggregate(total7[,13],list(total7$ireg),mean)
male_area<-aggregate(total7[,10],list(total7$ireg),mean)
married_area<-aggregate(total7[,11],list(total7$ireg),mean)

avg.matrix<-cbind(avg_wage_h_area,avg_acontrib_area,avg_acontrib2_area,avg_schooling_area,male_area,married_area, col.names=TRUE)
        
avg_wage_h_area_unlist <-unlist(avg_wage_h_area)
avg_schooling_area_unlist <-unlist(avg_schooling_area)
avg_acontrib_area_unlist <-unlist(avg_acontrib_area)
avg_acontrib2_area_unlist <-unlist(avg_acontrib2_area)
male_area_unlist <-unlist(male_area)
married_area_unlist <-unlist(married_area)
mod_av<-lm(avg_wage_h_area_unlist ~ avg_schooling_area_unlist+avg_acontrib_area_unlist+avg_acontrib2_area_unlist+male_area_unlist+married_area_unlist,avg.matrix)
summary(mod_av)

#heteroskedasticity test
bptest(mod_av)


