###########################################################################################
# PROJECT         : COVID trajectory category analysis
# PROGRAM NAME    : GMM_for_Publish.R
# DESCRIPTION     : COVID trajectory growth mixture model analysis and model fit
# NOTE            : 
#                 : any "outpath", "setwd" need to be updated 
#                 : the results for the random sample of 100 trajectories for fit plots 
#                 : might vary because seed number was not selected 
###########################################################################################


library(ggplot2)
library(lubridate)
library(dplyr)
library(lcmm)


################################################################
# Fitted growth mixture models
################################################################


###############
# Period-1
###############

rm(list=ls())

outpath <- ""


setwd("/\\t1")

ir<-read.csv("Ir_v1_1.csv",header=TRUE)
ir$FIPS<-as.factor(ir$FIPS_CODE)


# log transform the outcome  
ir_2=ir
ir_2$y=log(ir_2$ci100000+0.1)
ir_2$t=ir_2$weekn

# GMM fitted number of classes from 1 to 6

m1=lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=1,maxiter = 500, data=ir_2, link='splines')
# saveRDS(m1,"m1.rds")

m2=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=2,  data=ir_2,  mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m2,"m2.rds")

m3=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=3,  data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 500, link='splines'),rep=30,maxiter=50,minit=m1, cl = 4)
# saveRDS(m3,"m3.rds")

m4=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=4,  data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m4,"m4.rds")

m5=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=5,  data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m5,"m5.rds")

getwd()

m1=readRDS(file = "m1.rds")
m2=readRDS(file = "m2.rds")
m3=readRDS(file = "m3.rds")
m4=readRDS(file = "m4.rds")
m5=readRDS(file = "m5.rds")


###############################################################################################
###  Table S3. Growth Mixture Models Fit Statistics By Trajectory Class and Model (N=3,142) ###
###############################################################################################

tables3_p1 <- as.data.frame(summarytable(m1,m2,m3,m4,m5,which=c("G","SABIC","%class")))
tables3_p1$period <- 1


# generate the class membership for each county
# write.csv(m5$pprob[,c(1,2)], file="class_period1.csv")

# generate predict outcome and save the values

data_pred=data.frame(t=seq(min(ir_2$t),max(ir_2$t)))

pred_m3=predictY(m3,data_pred);pred_m3$pred=exp(pred_m3$pred)
pred_m4=predictY(m4,data_pred);pred_m4$pred=exp(pred_m4$pred)
pred_m5=predictY(m5,data_pred);pred_m5$pred=exp(pred_m5$pred)

saveRDS(pred_m3,"pred_m3_t1.rds")
saveRDS(pred_m4,"pred_m4_t1.rds")
saveRDS(pred_m5,"pred_m5_t1.rds")

###############
# Period-2
###############

rm(list=ls())

outpath <- ""

setwd("/\\t2")


ir<-read.csv("Ir_v2_1.csv",header=TRUE)
ir$FIPS<-as.factor(ir$FIPS_CODE)

# log transform the outcome  
ir_2=ir
ir_2$y=log(ir_2$ci100000+0.1)
ir_2$t=ir_2$weekn


# GMM fitted number of classes from 1 to 6

m1=lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=1,maxiter = 500, data=ir_2, link='splines')
# saveRDS(m1,"m1.rds")

m2=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=2,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T, link='splines'),rep=30,maxiter=10,minit=m1)
# saveRDS(m2,"m2.rds")

m3=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE",ng=3,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 500, link='splines'),rep=30,maxiter=50,minit=m1)
# saveRDS(m3,"m3.rds")

m4=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=4,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1)
# saveRDS(m4,"m4.rds")

m5=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=5,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1)
# saveRDS(m5,"m5.rds")

m6=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE",ng=6,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1)
# saveRDS(m6,"m6.rds")

getwd()

m1=readRDS(file = "m1.rds")
m2=readRDS(file = "m2.rds")
m3=readRDS(file = "m3.rds")
m4=readRDS(file = "m4.rds")
m5=readRDS(file = "m5.rds")
m6=readRDS(file = "m6.rds")

###############################################################################################
###  Table S3. Growth Mixture Models Fit Statistics By Trajectory Class and Model (N=3,142) ###
###############################################################################################

tables3_p2 <- as.data.frame(summarytable(m1,m2,m3,m4,m5, which=c("G","SABIC","%class")))
tables3_p2$period <- 2


# generate the class membership for each county
# write.csv(m4$pprob[,c(1,2)], file="class_period2.csv")

# generate predict outcome and save the values

data_pred=data.frame(t=seq(min(ir_2$t),max(ir_2$t)))

pred_m4=predictY(m4,data_pred)
pred_m4$pred=exp(pred_m4$pred)

saveRDS(pred_m4,"pred_m4_t2.rds")

###############
# Period-3
###############


rm(list=ls())

outpath <- ""

setwd("/\\t3")

ir<-read.csv("Ir_v3_1.csv",header=TRUE)  # 41656*8
ir$FIPS<-as.factor(ir$FIPS_CODE)

# log transform the outcome  
ir_2=ir
ir_2$y=log(ir_2$ci100000+0.1)
ir_2$t=ir_2$weekn


# GMM fitted number of classes from 1 to 6

m1=lcmm(y~poly(t,degree=2,raw=TRUE), random=~t,subject="FIPS_CODE", ng=1,maxiter = 500, data=ir_2, link='splines')
# saveRDS(m1,"m1.rds")

m2=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE",ng=2,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m2,"m2.rds")

m3=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t, subject="FIPS_CODE", ng=3,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 500, link='splines'),rep=30,maxiter=50,minit=m1, cl = 4)
# saveRDS(m3,"m3.rds")

m4=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t,subject="FIPS_CODE", ng=4,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m4,"m4.rds")

m5=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE), random=~t,  subject="FIPS_CODE",ng=5,data=ir_2, mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m5,"m5.rds")

m6=gridsearch(lcmm(y~poly(t,degree=2,raw=TRUE),  random=~t, subject="FIPS_CODE", ng=6,data=ir_2,  mixture=~poly(t,degree=2,raw=TRUE), 
                   nwg=T,maxiter = 5000, link='splines'),rep=30,maxiter=10,minit=m1, cl = 4)
# saveRDS(m6,"m6.rds")

getwd()

m1=readRDS(file = "m1.rds")
m2=readRDS(file = "m2.rds")
m3=readRDS(file = "m3.rds")
m4=readRDS(file = "m4.rds")
m5=readRDS(file = "m5.rds")
m6=readRDS(file = "m6.rds")


###############################################################################################
###  Table S3. Growth Mixture Models Fit Statistics By Trajectory Class and Model (N=3,142) ###
###############################################################################################

tables3_p3<-as.data.frame(summarytable(m1,m2,m3,m4,m5, which=c("G","SABIC","%class")))
tables3_p3$period <- 3

# generate the class membership for each county
# write.csv(m4$pprob[,c(1,2)], file="class_period3.csv")

# generate predict outcome and save the values

data_pred=data.frame(t=seq(min(ir_2$t),max(ir_2$t)))

pred_m4=predictY(m4,data_pred,draws=TRUE)
colnames(pred_m4$pred)[1:4] <- c("Ypred_class1","Ypred_class2","Ypred_class3",
                                 "Ypred_class4")
pred_m4$pred=exp(pred_m4$pred)

saveRDS(pred_m4,"pred_m4_t3.rds")


# 
tables3 <- rbind(tables3_p1,tables3_p2,tables3_p3)

write.csv(tables3, paste0(outpath,"tableS3.csv"),
          row.names=FALSE)


################################################################
################################################################
################################################################




################################################################
#  model fit plot and tables in supplement
################################################################

library(ggplot2)
library(ggpubr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(lcmm)


###############
# Period-1
###############

rm(list=ls())

outpath <- ""

setwd("/\\t1")

ir<-read.csv("Ir_v1_1.csv",header=TRUE)
ir$FIPS<-as.factor(ir$FIPS_CODE)
ir_2=ir
ir_2$y=log(ir_2$ci100000+0.1)
ir_2$t=ir_2$weekn

dim(ir_2) # 40530 * 10

# 3/31/25 5-class model
m5=readRDS(file = "m5.rds")

# re-class based on paper (we have the color theme)
# 1 -> 1
# 4 -> 2
# 2 -> 3
# 5 -> 4
# 3 -> 5


###############################################################################################
#### Table S4a. Posterior Probabilities - Period 1: March 15 - June 14, 2020 (N=3,142)     ####
###############################################################################################

# 1 -> 1
# 4 -> 2
# 2 -> 3
# 5 -> 4
# 3 -> 5

TableS4a <- postprob(m5)[[2]]
t <- cbind(TableS4a[,1],TableS4a[,4],TableS4a[,2],TableS4a[,5],TableS4a[,3])
t <- rbind(t[1,],t[4,],t[2,],t[5,],t[3,])

write.csv(t, paste0(outpath,"TableS4a.csv"))

ir_3 <- merge(ir_2, m5$pprob, by.x="FIPS_CODE", by.y="FIPS_CODE") # dim(ir_3) 40530 * 16

ir_3_ <- ir_3 %>%
  distinct(FIPS_CODE,class,.keep_all=TRUE) %>%
  dplyr::rename(class5=class) %>%
  select(FIPS_CODE,class5)

write.csv(ir_3_,"class_period1.csv",row.names=FALSE)

pred_m5_t1=readRDS(file = "pred_m5_t1.rds")
pred_m5_t1=do.call(data.frame,pred_m5_t1)


# class 1

ir_3_ = ir_3[ir_3$class=="1",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]

p1<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m5_t1, aes(t,pred.Ypred_class1),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("a. Class 1")+
  theme_bw()

# class 2


ir_3_ = ir_3[ir_3$class=="2",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]

p2<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m5_t1, aes(t,pred.Ypred_class2),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("c. Class 3")+
  theme_bw()

# class 3

ir_3_ = ir_3[ir_3$class=="3",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]

p3<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m5_t1, aes(t,pred.Ypred_class3),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("e. Class 5")+
  theme_bw()

# class 4

ir_3_ = ir_3[ir_3$class=="4",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]

p4<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m5_t1, aes(t,pred.Ypred_class4),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("b. Class 2")+
  theme_bw()

# class 5

ir_3_ = ir_3[ir_3$class=="5",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]

p5<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m5_t1, aes(t,pred.Ypred_class5),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("d. Class 4")+
  theme_bw()

####################################################################################################
#### Figure S1a. Predicted Marginal Trajectories - Period 1: March 15 - June 14, 2020 (N=3,142) ####
####################################################################################################


tiff(paste0(outpath,"Fit_plot_Period1.tiff"),
     compression = "lzw", units="in",  width=8,   height=5,  res=1200)

ggarrange(p1,p4,p2,p5,p3,
          ncol=3,nrow=2)

dev.off()


###############
# Period-2
###############


rm(list=ls())

outpath <- ""

setwd("/\\t2")


ir<-read.csv("Ir_v2_1.csv",header=TRUE)
ir$FIPS<-as.factor(ir$FIPS_CODE)
ir_2=ir
ir_2$y=log(ir_2$ci100000+0.1)
ir_2$t=ir_2$weekn

dim(ir_2) # 28122 * 10

m4=readRDS(file = "m4.rds")

# 2 -> 1 ->1/2
# 3 -> 2 ->3
# 1 -> 3 ->4
# 4 -> 4 ->5

####################################################################################################
####  Table S4b. Posterior Probabilities - Period 2: June 15 - August 14, 2020 (N=3,142)        ####
####################################################################################################

TableS4b <- postprob(m4)[[2]]
t <- cbind(TableS4b[,2],TableS4b[,3],TableS4b[,1],TableS4b[,4])
t <- rbind(t[2,],t[3,],t[1,],t[4,])

write.csv(t, paste0(outpath,"TableS4b.csv"))

ir_3 <- merge(ir_2, m4$pprob, by.x="FIPS_CODE", by.y="FIPS_CODE") # 28122 * 15
dim(ir_3)


ir_3_ <- ir_3 %>%
  distinct(FIPS_CODE,class,.keep_all=TRUE) %>%
  dplyr::rename(class4=class) %>%
  select(FIPS_CODE,class4)

write.csv(ir_3_,"class_period2.csv",row.names=FALSE)


pred_m4_t2=readRDS(file = "pred_m4_t2.rds")
pred_m4_t2=do.call(data.frame,pred_m4_t2)

# class 1


ir_3_ = ir_3[ir_3$class=="1",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p1<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t2, aes(t,pred.Ypred_class1),col="red",linewidth=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8),
                     labels=c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("c. Class 4")+
  theme_bw()

# class 2


ir_3_ = ir_3[ir_3$class=="2",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p2<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t2, aes(t,pred.Ypred_class2),col="red",linewidth=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8),
                     labels=c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("a. Class 1/2")+
  theme_bw()

# class 3


ir_3_ = ir_3[ir_3$class=="3",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p3<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t2, aes(t,pred.Ypred_class3),col="red",linewidth=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8),
                     labels=c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("b. Class 3")+
  theme_bw()

# class 4


ir_3_ = ir_3[ir_3$class=="4",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p4<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t2, aes(t,pred.Ypred_class4),col="red",linewidth=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8),
                     labels=c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("d. Class 5")+
  theme_bw()

# 2 -> 1 ->1/2
# 3 -> 2 ->3
# 1 -> 3 ->4
# 4 -> 4 ->5


####################################################################################################
### Figure S1b. Predicted Marginal Trajectories - Period 2: June 15 - August 14, 2020 (N=3,142) ####
####################################################################################################


tiff(paste0(outpath,"Fit_plot_Period2.tiff"),
     compression = "lzw", units="in",  width=8,   height=5,  res=1200)


ggarrange(p2,p3,p1,p4,
          ncol=2,nrow=2)

dev.off()


###############
# Period-3
###############


rm(list=ls())


outpath <- ""

setwd("/\\t3")

ir<-read.csv("Ir_v3_1.csv",header=TRUE)
ir$FIPS<-as.factor(ir$FIPS_CODE)
ir_2=ir
ir_2$y=log(ir_2$ci100000+0.1)
ir_2$t=ir_2$weekn

dim(ir_2) # 34455 * 10


m4=readRDS(file = "m4.rds")


# 1 -> 1 ->1/2
# 3 -> 2 ->3
# 4 -> 3 ->3
# 2 -> 4 ->5

####################################################################################################
#### Table S4c. Posterior Probabilities - Period 3: August 15 - November 15, 2020 (N=3,142)     ####
####################################################################################################


TableS4c <- postprob(m4)[[2]]
t <- cbind(TableS4c[,1],TableS4c[,3],TableS4c[,4],TableS4c[,2])
t <- rbind(t[1,],t[3,],t[4,],t[2,])

write.csv(t, paste0(outpath,"TableS4c.csv"))


ir_3 <- merge(ir_2, m4$pprob, by.x="FIPS_CODE", by.y="FIPS_CODE") # 34455 * 15
dim(ir_3)


ir_3_ <- ir_3 %>%
  distinct(FIPS_CODE,class,.keep_all=TRUE) %>%
  dplyr::rename(class4=class) %>%
  select(FIPS_CODE,class4)

write.csv(ir_3_,"class_period3.csv",row.names=FALSE)


pred_m4_t3=readRDS(file = "pred_m4_t3.rds")
pred_m4_t3=do.call(data.frame,pred_m4_t3)


# class 1


ir_3_ = ir_3[ir_3$class=="1",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p1<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t3, aes(t,pred.Ypred_class1),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("a. Class 1/2")+
  theme_bw()

# class 2

ir_3_ = ir_3[ir_3$class=="2",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p2<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t3, aes(t,pred.Ypred_class2),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("d. Class 5")+
  theme_bw()

# class 3

ir_3_ = ir_3[ir_3$class=="3",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p3<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t3, aes(t,pred.Ypred_class3),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("b. Class 3")+
  theme_bw()

# class 4

ir_3_ = ir_3[ir_3$class=="4",]
id = sample(unique(ir_3_$FIPS_CODE),100)
ir_3_1 = ir_3_[ir_3_$FIPS_CODE %in% id,]


p4<-ggplot()+
  geom_line(data=ir_3_1, aes(x=t,y=ci100000,group=FIPS_CODE),col="grey")+
  geom_line(data=pred_m4_t3, aes(t,pred.Ypred_class4),col="red",size=2)+
  scale_x_continuous(name="Week",
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                     labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  scale_y_continuous(name="IR/100,000",
                     breaks=c(0,100,200,300,400,500),
                     limits=c(0,500))+
  ggtitle("c. Class 4")+
  theme_bw()

# 1 -> 1->1/2
# 3 -> 2->3
# 4 -> 3->4
# 2 -> 4->5


###########################################################################################################
####  Figure S1c. Predicted Marginal Trajectories - Period 3: August 15 – November 15, 2020 (N=3,142)  ####
###########################################################################################################


tiff(paste0(outpath,"Fit_plot_Period3.tiff"),
     compression = "lzw", units="in",  width=8,   height=5,  res=1200)

ggarrange(p1,p3,p4,p2,
          ncol=2,nrow=2)

dev.off()
