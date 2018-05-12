getwd()
setwd("G:\\jigsaw\\CAPSTOE PROJECT")
shadata<-read.csv("telecomfinal.csv", stringsAsFactors = T)
library(dplyr)

names(shadata)
str(shadata)
summary(shadata) 



##%%%  DATA QUALITY REPORT (qrep) %%%##

#column names (variables)
column<-names(shadata)
qrep<-as.data.frame(column)
rm(column)

#Recording Data Type for each Variable
qrep$DataType<-sapply(shadata,class)

#No. of Records for each Variable
qrep$TotalNo.ofRecords<-nrow(shadata)

#No. of Unique Values variables
for(i in 1:ncol(shadata))
{
  qrep$UniqueValues[i]<-length(unique(shadata[,i]))
}

#No.of observations available and percentage  in variables
qrep$DataAvailable<-colSums(!is.na(shadata))
qrep$AvailablePercentage<-round(colMeans(!is.na(shadata)),4)

#Total No of Na Values for each Variable
qrep$Missing<-colSums(is.na(shadata))

#percentage of Na values
qrep$MissingPercentage<-round(colMeans(is.na(shadata)),4)

#Minimum, Mean, Quantile, Maximum Values for each Variable

for(i in 1:ncol(shadata))
{
  qrep$Minimum[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",min(shadata[,i],na.rm=T),0),2)
  qrep$Maximum[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",max(shadata[,i],na.rm=T),0),2)
  qrep$Mean[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",mean(shadata[,i],na.rm=T),0),2)
  qrep$fifthPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.05,na.rm=T),0),2)
  qrep$tenthPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.10,na.rm=T),0),2)
  qrep$twentyfifthPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.25,na.rm=T),0),2)
  qrep$fiftythPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.50,na.rm=T),0),2)
  qrep$seventyfifthPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.75,na.rm=T),0),2)
  qrep$ninetythPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.90,na.rm=T),0),2)
  qrep$ninetyfifthPercentile[i]<-round(ifelse(class(shadata[,i])=="integer"|class(shadata[,i])=="numeric",quantile(shadata[,i],p=0.95,na.rm=T),0),2)
}

str(qrep)

#Writing Data Quality Report into a file
write.csv(qrep,"Data Quality Report shahad.csv",row.names = T)


#Ommitting variables with more than 48% missing values and creating a new data set
shaddata1<-shadata[,colMeans(is.na(shadata))<=0.48]

names(shaddata1)




##%%%%  Data Exploration  %%%%##
## --- Profiling Continuous Variables (dat) ---##


## Deciling Continuous Variables on the Basis of Target Variabe "Churn"##

names(shaddata1)
str(shaddata1)

# <1>column 'mou_Mean'
summary(shaddata1$mou_Mean)
shaddata1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(shaddata1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-round(dat1$n/dat1$N,2)
dat1$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))
dat1
# <2> column "totmrc_Mean" 
summary(shaddata1$totmrc_Mean)
shaddata1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(shaddata1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
dat2


# <3> column "rev_Range" 
summary(shaddata1$rev_Range)
shaddata1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(shaddata1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))
dat3

# <4> column "mou_Range" 
summary(shaddata1$mou_Range)
shaddata1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(shaddata1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))                  
dat4

# <5> column "change_mou" 
summary(shaddata1$change_mou)
shaddata1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(shaddata1%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))    
dat5

# <6> column "drop_blk_Mean" 
shaddata1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(shaddata1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6)) 
dat6

# <7> column "drop_vce_Range" 
summary(shaddata1$drop_vce_Range)
shaddata1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(shaddata1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7)) 
dat7

# <8> column "owylis_vce_Range" 
summary(shaddata1$owylis_vce_Range)
shaddata1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(shaddata1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))
dat8

# <9> column "mou_opkv_Range" 
summary(shaddata1$mou_opkv_Range)
shaddata1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(shaddata1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))
dat9

# <10> column "months" 
summary(shaddata1$months)
shaddata1%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(shaddata1%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))
dat10

# <11> column "totcalls" 
summary(shaddata1$totcalls)
shaddata1%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(shaddata1%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))
dat11

# <12> column "eqpdays"
summary(shaddata1$eqpdays)

#my data, move to NA treatment lines #Missing Value Treatment - Since there is just 1 missing observation, will remove the same.
index<-which(is.na(shaddata1$eqpdays))
shaddata1<-shaddata1[-index,]

#Deciling basis Variable churn 
shaddata1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(shaddata1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))
dat12

# <13> column "custcare_Mean"----Getting less than 4 deciles,so Omitting the data as it won't be significant
summary(shaddata1$custcare_Mean)
shaddata1%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$varname<-rep("custcare_Mean",nrow(dat13))
dat13
plot(shaddata1$churn,shaddata1$custcare_Mean, col="red")



# <14> column "callwait_Mean"
summary(shaddata1$callwait_Mean)
shaddata1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(shaddata1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))
dat14

# <15> column "iwylis_vce_Mean"
summary(shaddata1$iwylis_vce_Mean)
shaddata1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(shaddata1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))
dat15

# <16> column "callwait_Range"----Getting less than 4 deciles,so Omitting the data as it won't be significant
summary(shaddata1$callwait_Range)
shaddata1%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("callwait_Range",nrow(dat16))
dat16



# <17> column "ccrndmou_Range"----Getting less than 4 deciles,so Omitting the data as it won't be significant---
summary(shaddata1$ccrndmou_Range)
shaddata1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17


# <18> column "adjqty"
summary(shaddata1$adjqty)
shaddata1%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(shaddata1%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))
dat18

# <19> column "ovrrev_Mean"
summary(shaddata1$ovrrev_Mean)
shaddata1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(shaddata1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))
dat19

# <20> column "rev_Mean"
summary(shaddata1$rev_Mean)
shaddata1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(shaddata1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))
dat20

# <21> column "ovrmou_Mean"
summary(shaddata1$ovrmou_Mean)
shaddata1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(shaddata1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))
dat21

# <22> column "comp_vce_Mean" ### my data check why----Data Transformation then Delete ---
summary(shaddata1$comp_vce_Mean)
shaddata1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(shaddata1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))
dat22

# <23> column "plcd_vce_Mean" ### 
summary(shaddata1$plcd_vce_Mean)
shaddata1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(shaddata1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))
dat23

# <24> column "avg3mou"
summary(shaddata1$avg3mou)
shaddata1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(shaddata1%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))
dat24

# <25> column "avgmou"
summary(shaddata1$avgmou)
shaddata1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(shaddata1%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))
dat25

# <26> column "avg3qty"
summary(shaddata1$avg3qty)
shaddata1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(shaddata1%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))
dat26

# <27> column "avgqty"
summary(shaddata1$avgqty)
shaddata1%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(shaddata1%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))
dat27

# <28> column "avg6mou"
summary(shaddata1$avg6mou)
shaddata1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(shaddata1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))
dat28

# <29> column "avg6qty"
summary(shaddata1$avg6qty)
shaddata1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(shaddata1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))
dat29

# <30> column "age1" ----Later Use As Factor-----
summary(shaddata1$age1)
shaddata1%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(shaddata1%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat30$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat30$varname<-rep("age1",nrow(dat30))
dat30

# <31> column "age2"----Later Use As Factor-----
summary(shaddata1$age2)
shaddata1%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$varname<-rep("age2",nrow(dat31))
dat31

# <32> column "models" ----Later Use As Factor-----
summary(shaddata1$models)
shaddata1%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$varname<-rep("models",nrow(dat32))
dat32

# <33> column "hnd_price" ----Later Use As Factor-----
summary(shaddata1$hnd_price)
shaddata1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(shaddata1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat33$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat33$varname<-rep("hnd_price",nrow(dat33))
dat33

# <34> column "actvsubs"----Later Use As Factor-----
summary(shaddata1$actvsubs)
shaddata1%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$varname<-rep("actvsubs",nrow(dat34))
dat34

# <35> column "uniqsubs" ----Later Use As Factor-----
summary(shaddata1$uniqsubs)
shaddata1%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$varname<-rep("uniqsubs",nrow(dat35))
dat35

# <36> column "forgntvl" ----Later Use As Factor-----
summary(shaddata1$forgntvl)
shaddata1%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("forgntvl",nrow(dat36))
dat36

# <37> column "opk_dat_Mean" ---- Omit as it shows less than 3 quantiles----
summary(shaddata1$opk_dat_Mean)
shaddata1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("opk_dat_Mean",nrow(dat37))
dat37

# <38> column "mtrcycle" ===>> ----Later Use As Factor-----
summary(shaddata1$mtrcycle)
shaddata1%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("mtrcycle",nrow(dat38))
dat38

# <39> column "truck" ----Later Use As Factor-----
summary(shaddata1$truck)
shaddata1%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("truck",nrow(dat39))
dat39

# <40> column "roam_Mean" ----Getting less than 4 deciles,so Omitting the data as it won't be significant---
summary(shaddata1$roam_Mean)
shaddata1%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$varname<-rep("roam_Mean",nrow(dat40))
dat40

# <41> column "recv_sms_Mean" ----Getting less than 4 deciles,so Omitting the data as it won't be significant---
summary(shaddata1$recv_sms_Mean)
shaddata1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$varname<-rep("recv_sms_Mean",nrow(dat41))
dat41

# <42> column "mou_pead_Mean" ----Getting less than 4 deciles,so Omitting the data as it won't be significant---
summary(shaddata1$mou_pead_Mean)
shaddata1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$varname<-rep("mou_pead_Mean",nrow(dat42))
dat42

# <43> column "da_Mean"
summary(shaddata1$da_Mean)
shaddata1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(shaddata1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat43$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat43$varname<-rep("da_Mean",nrow(dat43))
dat43

# <44> column "da_Range"
summary(shaddata1$da_Range)
shaddata1%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(shaddata1%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat44$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat44$varname<-rep("da_Range",nrow(dat44))
dat44

# <45> column "datovr_Mean" ----Getting less than 4 deciles,so Omitting the data as it won't be significant---
summary(shaddata1$datovr_Mean)
shaddata1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$varname<-rep("datovr_Mean",nrow(dat45))
dat45

# <46> column "datovr_Range" ----Getting less than 4 deciles,so Omitting the data as it won't be significant---
summary(shaddata1$datovr_Range)
shaddata1%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$varname<-rep("datovr_Range",nrow(dat46))
dat46

# <47> column "drop_dat_Mean" ----Getting less than 4 deciles,so Omitting the data as it won't be significant--- 

summary(shaddata1$drop_dat_Mean)
shaddata1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$varname<-rep("drop_dat_Mean",nrow(dat47))
dat47

# <48> column "drop_vce_Mean" 
summary(shaddata1$drop_vce_Mean)
shaddata1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$N<-unclass(shaddata1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat48$churn_perc<-dat48$n/dat48$N
dat48$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat48$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat48$varname<-rep("drop_vce_Mean",nrow(dat48))
dat48

# <49> column "adjmou" 
summary(shaddata1$adjmou)
shaddata1%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(shaddata1%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat49$churn_perc<-dat49$n/dat49$N
dat49$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat49$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat49$varname<-rep("adjmou",nrow(dat49))
dat49

# <50> column "totrev"
summary(shaddata1$totrev)
shaddata1%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(shaddata1%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat50$churn_perc<-dat50$n/dat50$N
dat50$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat50$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat50$varname<-rep("totrev",nrow(dat50))
dat50

# <51> column "adjrev" 
summary(shaddata1$adjrev)
shaddata1%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(shaddata1%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat51$churn_perc<-dat51$n/dat51$N
dat51$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat51$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat51$varname<-rep("adjrev",nrow(dat51))
dat51

# <52> column "avgrev" 
summary(shaddata1$avgrev)
shaddata1%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$N<-unclass(shaddata1%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat52$churn_perc<-dat52$n/dat52$N
dat52$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat52$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat52$varname<-rep("avgrev",nrow(dat52))
dat52

# <100> column "blck_dat_Mean" 
summary(shaddata1$blck_dat_Mean)
shaddata1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat100
dat100$N<-unclass(shaddata1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat100$churn_perc<-dat100$n/dat100$N
dat100$GreaterThan<-unclass(shaddata1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat100$LessThan<-unclass(shaddata1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat100$varname<-rep("blck_dat_Mean",nrow(dat100))
dat100



#Adding all continuous variable columns to create a deciled databind object to save
databind<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,dat14,dat15,dat18,dat19,
                dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat33,dat43,dat44,
                dat48,dat49,dat50,dat51,dat52)

#writting all deciled variables "Deciled Continuous variables to use in glm"
write.csv(databind,"Deciled Continuous variables to use in glm.csv",row.names = F)


#Deleting all Variables that is not able to decile ie it will come insignificant in the model

names(shaddata1)
shaddata1<-shaddata1[,-c(14,17,18,46,47,48,51,52,53,60,61,62)]
names(shaddata1)

## --- Profiling Catagorical Variables (datC) ---##


## Deciling Catagorical Variables on the Basis of Target Variabe "Churn"##

# <53> column "crclscod"  
summary(shaddata1$crclscod)
shaddata1%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC53
datC53$N<-unclass(shaddata1%>%filter(crclscod%in%datC53$levels)%>%count(crclscod))[[2]]
datC53$ChurnPerc<-datC53$n/datC53$N
datC53$Var.Name<-rep("crclscod",nrow(datC53))
datC53

# <54> column "asl_flag"  
summary(shaddata1$asl_flag)
shaddata1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC54
datC54$N<-unclass(shaddata1%>%filter(asl_flag%in%datC54$levels)%>%count(asl_flag))[[2]]
datC54$ChurnPerc<-datC54$n/datC54$N
datC54$Var.Name<-rep("asl_flag",nrow(datC54))
datC54

# <55> column "prizm_social_one"  
summary(shaddata1$prizm_social_one)
shaddata1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC55
datC55$N<-unclass(shaddata1%>%filter(prizm_social_one%in%datC55$levels)%>%count(prizm_social_one))[[2]]
datC55$ChurnPerc<-datC55$n/datC55$N
datC55$Var.Name<-rep("prizm_social_one",nrow(datC55))
datC55

# <56> column "area"  
summary(shaddata1$area)
shaddata1%>%count(churn,levels=area)%>%filter(churn==1)->datC56
datC56$N<-unclass(shaddata1%>%filter(area%in%datC56$levels)%>%count(area))[[2]]
datC56$ChurnPerc<-datC56$n/datC56$N
datC56$Var.Name<-rep("area",nrow(datC56))
datC56

# <57> column "refurb_new"  
summary(shaddata1$refurb_new)
shaddata1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC57
datC57$N<-unclass(shaddata1%>%filter(refurb_new%in%datC57$levels)%>%count(refurb_new))[[2]]
datC57$ChurnPerc<-datC57$n/datC57$N
datC57$Var.Name<-rep("refurb_new",nrow(datC57))
datC57

# <58> column "hnd_webcap"  
summary(shaddata1$hnd_webcap)
shaddata1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC58
datC58$N<-unclass(shaddata1%>%filter(hnd_webcap%in%datC58$levels)%>%count(hnd_webcap))[[2]]
datC58$ChurnPerc<-datC58$n/datC58$N
datC58$Var.Name<-rep("hnd_webcap",nrow(datC58))
datC58

# <59> column "marital"  
summary(shaddata1$marital)
shaddata1%>%count(churn,levels=marital)%>%filter(churn==1)->datC59
datC59$N<-unclass(shaddata1%>%filter(marital%in%datC59$levels)%>%count(marital))[[2]]
datC59$ChurnPerc<-datC59$n/datC59$N
datC59$Var.Name<-rep("marital",nrow(datC59))
datC59

# <60> column "ethnic"  
summary(shaddata1$ethnic)
shaddata1%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC60
datC60$N<-unclass(shaddata1%>%filter(ethnic%in%datC60$levels)%>%count(ethnic))[[2]]
datC60$ChurnPerc<-datC60$n/datC60$N
datC60$Var.Name<-rep("ethnic",nrow(datC60))
datC60

# <61> column "car_buy"  
summary(shaddata1$car_buy)
shaddata1%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC61
datC61$N<-unclass(shaddata1%>%filter(car_buy%in%datC61$levels)%>%count(car_buy))[[2]]
datC61$ChurnPerc<-datC61$n/datC61$N
datC61$Var.Name<-rep("car_buy",nrow(datC61))
datC61

# <62> column "csa" ===>>> **** Some Levels show less than 5% churn rate. So Omit as will come insignificant **** 
summary(shaddata1$csa)
shaddata1%>%count(churn,levels=csa)%>%filter(churn==1)->datC62
datC62$N<-unclass(shaddata1%>%filter(csa%in%datC62$levels)%>%count(csa))[[2]]
datC62$ChurnPerc<-datC62$n/datC62$N
datC62$Var.Name<-rep("csa",nrow(datC62))
datC62

# <71> column "dwlltype"  # while imputing NA, 20823 observation will be imputed with M, so data will be biased, so delete in second iteration
summary(shaddata1$dwlltype)
shaddata1%>%count(churn,levels=dwlltype)%>%filter(churn==1)->datC71
datC71$N<-unclass(shaddata1%>%filter(dwlltype%in%datC71$levels)%>%count(dwlltype))[[2]]
datC71$ChurnPerc<-datC71$n/datC71$N
datC71$Var.Name<-rep("dwlltype",nrow(datC71))
datC71

# <72> column "dwllsize"  # while imputing NA, 24990 observation will be imputed with M, so data will be biased, so delete in second iteration
summary(shaddata1$dwllsize)
shaddata1%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC72
datC72$N<-unclass(shaddata1%>%filter(dwllsize%in%datC72$levels)%>%count(dwllsize))[[2]]
datC72$ChurnPerc<-datC72$n/datC72$N
datC72$Var.Name<-rep("dwllsize",nrow(datC72))
datC72



# Using continuous variables like age2, models, actvsubs, uniqsubs, forgntvl, mtrcycle, truck and hnd_price as factor variables 

# <63> column "age2"  
summary(shaddata1$age2)
shaddata1%>%count(churn,levels=age2)%>%filter(churn==1)->datC63
datC63$N<-unclass(shaddata1%>%filter(age2%in%datC63$levels)%>%count(age2))[[2]]
datC63$ChurnPerc<-datC63$n/datC63$N
datC63$Var.Name<-rep("age2",nrow(datC63))
datC63

# <64> column "models"  
summary(shaddata1$models)
shaddata1%>%count(churn,levels=models)%>%filter(churn==1)->datC64
datC64$N<-unclass(shaddata1%>%filter(models%in%datC64$levels)%>%count(models))[[2]]
datC64$ChurnPerc<-datC64$n/datC64$N
datC64$Var.Name<-rep("models",nrow(datC64))
datC64

# <65> column "actvsubs"  
summary(shaddata1$actvsubs)
shaddata1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC65
datC65$N<-unclass(shaddata1%>%filter(actvsubs%in%datC65$levels)%>%count(actvsubs))[[2]]
datC65$ChurnPerc<-datC65$n/datC65$N
datC65$Var.Name<-rep("actvsubs",nrow(datC65))
datC65

# <66> column "uniqsubs"  
summary(shaddata1$uniqsubs)
shaddata1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC66
datC66$N<-unclass(shaddata1%>%filter(uniqsubs%in%datC66$levels)%>%count(uniqsubs))[[2]]
datC66$ChurnPerc<-datC66$n/datC66$N
datC66$Var.Name<-rep("uniqsubs",nrow(datC66))
datC66

# <67> column "forgntvl"  
summary(shaddata1$forgntvl)
shaddata1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC67
datC67$N<-unclass(shaddata1%>%filter(forgntvl%in%datC67$levels)%>%count(forgntvl))[[2]]
datC67$ChurnPerc<-datC67$n/datC67$N
datC67$Var.Name<-rep("forgntvl",nrow(datC67))
datC67

# <68> column "mtrcycle"  
summary(shaddata1$mtrcycle)
shaddata1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC68
datC68$N<-unclass(shaddata1%>%filter(mtrcycle%in%datC68$levels)%>%count(mtrcycle))[[2]]
datC68$ChurnPerc<-datC68$n/datC68$N
datC68$Var.Name<-rep("mtrcycle",nrow(datC68))
datC68

# <69> column "Truck"  
summary(shaddata1$truck)
shaddata1%>%count(churn,levels=truck)%>%filter(churn==1)->datC69
datC69$N<-unclass(shaddata1%>%filter(truck%in%datC69$levels)%>%count(truck))[[2]]
datC69$ChurnPerc<-datC69$n/datC69$N
datC69$Var.Name<-rep("Truck",nrow(datC69))
datC69

# <70> column "hnd_price"  
summary(shaddata1$hnd_price)
shaddata1%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC70
datC70$N<-unclass(shaddata1%>%filter(hnd_price%in%datC70$levels)%>%count(hnd_price))[[2]]
datC70$ChurnPerc<-datC70$n/datC70$N
datC70$Var.Name<-rep("hnd_price",nrow(datC70))
datC70


#Adding all Catagorical variable columns to create a deciled databindC_1 and databindC_2 object to save

databindC_1<-rbind(datC53,datC54,datC55,datC56,datC57,datC58,datC59,datC60,datC61,
                   datC62)

databindC_2<-rbind(datC63,datC64,datC65,datC66,datC67,datC68,datC69,datC70)


#writting all deciled catagorical variables 
write.csv(datC_1,"Categorical variables deciled 1.csv",row.names = F)
write.csv(datC_2,"Categorical variables deciled 2.csv",row.names = F)


#Removing columns with levels less than 5% churn rate as will come insignificant
names(shaddata1)
shaddata1<-shaddata1[,-48]
names(shaddata1)


#####DATA PREPERATION########

#-----Outlier Treatment For Continuous Variables----#


#Outlier Treatment Using Box Plot Method
names(shaddata1)
summary(shaddata1)
str(shaddata1)



str(shaddata1)
list<-names(shaddata11)
list

list<-list[-c(25:40,42,43,51)]
list
par(mfrow=c(3,11))
# Outlier Plots
par(mfrow=c(2,11))
for(i in 1:length(list))
{
  boxplot(shaddata11[,list[i]],main=list[i])
}

for(i in 1:length(list))
{
  plot(shaddata11[,list[i]],main=list[i])
}


# Outlier Treatment
#imputing outlier in mou_mean

box<-boxplot(shaddata1$mou_Mean)
str(box$out)

library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(shaddata1, "mou_Mean", which(shaddata1$mou_Mean > 2000),2000)

box<-boxplot(shaddata1$mou_Mean)#counter cheking

#imputing outlier in avg6mou


box<-boxplot(shaddata1$avg6mou)
str(box$out)


outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(shaddata1, "avg6mou", which(shaddata1$avg6mou > 4000),4000)

box<-boxplot(shaddata1$avg6mou)#counter cheking



#imputing outlier in actvsubs


box<-boxplot(shaddata1$uniqsubs)
str(box$out)


outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(shaddata1, "uniqsubs", which(shaddata1$uniqsubs > 9),4)

box<-boxplot(shaddata1$uniqsubs)#counter cheking


#imputing outlier in hnd_price


box<-boxplot(shaddata1$hnd_price)
str(box$out)


outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(shaddata1, "hnd_price", which(shaddata1$avg6qty > 250),250)

box<-boxplot(shaddata1$hnd_price)#counter cheking



#imputing outlier in hnd_price
summary(shaddata1$actvsubs)

box<-boxplot(shaddata1$actvsubs)
str(box$out)


outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(shaddata1, "actvsubs", which(shaddata1$avg6qty > 6),5)

box<-boxplot(shaddata1$actvsubs)#counter cheking





# Checking After Treatment

for(i in 1:length(list))
{
  boxplot(shaddata1[,list[i]],main=list[i])
}

for(i in 1:length(list))
{
  plot(shaddata1[,list[i]],main=list[i])
}

dev.off()



#-----Missing Value Treatment -------# 

summary(shaddata1)
names(tele)

# Factor Variables=> crclscod, asl_flag, prizm_social_one, area, refurb_new, hnd_webcap, marital, ethnic, "age1", 
#    "age2", "models", "hnd_price","actvsubs", "uniqsubs", "forgntvl", "mtrcycle", "truck", car_buy, csa retdays_1


# Deleting Missing Values of mou_Mean, totmrc_Mean, rev_Range, mou_Range"
index1<-which(is.na(shaddata1[,c(1:5)]))
shaddata1<-shaddata1[-index1,]
summary(shaddata1)

index2<-which(is.na(shaddata1$change_mou))
shaddata1<-shaddata1[-index2,]

index3<-which(is.na(shaddata1$area))
shaddata1<-shaddata1[-index3,]

index4<-which(is.na(shaddata1$eqpdays))
shaddata1<-shaddata1[-index4,]

index5<-which(is.na(shaddata1$forgntvl))
shaddata1<-shaddata1[-index5,]

summary(shaddata1)


# Imputation with mean or corresponding quantiles

shaddata1$marital[is.na(shaddata1$marital)]<-"S"

shaddata1$ethnic[is.na(shaddata1$ethnic)]<-"M"

shaddata1$truck[is.na(shaddata1$truck)]<-1

shaddata1$car_buy[is.na(shaddata1$car_buy)]<-"UNKNOWN"


shaddata1$avg6mou[is.na(shaddata1$avg6mou)]<-quantile(shaddata1$avg6mou,prob=0.90,na.rm = TRUE)

shaddata1$avg6qty[is.na(shaddata1$avg6qty)]<-quantile(shaddata1$avg6qty,prob=0.90,na.rm = TRUE)

shaddata1$hnd_price[is.na(shaddata1$hnd_price)]<-mean(shaddata1$hnd_price,na.rm = T)

shaddata1$income[is.na(shaddata1$income)]<-mean(shaddata1$income,na.rm = T)

summary(shaddata1)


# Creating seperate category "Missing" for Factor Variables

#Variable prizm_social_one#substitute with s
shaddata1$prizm_social_one_1<-ifelse(is.na(shaddata1$prizm_social_one),"Missing",as.factor(shaddata1$prizm_social_one))
str(shaddata1$prizm_social_one_1)
shaddata1$prizm_social_one_1<-as.factor(shaddata1$prizm_social_one_1)
summary(shaddata1$prizm_social_one)
summary(shaddata1$prizm_social_one_1)
shaddata1$prizm_social_one_1<-factor(shaddata1$prizm_social_one_1,labels =c("C","R","S","T","U","Missing"))
summary(shaddata1$prizm_social_one_1)

names(shaddata1)
#Deleting prizm_social_one
shaddata1<-shaddata1[,-30]
summary(shaddata1)



#Variable hnd_webcap
shaddata1$hnd_webcap_1<-ifelse(is.na(shaddata1$hnd_webcap),"Missing",as.factor(shaddata1$hnd_webcap))
str(shaddata1$hnd_webcap_1)
shaddata1$hnd_webcap_1<-as.factor(shaddata1$hnd_webcap_1)
summary(shaddata1$hnd_webcap)
summary(shaddata1$hnd_webcap_1)
shaddata1$hnd_webcap_1<-factor(shaddata1$hnd_webcap_1,labels =c("UNKW","WC","WCMB","Missing"))
summary(shaddata1$hnd_webcap_1)

names(shaddata1)
shaddata1<-shaddata1[,-32]
summary(shaddata1)



#Checking Churn Rate in the data after oulier and NA Imputations
table(shadata$churn)/nrow(shadata)
table(shaddata1$churn)/nrow(shaddata1)



# Converting age1, age2, models, hnd_price, actvsubs,uniqsubs, forgntvl, mtrcycle, truck
# into Factor variable and Creating Dummy Variables 


#age1
str(shaddata1$age1)
shaddata1$age1_1<-ifelse(shaddata1$age1==0,"Default",ifelse(shaddata1$age1<=30,"Young",
                                                            ifelse(shaddata1$age1>30 & shaddata1$age1<=55,"Mid Age","Old")))
str(shaddata1$age1_1)
shaddata1$age1_1<-as.factor(shaddata1$age1_1)
summary(shaddata1$age1_1)

names(shaddata1)
shaddata1<-shaddata1[,-34]
summary(shaddata1)

#age2

str(shaddata1$age2)
shaddata1$age2_1<-ifelse(shaddata1$age2==0,"Default",ifelse(shaddata1$age2<=30,"Young",
                                                            ifelse(shaddata1$age2>30 & shaddata1$age2<=55,"Mid Age","Old")))
str(shaddata1$age2_1)
shaddata1$age2_1<-as.factor(shaddata1$age2_1)
summary(shaddata1$age2_1)

names(shaddata1)
shaddata1<-shaddata1[,-34]
summary(shaddata1)

#income

table(shaddata1$income)
shaddata1$income_1<-ifelse(shaddata1$income==0,"Default",ifelse(shaddata1$income<=4,"poor",
                                                            ifelse(shaddata1$income>4 & shaddata1$income<=6,"Midile class","Rich")))
str(shaddata1$income_1)
shaddata1$income_1<-as.factor(shaddata1$income_1)
summary(shaddata1$income_1)

names(shaddata1)
shaddata1<-shaddata1[,-12]
summary(shaddata1)



#models
str(shaddata1$models)
summary(shaddata1$models)
shaddata1$models<-as.factor(shaddata1$models)
summary(shaddata1$models)


str(shaddata1$hnd_price)
summary(shaddata1$hnd_price)
shaddata1$hnd_price<-as.factor(shaddata1$hnd_price)
summary(shaddata1$hnd_price)


str(shaddata1$actvsubs)
summary(shaddata1$actvsubs)
shaddata1$actvsubs<-as.factor(shaddata1$actvsubs)
summary(shaddata1$actvsubs)


str(shaddata1$uniqsubs)
summary(shaddata1$uniqsubs)
shaddata1$uniqsubs<-as.factor(shaddata1$uniqsubs)
summary(shaddata1$uniqsubs)


str(shaddata1$forgntvl)
summary(shaddata1$forgntvl)
shaddata1$forgntvl<-as.factor(shaddata1$forgntvl)
summary(shaddata1$forgntvl)


str(shaddata1$mtrcycle)
summary(shaddata1$mtrcycle)
shaddata1$mtrcycle<-as.factor(shaddata1$mtrcycle)
summary(shaddata1$mtrcycle)


str(shaddata1$truck)
summary(shaddata1$truck)
shaddata1$truck<-as.factor(shaddata1$truck)
summary(shaddata1$truck)


###%%%% LOGISTIC REGGRESSION MODEL %%%%%###

# Splitting DATA into Test and Training Samples
set.seed(200)
index<-sample(nrow(shaddata1),0.70*nrow(shaddata1),replace=F)
train<-shaddata1[index,]
test<-shaddata1[-index,]


#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

names(shaddata1)


# Building Logistic Regression Model after excluding var "Customer_ID" 
mod<-glm(churn~.,data=train[,-50],family="binomial")
summary(mod)

#my data edit step ise

# Step wise Regression Model###------ Each Step Takes Atleat 1/2 Hour.Also showing extensive memory issues.
#                                 So cannot use the method. Doing Manually..
step(mod,direction = "both")

names(train)

## ------Creating Dummy Variables for Factor Variables which are significant-----##

summary(shaddata1$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)



summary(train$area)


train$area_nrthwst<-ifelse(train$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)
test$area_nrthwst<-ifelse(test$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)

train$area_southflrda<-ifelse(train$area == "SOUTH FLORIDA AREA", 1, 0)
test$area_southflrda<-ifelse(test$area == "SOUTH FLORIDA AREA", 1, 0)



summary(train$refurb_new)
train$refurb_R<-ifelse(train$refurb_new == "R", 1, 0)
test$refurb_R<-ifelse(test$refurb_new == "R", 1, 0)



summary(train$ethnic)
train$ethnic_C<-ifelse(train$ethnic == "C", 1, 0)
test$ethnic_C<-ifelse(test$ethnic == "C", 1, 0)


train$ethnic_O<-ifelse(train$ethnic == "O", 1, 0)
test$ethnic_O<-ifelse(test$ethnic == "O", 1, 0)

train$ethnic_Z<-ifelse(train$ethnic == "Z", 1, 0)
test$ethnic_Z<-ifelse(test$ethnic == "Z", 1, 0)



summary(train$hnd_price)


train$hnd_price_105.08<-ifelse(train$hnd_price == "105.083038078331", 1, 0)
test$hnd_price_105.08<-ifelse(test$hnd_price == "105.083038078331", 1, 0)

train$hnd_price_129.98<-ifelse(train$hnd_price == "129.9899902", 1, 0)
test$hnd_price_129.98<-ifelse(test$hnd_price == "129.9899902", 1, 0)


train$hnd_price_199.98<-ifelse(train$hnd_price == "199.9899902", 1, 0)
test$hnd_price_199.98<-ifelse(test$hnd_price == "199.9899902", 1, 0)

train$hnd_price_249.98<-ifelse(train$hnd_price == "249.9899902", 1, 0)
test$hnd_price_249.98<-ifelse(test$hnd_price == "249.9899902", 1, 0)


summary(train$uniqsubs)

train$unq_2<-ifelse(train$uniqsubs == "2", 1, 0)
test$unq_2<-ifelse(test$uniqsubs == "2", 1, 0)

train$unq_3<-ifelse(train$uniqsubs == "3", 1, 0)
test$unq_3<-ifelse(test$uniqsubs == "3", 1, 0)

train$unq_4<-ifelse(train$uniqsubs == "4", 1, 0)
test$unq_4<-ifelse(test$uniqsubs == "4", 1, 0)

train$unq_5<-ifelse(train$uniqsubs == "5", 1, 0) 
test$unq_5<-ifelse(test$uniqsubs == "5", 1, 0)


train$unq_7<-ifelse(train$uniqsubs == "7", 1, 0)
test$unq_7<-ifelse(test$uniqsubs == "7", 1, 0)



summary(train$truck)


summary(train$prizm_social_one_1)

train$przm_social_R<-ifelse(train$prizm_social_one_1 == "R", 1, 0)
test$przm_social_R<-ifelse(test$prizm_social_one_1 == "R", 1, 0)

train$przm_social_T<-ifelse(train$prizm_social_one_1 == "T", 1, 0)

test$przm_social_T<-ifelse(test$prizm_social_one_1 == "T", 1, 0)
summary(train$przm_social_R)



summary(train$age1_1)

train$age1_Young<-ifelse(train$age1_1 == "Young", 1, 0) 
test$age1_Young<-ifelse(test$age1_1 == "Young", 1, 0)

train$age1_Mid_Age<-ifelse(train$age1_1 == "Mid Age", 1, 0)
test$age1_Mid_Age<-ifelse(test$age1_1 == "Mid Age", 1, 0)

train$age1_Old<-ifelse(train$age1_1 == "Old", 1, 0)
test$age1_Old<-ifelse(test$age1_1 == "Old", 1, 0)


summary(train$age2_1)

train$age2_Old<-ifelse(train$age2_1 == "Old", 1, 0)
test$age2_Old<-ifelse(test$age2_1 == "Old", 1, 0)

summary(train$models)
train$models_2<-ifelse(train$models == "2", 1, 0)
test$models_2<-ifelse(test$models == "2", 1, 0)

train$models_4<-ifelse(train$models == "4", 1, 0)
test$models_4<-ifelse(test$models == "4", 1, 0)


## -----Second Model with Significant Variables ----- ##

names(train)
model1<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
              mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + avgmou + 
              asl_flag_Y + area_nrthwst + area_southflrda +
              refurb_R + ethnic_C + ethnic_O + ethnic_Z + 
              hnd_price_105.08 + hnd_price_129.98 + 
              hnd_price_199.98 + hnd_price_249.98 + unq_2 + unq_3 + unq_4 + unq_5 + unq_7 +
              przm_social_R + przm_social_T + age1_Mid_Age +
              age1_Old + age1_Young + age2_Old + models_2 ,data=train,family="binomial")
summary(model1)



## ----Third model with Significant Variables ----- ##

model2<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
              mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + avgmou + 
              asl_flag_Y + area_nrthwst + area_southflrda +
              refurb_R + ethnic_C + ethnic_O + ethnic_Z + 
              hnd_price_105.08 + hnd_price_129.98 + 
              hnd_price_199.98 + hnd_price_249.98 + unq_2 + unq_3 + unq_4 + unq_7 +
              przm_social_T + age1_Mid_Age +
              age1_Old + age2_Old + models_2 , data=train, family="binomial")
summary(model2)

## ----FOURTH model with Significant Variables ----- ##


model3<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
              mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + avgmou + 
              asl_flag_Y + area_nrthwst + area_southflrda +
              refurb_R + ethnic_C + ethnic_O + ethnic_Z + 
              hnd_price_105.08 + hnd_price_129.98 + 
              hnd_price_199.98 + hnd_price_249.98 + unq_2 + unq_3 + unq_4 + unq_7 +
              przm_social_T + age1_Mid_Age +
              age1_Old + age2_Old  , data=train, family="binomial")
summary(model3)

# All the variables are  significant Third model(mod2)# 


# Checking For Multicollinearity


library(car)

vif(model2)

# Ideally vif values for the variables should be  Less than five so  Choosing vif cut-off value as 5.. 
# In this model each variable shows VIF value Less than five, so finelising model2 model 


# Checking Confidence Interval
confint(model3) 

#Assuming cut-off probablity as 0.5
table(shaddata1$churn)/nrow(shaddata1)

pred<-predict(model3, type="response", newdata=test)
head(pred)
str(pred)




predicted<-ifelse(pred>=0.5,1,0)
table(predicted)




## ----Checking Quality Of The prediction---- ##



#Confusion Matrix

ab <- table(predicted,train$churn)
#Accuracy = (TP+TN)/(P+N)
acc <- (34518+580)/(34518+580+10463+764)
acc
### acc value ==0.7570172  ###
pred<-prediction(predicted,train$churn)
auc<-performance(pred,"auc")
auc

###auc value==61.29###




###%%%%ROCR Curve%%%%#####

dev.off()

library(ROCR)
pred2<-prediction(predicted,test$churn)
pref<-performance(pred2,"tpr","fpr")
plot(pref,col="red")
abline(0,1,lty=8,col="grey")
auc<-performance(pred2,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc



#Gains Chart

library(gains)
gains(test$churn,predict(model3,type="response",newdata=test),groups = 10)

#the Gains Chart shows that the top 30% of the probabilities contain 41.7% customers that are likely to churn.


test$prob<-predict(model3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 40% of the probability scores lie between 0.2508397 and 0.2747686
#We can use this probablity to extract the data of customers who are highly likely to churn.





### -------%%%%% Answering Business Questions %%%%-------- ###


#  1.  What are the top five factors driving likelihood of churn at Mobicom?

head(sort(abs(model3$coefficients),decreasing = T),10)
summary(model3)

# from the summary of my final model, "Mod6".

## The model results show that the top 9 factors affecting churn are:


#         ethnic_C            unq_7 

#        0.8048570         0.7296546 

#    asl_flag_Y             area_southflrda                

#      0.4266324            0.3700085                           

#  hnd_price_249.08              

#    0.2997775                           


##%%% EXPLANATION%%%##

# The 1st Factor explains, with a unit increase in variable ethnic_C, there is 0.8048570 unit increase in churn.
# Same explaination applies to the next 4 variables.

# There is a huge impact of account spending limit in churning rate, so make it little bit liberal
# family bundles should be rolled out for families with unique subscribers 7(unq_7), Special offers should be given.
# people with Asian and African american Ethnicity. Special special plans should be rolled out for customers located in SOUTH FLORIDA AREA. 
# As the price of hnd_price increse the churn rate is increasing. so hand set price should be feasible. if handset price is more people will opt for less price hand set.

#  Q2.  Validation of survey findings. 

# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  

# The following variables explain "cost and billing" and "network and service quality"

# Variables totmrc_Mean i.e. 'base plan charge' representing cost to customer, 
# var rev_Range i.e. 'Range of Revenue(charge amount)' representing billing amount,
# i.e. 'Mean overage revenue' (It is the sum of data and voice 
# overage revenues) representing the overange revenue earned from customers after billing the same to them.   



# var totmrc_Mean has beta coefficient value of -0.00422403 meaning a unit increase in this variable is causing 
# decrease in churn by 0.004224030.005294251/unit.

# var rev_Range has beta coefficient value of 0.00394177 meaning a unit increase in this variable is causing 
# increase in churn by 0.00394177/unit


# The above values of cost and billing gives us an insight that the above mentioned beta values, a unit increase in them is having almost 0% impact 
# on churn. SO it seems cost and billing is not very important factors here influencing churn behaviour at Mobicom.



# The following variables explain "network and service quality" 

# VARIABLE          BETA COEFFICIENT

# mou_Range         0.0006015
# change_mou       -0.00056797
# drop_blk_Mean     0.00600541
# drop_vce_Range    0.01826918 
# mou_opkv_Range   -0.00127678  
# iwylis_vce_Mean  -0.01612279
# adjmou            0.00056597



# From the above statistics, data explains the following:

# Variables mou_Range 1.e. with a unit increase in 'Range of number of minutes of use', 
#       there is increase in Churn by 0.0006015 units.
# var change_mou i.e. with a unit increase in 'Percentage change in monthly minutes of 
#     use vs previous three month average, there is decrease in Churn by -0.00056797 units.
# var drop_blk_Mean i.e. with unit increase in 'Mean number of dropped or blocked calls', 
#     there is an increase in churn by 0.00600541 units
# var drop_vce_Range i.e. with a unit increase in 'Range of number of dropped (failed) voice calls', 
#     there is an increase in Churn by 0.01826918 units.
# var mou_opkv_Range  i.e. with a unit increase in  'Range of unrounded minutes of use of 
#     off-peak voice calls, there is a decrease in Churn by -0.00127678 units.
# var iwylis_vce_Mean i.e. with a unit increase in 'Mean number of inbound wireless to wireless voice calls',
#     there is a decrease in churn by -0.01612279 units.
# var adjmou i.e. with unit increase in  'Billing adjusted total minutes of use over the life of the customer',
#     there is an increase in Churn by 0.00056597 units.

# Of the above variables, the beta coefficient of variable drop_vce_Range is expressing a moderate important 
# factor in influencing Churn behaviour. That is  with the increase in the mean no of dropped voice calls 
# the customer's chances of churning is high. this gives an indication that you have to give more concentration on your network issues.





#  2b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

#   opk_dat_Mean - Mean number of off-peak data calls
#   blck_dat_Mean - Mean no. of blocked / failed data calls
#   datovr_Mean - Mean revenue of data overage. 
#   datovr_Range - Range of revenue of data overage


#   The above variables express data usage connectivity. 
quantile(shadata$opk_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(shadata$blck_dat_Mean=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(shadata$datovr_Mean=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(shadata$datovr_Range=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))

#   The Data Quality Report for all the above variables show that only 10% to 15% customers are actualy 
#   making data calls or using the internet. 
#   In this case it seems customers are not really using the internet. So it would be good to work 
#   towards attaining more customers to use data and also towards proving quality network connectivity
#   and service to provide maximum customer satisfaction and reduce Churn.




#   3. Would you recommend rate plan migration as a proactive retention strategy?

#   Variable ovrrev_Mean has beta coefficient of 0.009546578. 
#   The Beta coefficient is not showing a strong impact of overage billing as an influencer 
#   of churn behaviour. 
#   Though this might be a matter of concern for few individual customers and they could be 
#   catered to on case to case basis. But overall rate plan migration as a proactive retention strategy
#   might not help much at Mobicom.


#   Q4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

# Solution:
#Gains Chart
library(gains)
gains(test$churn,predict(model3,type="response",newdata=test),groups = 10)
#the Gains Chart shows that the top 20% of the probabilities contain 30.2% customers that are highly likely to churn.


# Selecting Customers with high churn rate
test$prob<-predict(model3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.3016806 and 0.6864997 

# Applying cutoff value to predict customers who Will Churn
pred4<-predict(model3, type="response", newdata=test)
pred4<-ifelse(pred4>=0.3016806 , 1, 0)
table(pred4,test$churn)

Targeted<-test[test$prob>0.3016806 & test$prob<=0.6864997 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

#   Thus Using the modelwe can use the model to predict customers with high probability of Churn and extract the 
#   target list using their "Customer ID". 

write.csv(Targeted,"Target_Customers with heigh churn P.csv",row.names = F)


# 5. What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue 
# customers besides managing churn. Given a budget constraint of a contact list of 20% of the subscriber pool, 
# which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

# Solution:
pred5<-predict(model3, type="response", newdata=test)
test$prob<-predict(model3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
table(pred6,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                  test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred6,Revenue_Levels)

##  Thus this table can be used to select the levels of customers are to be targeted
##  and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                       test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Customers_with_heigh_p.csv",row.names = F)




