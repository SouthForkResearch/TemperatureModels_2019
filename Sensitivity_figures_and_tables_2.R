#Table and figure data for manuscript 

#### SET WORKING DIRECTORY ####
WD <-"C:/Users/deraj/Documents/South Fork/Temperature models/Manuscript_clean/"
#put all CSV files in directory
setwd(WD)

### Or alternatively load all data from data file
load(file = "All_models.RData")



library(visreg)
library(mgcv)

#### MODEL RESULTS ####

#### * RMSE by year for each watershed table results ####
WenatcheeYearRMSE
WenatcheeYearRMSELin
ChiwawaYearRMSE
ChiwawaYearRMSELin
MFJD_RMSE_GAM
MFJD_RMSE_Lin
Tuc_RMSE_GAM
Tuc_RMSE_Lin

#### * NSE VALUES ####
#NSE for all daily models
1 - ((sum((data.NW.day.COM$residGAM)^2))/(sum((data.NW.day.COM$DailyMean-(mean(data.NW.day.COM$DailyMean)))^2))) #0.939086
1 - ((sum((data.NW.day.CHCOM$residLIN)^2))/(sum((data.NW.day.CHCOM$DailyMean-(mean(data.NW.day.CHCOM$DailyMean)))^2))) #0.9659742
1 - ((sum((data.sub.NW.COM$residGAM)^2))/(sum((data.sub.NW.COM$DailyMean-(mean(data.sub.NW.COM$DailyMean)))^2))) #0.8863101
1 - ((sum((Tuc_NWmainstem_COM$residCHAMP)^2))/(sum((Tuc_NWmainstem_COM$DailyMean-(mean(Tuc_NWmainstem_COM$DailyMean)))^2))) #0.9641875

colnames(Tuc_Champfl)
#Wenatchee NSC
1 - ((sum((data.NW.day.sp$residGAM)^2))/(sum((data.NW.day.sp$DailyMean-(mean(data.NW.day.sp$DailyMean)))^2))) #0.9412966
1 - ((sum((data.NW.day.fl$residGAM)^2))/(sum((data.NW.day.fl$DailyMean-(mean(data.NW.day.fl$DailyMean)))^2))) #0.9301683
1 - ((sum((data.NW.day.sp$residLIN)^2))/(sum((data.NW.day.sp$DailyMean-(mean(data.NW.day.sp$DailyMean)))^2))) #0.9288725
1 - ((sum((data.NW.day.fl$residLIN)^2))/(sum((data.NW.day.fl$DailyMean-(mean(data.NW.day.fl$DailyMean)))^2))) #0.9118443 
#NSC Train
1 - ((sum((data.day.sp$resid)^2))/(sum((data.day.sp$AvgDailyTemp-(mean(data.day.sp$AvgDailyTemp)))^2))) #0.9547764
1 - ((sum((data.day.fl$resid)^2))/(sum((data.day.fl$AvgDailyTemp-(mean(data.day.fl$AvgDailyTemp)))^2))) #0.952161
1 - ((sum((data.day.sp$residLin)^2))/(sum((data.day.sp$AvgDailyTemp-(mean(data.day.sp$AvgDailyTemp)))^2))) #0.9537611
1 - ((sum((data.day.fl$residLin)^2))/(sum((data.day.fl$AvgDailyTemp-(mean(data.day.fl$AvgDailyTemp)))^2))) #0.9486571
#Chiwawa NSC
1 - ((sum((data.NW.day.spCH$residGAM)^2))/(sum((data.NW.day.spCH$DailyMean-(mean(data.NW.day.spCH$DailyMean)))^2))) #0.9713482
1 - ((sum((data.NW.day.flCH$residGAM)^2))/(sum((data.NW.day.flCH$DailyMean-(mean(data.NW.day.flCH$DailyMean)))^2))) #0.9502811
1 - ((sum((data.NW.day.spCH$residLIN)^2))/(sum((data.NW.day.spCH$DailyMean-(mean(data.NW.day.spCH$DailyMean)))^2))) #0.9676852
1 - ((sum((data.NW.day.flCH$residLIN)^2))/(sum((data.NW.day.flCH$DailyMean-(mean(data.NW.day.flCH$DailyMean)))^2))) #0.9577446
#NSC Train
1 - ((sum((data.NW.day.spCH$residGAM)^2))/(sum((data.NW.day.spCH$AvgDailyTemp-(mean(data.NW.day.spCH$AvgDailyTemp)))^2))) #0.9713482
1 - ((sum((data.NW.day.flCH$residGAM)^2))/(sum((data.NW.day.flCH$AvgDailyTemp-(mean(data.NW.day.flCH$AvgDailyTemp)))^2))) #0.9502811
1 - ((sum((data.NW.day.spCH$residLIN)^2))/(sum((data.NW.day.spCH$AvgDailyTemp-(mean(data.NW.day.spCH$AvgDailyTemp)))^2))) #0.9676852
1 - ((sum((data.NW.day.flCH$residLIN)^2))/(sum((data.NW.day.flCH$AvgDailyTemp-(mean(data.NW.day.flCH$AvgDailyTemp)))^2))) #0.9577446
#MFJD NSC
1 - ((sum((data.sub.NW.sp$residGAM)^2))/(sum((data.sub.NW.sp$DailyMean-(mean(data.sub.NW.sp$DailyMean)))^2))) #0.8963952
1 - ((sum((data.sub.NW.fl$residGAM)^2))/(sum((data.sub.NW.fl$DailyMean-(mean(data.sub.NW.fl$DailyMean)))^2))) #0.8863101
1 - ((sum((data.sub.NW.sp$residLin)^2))/(sum((data.sub.NW.sp$DailyMean-(mean(data.sub.NW.sp$DailyMean)))^2))) #0.8535131
1 - ((sum((data.sub.NW.fl$residLin)^2))/(sum((data.sub.NW.fl$DailyMean-(mean(data.sub.NW.fl$DailyMean)))^2))) #0.8471668
#NSC Train
1 - ((sum((data.CH.sub.sp$residCHAMP)^2))/(sum((data.CH.sub.sp$AvgDailyTemp-(mean(data.CH.sub.sp$AvgDailyTemp)))^2))) #0.9574925
1 - ((sum((data.CH.sub.fl$residCHAMP)^2))/(sum((data.CH.sub.fl$AvgDailyTemp-(mean(data.CH.sub.fl$AvgDailyTemp)))^2))) #0.9359267
1 - ((sum((data.CH.sub.sp$residLin)^2))/(sum((data.CH.sub.sp$AvgDailyTemp-(mean(data.CH.sub.sp$AvgDailyTemp)))^2))) #0.9450221
1 - ((sum((data.CH.sub.fl$residLin)^2))/(sum((data.CH.sub.fl$AvgDailyTemp-(mean(data.CH.sub.fl$AvgDailyTemp)))^2))) #0.9297717
#Tucannon NSC
1 - ((sum((Tuc_NWsp$residCHAMP)^2))/(sum((Tuc_NWsp$DailyMean-(mean(Tuc_NWsp$DailyMean)))^2))) #0.9698696
1 - ((sum((Tuc_NWfl$residCHAMP)^2))/(sum((Tuc_NWfl$DailyMean-(mean(Tuc_NWfl$DailyMean)))^2))) #0.9568407
1 - ((sum((Tuc_NWsp$residLin)^2))/(sum((Tuc_NWsp$DailyMean-(mean(Tuc_NWsp$DailyMean)))^2))) #0.9681377
1 - ((sum((Tuc_NWfl$residLin)^2))/(sum((Tuc_NWfl$DailyMean-(mean(Tuc_NWfl$DailyMean)))^2))) #0.9554763
#NSC Train
1 - ((sum((Tuc_Champsp$residCHAMP)^2))/(sum((Tuc_Champsp$AvgDailyTemp-(mean(Tuc_Champsp$AvgDailyTemp)))^2))) #0.9699556
1 - ((sum((Tuc_Champfl$residCHAMP)^2))/(sum((Tuc_Champfl$AvgDailyTemp-(mean(Tuc_Champfl$AvgDailyTemp)))^2))) #0.9572649
1 - ((sum((Tuc_Champsp$residLin)^2))/(sum((Tuc_Champsp$AvgDailyTemp-(mean(Tuc_Champsp$AvgDailyTemp)))^2))) #0.9572649
1 - ((sum((Tuc_Champfl$residLin)^2))/(sum((Tuc_Champfl$AvgDailyTemp-(mean(Tuc_Champfl$AvgDailyTemp)))^2))) #0.9572649


#### * Sample sizes ####

#Wenatchee
siteyear <- aggregate(SiteName ~ Year, data=data.day, FUN=function(x) length(unique(x)))
mean(siteyear$SiteName)
siteyear <- aggregate(PERMA_FID.x ~ year, data=data.NW.day2, FUN=function(x) length(unique(x)))
mean(siteyear$PERMA_FID.x)
siteyear <- aggregate(PERMA_FID.x ~ year, data=data.NW.day2, FUN=function(x) length(unique(x)))
mean(siteyear$PERMA_FID.x)

#Chiwawa
siteyear <- aggregate(SiteName ~ Year, data=data.dayCH, FUN=function(x) length(unique(x)))
mean(siteyear$SiteName)
siteyear <- aggregate(PERMA_FID.x ~ year, data=data.NW.dayCH, FUN=function(x) length(unique(x)))
mean(siteyear$PERMA_FID.x)

#MFJD
library(plyr)
count<-count(data.CH.sub, "Month")
count<-count(data.sub.NW, "month")
length(unique(data.CH.sub$SiteName))
length(unique(data.sub.NW$PERMA_FID.x))
sitestuc<-aggregate(SiteName ~ Year, data=data.CH.sub, FUN=function(x) length(unique(x)))
sitesMFJD<-aggregate(SiteName ~ Year, data=data.CH.sub, FUN=function(x) length(unique(x)))
aggregate(Year ~ SiteName, data=data.CH.sub, FUN=function(x) length(unique(x)))
siteyear <- aggregate(SiteName ~ Year, data=data.CH.sub, FUN=function(x) length(unique(x)))
mean(siteyear$SiteName)
siteyear <- aggregate(PERMA_FID.x ~ year, data=data.sub.NW, FUN=function(x) length(unique(x)))
mean(siteyear$PERMA_FID.x)
#Tucannon 
library(plyr)
count<-count(Tuc_Champ_mainstem, "Month")
count<-count(Tuc_NW_mainstem, "month")
count<-count(Tuc_Champ_mainstem, "Month", "SiteName")
length(unique(Tuc_Champ_mainstem$SiteName))
length(unique(Tuc_NW_mainstem$PERMA_FID.x))
data.frame ( table ( Tuc_Champ_mainstem$Year, unique(Tuc_Champ_mainstem$SiteName)) [,] ) 
aggregate(Year ~ SiteName, data=Tuc_Champ_mainstem, FUN=function(x) length(unique(x)))
sitestuc<-aggregate(SiteName ~ Year, data=Tuc_Champ_mainstem, FUN=function(x) length(unique(x)))
mean(sitestuc$SiteName)
aggregate(Year ~ SiteName, data=Tuc_Champ_mainstem, FUN=function(x) length(unique(x)))
Tuc_sites_notfull <-c("CBW05583-460671","CBW05583-427903","CBW05583-460671","CBW05583-420019","CBW05583-415923","CBW05583-384819","CBW05583-353323","CBW05583-345983","CBW05583-274303","CBW05583-038783","CBW05583-051659","CBW05583-079743","CBW05583-128895","CBW05583-141771","CBW05583-168191","CBW05583-203211","CBW05583-208767","CBW05583-212787","CBW05583-214475","CBW05583-214911","CBW05583-222251","CBW05583-178047","CBW05583-465355")
Tuc_Champ_mainstemFULL <- subset(Tuc_Champ_mainstem, !(SiteName %in% Tuc_sites_notfull))
aggregate(Year ~ SiteName, data=Tuc_Champ_mainstemFULL, FUN=function(x) length(unique(x)))
length(unique(Tuc_Champ_mainstemFULL$SiteName))
siteyear <- aggregate(SiteName ~ Year, data=Tuc_Champ_mainstem, FUN=function(x) length(unique(x)))
mean(siteyear$SiteName)
siteyear <- aggregate(PERMA_FID.x ~ year, data=Tuc_NW_mainstem, FUN=function(x) length(unique(x)))
mean(siteyear$PERMA_FID.x)




#### * Month X month RMSE and NSC table ####
Monthly_Table
#write.csv(Monthly_Table,"Monthly_table.csv")








#  SENSITIVITY ANALYSIS  #####
#(YEARS AND SITES SEPERATELY), run before running loops 


# * Wenatchee Years Sensitivity  ####

data.NWsub.day.sp <- data.NW.day.sp
data.NWsub.day.fl <- data.NW.day.fl

#drop unused factor levels
data.day.sp$SiteName<-factor(data.day.sp$SiteName)
data.day.fl$SiteName<-factor(data.day.fl$SiteName)
data.day.COM<-rbind(data.day.sp,data.day.fl)
We_Sites<-unique(data.day.COM$SiteName)
SpWe_Sites<-unique(data.day.sp$SiteName)
FlWe_Sites<-unique(data.day.fl$SiteName)
SpWe_Years<-unique(data.day.sp$Year)
FlWe_Years<-unique(data.day.fl$Year)


#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD.MEANS <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"

####Looping for number of years in prediction dataset##
Years <- c(2012:2017)
year.count<- c(6:3)
for (x in year.count){
  Years.list <- as.data.frame(replicate(100, sample(Years,x,replace = FALSE)))##replace 7 with y

  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
#Loop for iterations#
  loop_iterations<-c(1:100)
for (y in loop_iterations) {
  #selecting years for iteration
  Years.used <- Years.list[,y]
  
data.day.sp$subsetY <- (data.day.sp$Year) %in% (Years.used)
data.daySub.sp<- subset(data.day.sp, subsetY == "TRUE")
data.day.fl$subsetY <- (data.day.fl$Year) %in% (Years.used)
data.daySub.fl<- subset(data.day.fl, subsetY == "TRUE")

#Spring models Wenatchee
m1gs <- (gam(AvgDailyTemp ~ s(TAVGn5dC, k = 4) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVGn5dC, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+s(BFI, by = TAVGn5dC, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3)+s(for_cover, by = TAVGn5dC, k = 3), data = data.daySub.sp))
m1gsL <- (lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn5dC+catch_area*dailymeanCMS+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC+for_cover*TAVGn5dC, data = data.daySub.sp))
#fall models Wenatchee
m1gf <- (gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = TAVGn3dC, k = 3)+s(ae, by = TAVGn3dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(BFI, by = TAVGn3dC, k = 3)+ s(Echange2, by = TAVGn3dC, k = 3)+ s(JulianDate, k = 5), data = data.daySub.fl))
m1gfL <- (lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn3dC+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+SLOPE*TAVGn3dC+BFI*TAVGn3dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = data.daySub.fl))

#Prediicting Wenatchee
data.NWsub.day.sp$predictGAM<- predict(m1gs, newdata = data.NWsub.day.sp)
data.NWsub.day.fl$predictGAM<- predict(m1gf, newdata = data.NWsub.day.fl)
data.NWsub.day.sp$predictGAM[data.NWsub.day.sp$predictGAM < 0] <- 0
data.NWsub.day.fl$predictGAM[data.NWsub.day.fl$predictGAM < 0] <- 0
data.NWsub.day.sp$predictLIN<- predict(m1gsL, newdata = data.NWsub.day.sp)
data.NWsub.day.fl$predictLIN<- predict(m1gfL, newdata = data.NWsub.day.fl)
data.NWsub.day.sp$predictLIN[data.NWsub.day.sp$predictLIN < 0] <- 0
data.NWsub.day.fl$predictLIN[data.NWsub.day.fl$predictLIN < 0] <- 0

###calculating residuals 
data.NWsub.day.sp$residGAM<- data.NWsub.day.sp$DailyMean - data.NWsub.day.sp$predictGAM
data.NWsub.day.fl$residGAM<- data.NWsub.day.fl$DailyMean - data.NWsub.day.fl$predictGAM
data.NWsub.day.sp$Resid2 <- ((data.NWsub.day.sp$residGAM)^2)
data.NWsub.day.fl$Resid2 <- ((data.NWsub.day.fl$residGAM)^2)
data.NWsub.day.sp$residLIN<- data.NWsub.day.sp$DailyMean - data.NWsub.day.sp$predictLIN
data.NWsub.day.fl$residLIN<- data.NWsub.day.fl$DailyMean - data.NWsub.day.fl$predictLIN
data.NWsub.day.sp$ResidLIN2 <- ((data.NWsub.day.sp$residLIN)^2)
data.NWsub.day.fl$ResidLIN2 <- ((data.NWsub.day.fl$residLIN)^2)

###CHAMP predicting NorWest COMBINED ALL YEAR DATA
data.NWsub.day.COM <- rbind(data.NWsub.day.sp,data.NWsub.day.fl)
#RMSE for each iteration of sites
GAM.RMSE <-mean(data.NWsub.day.COM$Resid2)^.5 
Lin.RMSE <-mean(data.NWsub.day.COM$ResidLIN2)^.5 

#bringing together a list of each site iteration
GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)

}#end of loop for year iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #

GAM.RMSE.MEANS.WNyear<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.WNyear<- Lin.RMSE.MEANS
GAM.SD.MEANS.WNyear<- GAM.SD.MEANS
Lin.SD.MEANS.WNyear<- Lin.SD.MEANS

GAM.RMSE.MEANS.WNyear
Lin.RMSE.MEANS.WNyear
GAM.SD.MEANS.WNyear
Lin.SD.MEANS.WNyear
#STOP FOR WENATCHEE YEAR SENSITIVITY#






# * Wenatchee Sites Sensitivity####


#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.SD)<-"SD"
Lin.SD.MEANS <- matrix(ncol=1, nrow=0)
colnames(Lin.SD)<-"SD"


# Loop site sub sampling #
site.countWE <- c(40,35,30,25,20,15,10)
for (i in site.countWE) {
  
  #making replicates for subsampling number of sites
  SitesList <- as.data.frame(replicate(100, sample(We_Sites,i,replace = FALSE)))
  colnames(SitesList)<-paste0("x",c(loop_iterations))
  
#creating RMSE item to be filled by iteration values
GAM.RMSE.list <- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.list)<-"RMSE_iterations"
Lin.RMSE.list <- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.list)<-"RMSE_iterations"

#number of iterations (must match replicate number above)
loop_iterations<-c(1:100)
for (z in loop_iterations) {
  #selecting sites for iteration
We_Sites_sample <- SitesList[,z]

  #subsetting for iteration
data.day.sp$subset <- (data.day.sp$SiteName) %in% (We_Sites_sample)
data.daySub.sp<- subset(data.day.sp, subset == "TRUE")
data.day.fl$subset <- (data.day.fl$SiteName) %in% (We_Sites_sample)
data.daySub.fl<- subset(data.day.fl, subset == "TRUE")

#Spring models Wenatchee
m1gs <- (gam(AvgDailyTemp ~ s(TAVGn5dC, k = 4) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVGn5dC, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+s(BFI, by = TAVGn5dC, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3)+s(for_cover, by = TAVGn5dC, k = 3), data = data.daySub.sp))
m1gsL <- (lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn5dC+catch_area*dailymeanCMS+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC+for_cover*TAVGn5dC, data = data.daySub.sp))
#fall models Wenatchee
m1gf <- (gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = TAVGn3dC, k = 3)+s(ae, by = TAVGn3dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(BFI, by = TAVGn3dC, k = 3)+ s(Echange2, by = TAVGn3dC, k = 3)+ s(JulianDate, k = 5), data = data.daySub.fl))
m1gfL <- (lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn3dC+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+SLOPE*TAVGn3dC+BFI*TAVGn3dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = data.daySub.fl))

#Prediicting Wenatchee
data.NWsub.day.sp$predictGAM<- predict(m1gs, newdata = data.NWsub.day.sp)
data.NWsub.day.fl$predictGAM<- predict(m1gf, newdata = data.NWsub.day.fl)
data.NWsub.day.sp$predictGAM[data.NWsub.day.sp$predictGAM < 0] <- 0
data.NWsub.day.fl$predictGAM[data.NWsub.day.fl$predictGAM < 0] <- 0
data.NWsub.day.sp$predictLIN<- predict(m1gsL, newdata = data.NWsub.day.sp)
data.NWsub.day.fl$predictLIN<- predict(m1gfL, newdata = data.NWsub.day.fl)
data.NWsub.day.sp$predictLIN[data.NWsub.day.sp$predictLIN < 0] <- 0
data.NWsub.day.fl$predictLIN[data.NWsub.day.fl$predictLIN < 0] <- 0

###calculating residuals 
data.NWsub.day.sp$residGAM<- data.NWsub.day.sp$DailyMean - data.NWsub.day.sp$predictGAM
data.NWsub.day.fl$residGAM<- data.NWsub.day.fl$DailyMean - data.NWsub.day.fl$predictGAM
data.NWsub.day.sp$Resid2 <- ((data.NWsub.day.sp$residGAM)^2)
data.NWsub.day.fl$Resid2 <- ((data.NWsub.day.fl$residGAM)^2)
data.NWsub.day.sp$residLIN<- data.NWsub.day.sp$DailyMean - data.NWsub.day.sp$predictLIN
data.NWsub.day.fl$residLIN<- data.NWsub.day.fl$DailyMean - data.NWsub.day.fl$predictLIN
data.NWsub.day.sp$ResidLIN2 <- ((data.NWsub.day.sp$residLIN)^2)
data.NWsub.day.fl$ResidLIN2 <- ((data.NWsub.day.fl$residLIN)^2)

###CHAMP predicting NorWest COMBINED ALL YEAR DATA
data.NWsub.day.COM <- rbind(data.NWsub.day.sp,data.NWsub.day.fl)
  #RMSE for each iteration of sites
GAM.RMSE <-mean(data.NWsub.day.COM$Resid2)^.5 
Lin.RMSE <-mean(data.NWsub.day.COM$ResidLIN2)^.5 

  #bringing together a list of each site iteration
GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)

}#end of loop for site iterations


  #Taking mean of all iterations for year/site# combination
GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))

  #standard deviatoin
GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
Lin.SD.mean<-sd(unlist(Lin.RMSE.list))

#adding means to vector of different number of sites (same number of years)
GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)

GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)

}#end of foorloop for site #

GAM.RMSE.MEANS.WNsites<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.WNsites<- Lin.RMSE.MEANS
GAM.SD.MEANS.WNsites<- GAM.SD.MEANS
Lin.SD.MEANS.WNsites<- Lin.SD.MEANS


      #end results for wenatchee
GAM.RMSE.MEANS.WNsites
Lin.RMSE.MEANS.WNsites
GAM.SD.MEANS.WNsites
Lin.SD.MEANS.WNsites

GAM.RMSE.MEANS.WNyear
Lin.RMSE.MEANS.WNyear
GAM.SD.MEANS.WNyear
Lin.SD.MEANS.WNyear

#################### STOP WENATCHEE SITES SENSITIVITY ##







#LOOPS FOR MFJD (YEARS AND SITES SEPERATELY), run before running loops

# * MFJD Years Sensitivity ##################################


data.SUB.NW.sp <- data.sub.NW.sp
data.SUB.NW.fl <- data.sub.NW.fl

#drop unused factor levels
data.CH.sub.sp$SiteName<-factor(data.CH.sub.sp$SiteName)
data.CH.sub.fl$SiteName<-factor(data.CH.sub.fl$SiteName)
data.CH.sub.fl.COM  <- rbind(data.CH.sub.sp,data.CH.sub.fl)
SpWe_Sites<-unique(data.CH.sub.fl.COM$SiteName)

SpWe_Years<-unique(data.CH.sub.sp$Year)
FlWe_Years<-unique(data.CH.sub.fl$Year)




#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD<- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"

Years <- c(2012:2017)
year.count<- c(6:3)
for (x in year.count){
  Years.list <- as.data.frame(replicate(100, sample(Years,x,replace = FALSE)))##replace 7 with y
  
  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
  #Loop for iterations#
  loop_iterations<-c(1:100)
  for (y in loop_iterations) {
    #selecting years for iteration
    Years.used <- Years.list[,y]
    
    data.CH.sub.sp$subsetY <- (data.CH.sub.sp$Year) %in% (Years.used)
    data.CH.SUB.sp.x<- subset(data.CH.sub.sp, subsetY == "TRUE")
    data.CH.sub.fl$subsetY <- (data.CH.sub.fl$Year) %in% (Years.used)
    data.CH.SUB.fl.x<- subset(data.CH.sub.fl, subsetY == "TRUE")
    
    #####CURRENT BEST GAM MODELS
    MFJDgsCH <- (gam(AvgDailyTemp ~ s(TAVGn5d, k = 4) +s(Tchange5, k = 3)+s(SNWD,by = TAVGn5d, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(GRADIENT, by = TAVGn5d, k = 3)+ s(BFI, by = TAVGn5d, k = 3)+ s(dailymeanCMS, by = TAVGn5d, k = 3) + s(JulianDate, by = TAVGn5d, k = 5)+ s(Echange2, by = TAVGn5d, k = 3)+ s(TreeDens_1, by = TAVGn5d, k = 3), data = data.CH.SUB.sp.x))#+s(TreeDens_1, by = TAVGn5d, k = 3)
    MFJDgfCH <- (gam(AvgDailyTemp ~ s(TAVGn3d, k = 4) +s(Tchange3, k = 3)+s(SNWD,by = TAVGn3d, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(catch_area, by = TAVGn3d, k = 3)+s(ae, by = SNWD, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3d, k = 3)+ s(Echange2, by = TAVGn3d, k = 3)+ s(TreeDens_1, by = TAVGn3d, k = 3)+s(GRADIENT, by = TAVGn3d, k = 3), data = data.CH.SUB.fl.x))#+ s(TreeDens_1, by = TAVGn3d, k = 3)
    #linear
    MFJDgsCHLin <- (lm(AvgDailyTemp ~ TAVGn5d+Tchange5+SNWD*TAVGn5d+A1Snow*JulianDate+catch_area*dailymeanCMS+ae*JulianDate+GRADIENT*TAVGn5d+BFI*TAVGn5d+dailymeanCMS*TAVGn5d+JulianDate*TAVGn5d+Echange2*TAVGn5d, data = data.CH.SUB.sp.x))
    MFJDgfCHLin <- (lm(AvgDailyTemp ~TAVGn3d+Tchange3+SNWD*TAVGn3d+dailymeanCMS*TAVGn3d+catch_area*TAVGn3d+ae*SNWD+ae*JulianDate+Echange2*TAVGn3d+TreeDens_1*TAVGn3d+GRADIENT*TAVGn3d, data = data.CH.SUB.fl.x))
    
    #Prediicting MFJD
    data.SUB.NW.sp$predictGAM<- predict(MFJDgsCH, newdata = data.SUB.NW.sp)
    data.SUB.NW.fl$predictGAM<- predict(MFJDgfCH, newdata = data.SUB.NW.fl)
    data.SUB.NW.sp$predictGAM[data.SUB.NW.sp$predictGAM < 0] <- 0
    data.SUB.NW.fl$predictGAM[data.SUB.NW.fl$predictGAM < 0] <- 0
    data.SUB.NW.sp$predictLIN<- predict(MFJDgsCHLin, newdata = data.SUB.NW.sp)
    data.SUB.NW.fl$predictLIN<- predict(MFJDgfCHLin, newdata = data.SUB.NW.fl)
    data.SUB.NW.sp$predictLIN[data.SUB.NW.sp$predictLIN < 0] <- 0
    data.SUB.NW.fl$predictLIN[data.SUB.NW.fl$predictLIN < 0] <- 0
    
    ###calculating residuals 
    data.SUB.NW.sp$residGAM<- data.SUB.NW.sp$DailyMean - data.SUB.NW.sp$predictGAM
    data.SUB.NW.fl$residGAM<- data.SUB.NW.fl$DailyMean - data.SUB.NW.fl$predictGAM
    data.SUB.NW.sp$Resid2 <- ((data.SUB.NW.sp$residGAM)^2)
    data.SUB.NW.fl$Resid2 <- ((data.SUB.NW.fl$residGAM)^2)
    data.SUB.NW.sp$residLIN<- data.SUB.NW.sp$DailyMean - data.SUB.NW.sp$predictLIN
    data.SUB.NW.fl$residLIN<- data.SUB.NW.fl$DailyMean - data.SUB.NW.fl$predictLIN
    data.SUB.NW.sp$ResidLIN2 <- ((data.SUB.NW.sp$residLIN)^2)
    data.SUB.NW.fl$ResidLIN2 <- ((data.SUB.NW.fl$residLIN)^2)
    
    ###CHAMP predicting NorWest COMBINED ALL YEAR DATA
    data.SUB.NW.COM <- rbind(data.SUB.NW.sp,data.SUB.NW.fl)
    #RMSE for each iteration of sites
    GAM.RMSE <-mean(data.SUB.NW.COM$Resid2)^.5 
    Lin.RMSE <-mean(data.SUB.NW.COM$ResidLIN2)^.5 
    
    #bringing together a list of each site iteration MFJD
    GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
    Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)
    
  }#end of loop for year iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #

GAM.RMSE.MEANS.MFJDyear<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.MFJDyear<- Lin.RMSE.MEANS
GAM.SD.MEANS.MFJDyear<- GAM.SD.MEANS
Lin.SD.MEANS.MFJDyear<- Lin.SD.MEANS

GAM.RMSE.MEANS.MFJDyear
Lin.RMSE.MEANS.MFJDyear
GAM.SD.MEANS.MFJDyear
Lin.SD.MEANS.MFJDyear
#STOP FOR MFJD YEAR SENSITIVITY###




#### * MFJD Sites Sensitivity ####
#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD.MEANS <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"


#### Loop site sub sampling ##
site.countWE <- c(45,35,30,25,20,15,10)
for (i in site.countWE) {
  
  #making replicates for subsampling number of sites
  SitesList <- as.data.frame(replicate(100, sample(SpWe_Sites,i,replace = FALSE)))
  colnames(SitesList)<-paste0("x",c(loop_iterations))
  
  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
  #number of iterations (must match replicate number above)
  loop_iterations<-c(1:100)
  for (z in loop_iterations) {
    #selecting sites for iteration
    We_Sites_sample <- SitesList[,z]
    
    #subsetting for iteration
    data.CH.sub.sp$subset <- (data.CH.sub.sp$SiteName) %in% (We_Sites_sample)
    data.CH.SUB.sp.x<- subset(data.CH.sub.sp, subset == "TRUE")
    data.CH.sub.fl$subset <- (data.CH.sub.fl$SiteName) %in% (We_Sites_sample)
    data.CH.SUB.fl.x<- subset(data.CH.sub.fl, subset == "TRUE")
    
    #####CURRENT BEST GAM MODELS
    MFJDgsCH <- (gam(AvgDailyTemp ~ s(TAVGn5d, k = 4) +s(Tchange5, k = 3)+s(SNWD,by = TAVGn5d, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(GRADIENT, by = TAVGn5d, k = 3)+ s(BFI, by = TAVGn5d, k = 3)+ s(dailymeanCMS, by = TAVGn5d, k = 3) + s(JulianDate, by = TAVGn5d, k = 5)+ s(Echange2, by = TAVGn5d, k = 3)+ s(TreeDens_1, by = TAVGn5d, k = 3), data = data.CH.SUB.sp.x))#+s(TreeDens_1, by = TAVGn5d, k = 3)
    MFJDgfCH <- (gam(AvgDailyTemp ~ s(TAVGn3d, k = 4) +s(Tchange3, k = 3)+s(SNWD,by = TAVGn3d, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(catch_area, by = TAVGn3d, k = 3)+s(ae, by = SNWD, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3d, k = 3)+ s(Echange2, by = TAVGn3d, k = 3)+ s(TreeDens_1, by = TAVGn3d, k = 3)+s(GRADIENT, by = TAVGn3d, k = 3), data = data.CH.SUB.fl.x))#+ s(TreeDens_1, by = TAVGn3d, k = 3)
    #linear
    MFJDgsCHLin <- (lm(AvgDailyTemp ~ TAVGn5d+Tchange5+SNWD*TAVGn5d+A1Snow*JulianDate+catch_area*dailymeanCMS+ae*JulianDate+GRADIENT*TAVGn5d+BFI*TAVGn5d+dailymeanCMS*TAVGn5d+JulianDate*TAVGn5d+Echange2*TAVGn5d, data = data.CH.SUB.sp.x))
    MFJDgfCHLin <- (lm(AvgDailyTemp ~TAVGn3d+Tchange3+SNWD*TAVGn3d+dailymeanCMS*TAVGn3d+catch_area*TAVGn3d+ae*SNWD+ae*JulianDate+Echange2*TAVGn3d+TreeDens_1*TAVGn3d+GRADIENT*TAVGn3d, data = data.CH.SUB.fl.x))
    
    #Prediicting MFJD
    data.SUB.NW.sp$predictGAM<- predict(MFJDgsCH, newdata = data.SUB.NW.sp)
    data.SUB.NW.fl$predictGAM<- predict(MFJDgfCH, newdata = data.SUB.NW.fl)
    data.SUB.NW.sp$predictGAM[data.SUB.NW.sp$predictGAM < 0] <- 0
    data.SUB.NW.fl$predictGAM[data.SUB.NW.fl$predictGAM < 0] <- 0
    data.SUB.NW.sp$predictLIN<- predict(MFJDgsCHLin, newdata = data.SUB.NW.sp)
    data.SUB.NW.fl$predictLIN<- predict(MFJDgfCHLin, newdata = data.SUB.NW.fl)
    data.SUB.NW.sp$predictLIN[data.SUB.NW.sp$predictLIN < 0] <- 0
    data.SUB.NW.fl$predictLIN[data.SUB.NW.fl$predictLIN < 0] <- 0
    
    ###calculating residuals 
    data.SUB.NW.sp$residGAM<- data.SUB.NW.sp$DailyMean - data.SUB.NW.sp$predictGAM
    data.SUB.NW.fl$residGAM<- data.SUB.NW.fl$DailyMean - data.SUB.NW.fl$predictGAM
    data.SUB.NW.sp$Resid2 <- ((data.SUB.NW.sp$residGAM)^2)
    data.SUB.NW.fl$Resid2 <- ((data.SUB.NW.fl$residGAM)^2)
    data.SUB.NW.sp$residLIN<- data.SUB.NW.sp$DailyMean - data.SUB.NW.sp$predictLIN
    data.SUB.NW.fl$residLIN<- data.SUB.NW.fl$DailyMean - data.SUB.NW.fl$predictLIN
    data.SUB.NW.sp$ResidLIN2 <- ((data.SUB.NW.sp$residLIN)^2)
    data.SUB.NW.fl$ResidLIN2 <- ((data.SUB.NW.fl$residLIN)^2)
    
    ###CHAMP predicting NorWest COMBINED ALL YEAR DATA
    data.SUB.NW.COM <- rbind(data.SUB.NW.sp,data.SUB.NW.fl)
    #RMSE for each iteration of sites
    GAM.RMSE <-mean(data.SUB.NW.COM$Resid2)^.5 
    Lin.RMSE <-mean(data.SUB.NW.COM$ResidLIN2)^.5 
    
    #bringing together a list of each site iteration
    GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
    Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)
    
  }#end of loop for site iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #

GAM.RMSE.MEANS.MFJDsites<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.MFJDsites<- Lin.RMSE.MEANS
GAM.SD.MEANS.MFJDsites<- GAM.SD.MEANS
Lin.SD.MEANS.MFJDsites<- Lin.SD.MEANS

GAM.RMSE.MEANS.MFJDsites
Lin.RMSE.MEANS.MFJDsites
GAM.SD.MEANS.MFJDsites
Lin.SD.MEANS.MFJDsites

GAM.RMSE.MEANS.MFJDyear
Lin.RMSE.MEANS.MFJDyear
GAM.SD.MEANS.MFJDyear
Lin.SD.MEANS.MFJDyear

#### STOP MFJD SITES SENSITIVITY ### 





#     #       #       






#LOOPS FOR Tucannon (YEARS AND SITES SEPERATELY), run before running loops
# * Tucannon Years Sensitivity ##################################
library(mgcv)

Tuc_NWsp_sub <- Tuc_NWsp
Tuc_NWfl_sub <- Tuc_NWfl

#drop unused factor levels
Tuc_Champsp$SiteName<-factor(Tuc_Champsp$SiteName)
Tuc_Champfl$SiteName<-factor(Tuc_Champfl$SiteName)
Tuc_Champ_COM  <- rbind(Tuc_Champsp,Tuc_Champfl)
SpWe_Sites<-unique(Tuc_Champ_COM$SiteName)

SpWe_Years<-unique(Tuc_Champsp$Year)
FlWe_Years<-unique(Tuc_Champfl$Year)





#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD <- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"

####Looping for number of years in prediction dataset##
Years <- c(2012:2017)
year.count<- c(6:3)
for (x in year.count){
  Years.list <- as.data.frame(replicate(100, sample(Years,x,replace = FALSE)))##replace 7 with y
  
  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
  #############Loop for iterations#
  loop_iterations<-c(1:100)
  for (y in loop_iterations) {
    #selecting years for iteration
    Years.used <- Years.list[,y]
    
    Tuc_Champsp$subsetY <- (Tuc_Champsp$Year) %in% (Years.used)
    Tuc_Champsp_SUB<- subset(Tuc_Champsp, subsetY == "TRUE")
    Tuc_Champfl$subsetY <- (Tuc_Champfl$Year) %in% (Years.used)
    Tuc_Champfl.SUB<- subset(Tuc_Champfl, subsetY == "TRUE")
    
    ######GAM models
    TucGsp <- (gam(AvgDailyTemp ~ s(TAVG5, k = 4) +s(Tchange5)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG5, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVG5, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(Echange2, by = TAVG5, k = 3), data = Tuc_Champsp_SUB))
    TucGfl <- (gam(AvgDailyTemp ~ s(TAVG3, k = 4) +s(Tchange3)+s(SNWD.lin,by = TAVG3, k = 3) +s(ae,by = SNWD.lin, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG3, k = 3)+ s(dailymeanCMS, by = TAVG3, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(ae, by = JulianDate, k = 5) + s(Echange2, by = TAVG3, k = 3), data = Tuc_Champfl.SUB))
   ######Linear Modesl 
    TucLsp <- (lm(AvgDailyTemp ~ TAVG5 +Tchange5+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+catch_area*TAVG5+ae*JulianDate+dailymeanCMS*TAVG5+JulianDate*TAVG5 + Echange2*TAVG5, data = Tuc_Champsp_SUB))
    TucLfl <- (lm(AvgDailyTemp ~ TAVG3 + Tchange3 + SNWD.lin*TAVG3+SNWD.lin*ae +catch_area*ae+catch_area*TAVG3+dailymeanCMS*TAVG3+JulianDate*TAVG5+ae*JulianDate+Echange2*TAVG3, data = Tuc_Champfl.SUB))
    
    #Prediicting MFJD
    Tuc_NWsp_sub$predictGAM<- predict(TucGsp, newdata = Tuc_NWsp_sub)
    Tuc_NWfl_sub$predictGAM<- predict(TucGfl, newdata = Tuc_NWfl_sub)
    Tuc_NWsp_sub$predictGAM[Tuc_NWsp_sub$predictGAM < 0] <- 0
    Tuc_NWfl_sub$predictGAM[Tuc_NWfl_sub$predictGAM < 0] <- 0
    Tuc_NWsp_sub$predictLIN<- predict(TucLsp, newdata = Tuc_NWsp_sub)
    Tuc_NWfl_sub$predictLIN<- predict(TucLfl, newdata = Tuc_NWfl_sub)
    Tuc_NWsp_sub$predictLIN[Tuc_NWsp_sub$predictLIN < 0] <- 0
    Tuc_NWfl_sub$predictLIN[Tuc_NWfl_sub$predictLIN < 0] <- 0
    
    ###calculating residuals 
    Tuc_NWsp_sub$residGAM<- Tuc_NWsp_sub$DailyMean - Tuc_NWsp_sub$predictGAM
    Tuc_NWfl_sub$residGAM<- Tuc_NWfl_sub$DailyMean - Tuc_NWfl_sub$predictGAM
    Tuc_NWsp_sub$Resid2 <- ((Tuc_NWsp_sub$residGAM)^2)
    Tuc_NWfl_sub$Resid2 <- ((Tuc_NWfl_sub$residGAM)^2)
    Tuc_NWsp_sub$residLIN<- Tuc_NWsp_sub$DailyMean - Tuc_NWsp_sub$predictLIN
    Tuc_NWfl_sub$residLIN<- Tuc_NWfl_sub$DailyMean - Tuc_NWfl_sub$predictLIN
    Tuc_NWsp_sub$ResidLIN2 <- ((Tuc_NWsp_sub$residLIN)^2)
    Tuc_NWfl_sub$ResidLIN2 <- ((Tuc_NWfl_sub$residLIN)^2)
    
    ###CHAMP predicting NorWest COMBINED ALL YEAR DATA
    Tuc_NWsp_COM <- rbind(Tuc_NWsp_sub,Tuc_NWfl_sub)
    #RMSE for each iteration of sites
    GAM.RMSE <-mean(Tuc_NWsp_COM$Resid2)^.5 
    Lin.RMSE <-mean(Tuc_NWsp_COM$ResidLIN2)^.5 
    
    #bringing together a list of each site iteration MFJD
    GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
    Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)
    
  }#end of loop for year iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #

GAM.RMSE.MEANS.TUCyear<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.TUCyear<- Lin.RMSE.MEANS
GAM.SD.MEANS.TUCyear<- GAM.SD.MEANS
Lin.SD.MEANS.TUCyear<- Lin.SD.MEANS

GAM.RMSE.MEANS.TUCyear
Lin.RMSE.MEANS.TUCyear
GAM.SD.MEANS.TUCyear
Lin.SD.MEANS.TUCyear
#STOP FOR TUCANNON YEAR SENSITIVITY##






# * Tucannon Sites Sensitivity ##################################

#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD.MEANS <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"


#### Loop site sub sampling #
site.countWE <- c(41,35,30,25,20,15,10)
for (i in site.countWE) {
  
  #making replicates for subsampling number of sites
  SitesList <- as.data.frame(replicate(100, sample(SpWe_Sites,i,replace = FALSE)))
  colnames(SitesList)<-paste0("x",c(loop_iterations))
  
  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
  #number of iterations (must match replicate number above)
  loop_iterations<-c(1:100)
  for (z in loop_iterations) {
    #selecting sites for iteration
    We_Sites_sample <- SitesList[,z]
    
    #subsetting for iteration
    Tuc_Champsp$subset <- (Tuc_Champsp$SiteName) %in% (We_Sites_sample)
    Tuc_Champsp_SUB<- subset(Tuc_Champsp, subset == "TRUE")
    Tuc_Champfl$subset <- (Tuc_Champfl$SiteName) %in% (We_Sites_sample)
    Tuc_Champfl.SUB<- subset(Tuc_Champfl, subset == "TRUE")
 
    ######GAM models
    TucGsp <- (gam(AvgDailyTemp ~ s(TAVG5, k = 4) +s(Tchange5)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG5, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVG5, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(Echange2, by = TAVG5, k = 3), data = Tuc_Champsp_SUB))
    TucGfl <- (gam(AvgDailyTemp ~ s(TAVG3, k = 4) +s(Tchange3)+s(SNWD.lin,by = TAVG3, k = 3) +s(ae,by = SNWD.lin, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG3, k = 3)+ s(dailymeanCMS, by = TAVG3, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(ae, by = JulianDate, k = 5) + s(Echange2, by = TAVG3, k = 3), data = Tuc_Champfl.SUB))
    ######Linear Modesl 
    TucLsp <- (lm(AvgDailyTemp ~ TAVG5 +Tchange5+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+catch_area*TAVG5+ae*JulianDate+dailymeanCMS*TAVG5+JulianDate*TAVG5 + Echange2*TAVG5, data = Tuc_Champsp_SUB))
    TucLfl <- (lm(AvgDailyTemp ~ TAVG3 + Tchange3 + SNWD.lin*TAVG3+SNWD.lin*ae +catch_area*ae+catch_area*TAVG3+dailymeanCMS*TAVG3+JulianDate*TAVG5+ae*JulianDate+Echange2*TAVG3, data = Tuc_Champfl.SUB))
    
    ######GAM models
    TucGsp <- (gam(AvgDailyTemp ~ s(TAVG5, k = 4) +s(Tchange5)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG5, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVG5, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(Echange2, by = TAVG5, k = 3), data = Tuc_Champsp_SUB))
    TucGfl <- (gam(AvgDailyTemp ~ s(TAVG3, k = 4) +s(Tchange3)+s(SNWD.lin,by = TAVG3, k = 3) +s(ae,by = SNWD.lin, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG3, k = 3)+ s(dailymeanCMS, by = TAVG3, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(ae, by = JulianDate, k = 5) + s(Echange2, by = TAVG3, k = 3), data = Tuc_Champfl.SUB))
    ######Linear Modesl 
    TucLsp <- (lm(AvgDailyTemp ~ TAVG5 +Tchange5+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+catch_area*TAVG5+ae*JulianDate+dailymeanCMS*TAVG5+JulianDate*TAVG5 + Echange2*TAVG5, data = Tuc_Champsp_SUB))
    TucLfl <- (lm(AvgDailyTemp ~ TAVG3 + Tchange3 + SNWD.lin*TAVG3+SNWD.lin*ae +catch_area*ae+catch_area*TAVG3+dailymeanCMS*TAVG3+JulianDate*TAVG5+ae*JulianDate+Echange2*TAVG3, data = Tuc_Champfl.SUB))

        #Prediicting MFJD
    Tuc_NWsp_sub$predictGAM<- predict(TucGsp, newdata = Tuc_NWsp_sub)
    Tuc_NWfl_sub$predictGAM<- predict(TucGfl, newdata = Tuc_NWfl_sub)
    Tuc_NWsp_sub$predictGAM[Tuc_NWsp_sub$predictGAM < 0] <- 0
    Tuc_NWfl_sub$predictGAM[Tuc_NWfl_sub$predictGAM < 0] <- 0
    Tuc_NWsp_sub$predictLIN<- predict(TucLsp, newdata = Tuc_NWsp_sub)
    Tuc_NWfl_sub$predictLIN<- predict(TucLfl, newdata = Tuc_NWfl_sub)
    Tuc_NWsp_sub$predictLIN[Tuc_NWsp_sub$predictLIN < 0] <- 0
    Tuc_NWfl_sub$predictLIN[Tuc_NWfl_sub$predictLIN < 0] <- 0
    
    ###calculating residuals 
    Tuc_NWsp_sub$residGAM<- Tuc_NWsp_sub$DailyMean - Tuc_NWsp_sub$predictGAM
    Tuc_NWfl_sub$residGAM<- Tuc_NWfl_sub$DailyMean - Tuc_NWfl_sub$predictGAM
    Tuc_NWsp_sub$Resid2 <- ((Tuc_NWsp_sub$residGAM)^2)
    Tuc_NWfl_sub$Resid2 <- ((Tuc_NWfl_sub$residGAM)^2)
    Tuc_NWsp_sub$residLIN<- Tuc_NWsp_sub$DailyMean - Tuc_NWsp_sub$predictLIN
    Tuc_NWfl_sub$residLIN<- Tuc_NWfl_sub$DailyMean - Tuc_NWfl_sub$predictLIN
    Tuc_NWsp_sub$ResidLIN2 <- ((Tuc_NWsp_sub$residLIN)^2)
    Tuc_NWfl_sub$ResidLIN2 <- ((Tuc_NWfl_sub$residLIN)^2)
    
    ###CHAMP predicting NorWest COMBINED ALL YEAR DATA
    Tuc_NWsp_COM <- rbind(Tuc_NWsp_sub,Tuc_NWfl_sub)
    #RMSE for each iteration of sites
    GAM.RMSE <-mean(Tuc_NWsp_COM$Resid2)^.5 
    Lin.RMSE <-mean(Tuc_NWsp_COM$ResidLIN2)^.5 
    
    #bringing together a list of each site iteration MFJD
    GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
    Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)
    
  }#end of loop for site iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #

GAM.RMSE.MEANS.TUCsites<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.TUCsites<- Lin.RMSE.MEANS
GAM.SD.MEANS.TUCsites<- GAM.SD.MEANS
Lin.SD.MEANS.TUCsites<- Lin.SD.MEANS

GAM.RMSE.MEANS.TUCsites
Lin.RMSE.MEANS.TUCsites
GAM.SD.MEANS.TUCsites
Lin.SD.MEANS.TUCsites

GAM.RMSE.MEANS.TUCyear
Lin.RMSE.MEANS.TUCyear
GAM.SD.MEANS.TUCyear
Lin.SD.MEANS.TUCyear

############################ STOP TUCANNON ###










#LOOPS FOR Chiwawa (YEARS AND SITES SEPERATELY), run before running loops
library(mgcv)

data.SUB.NW.spCH <- data.NW.day.spCH
data.SUB.NW.flCH <- data.NW.day.flCH

data.SUB.day.spCH <- data.day.spCH
data.SUB.day.flCH <- data.day.flCH

#drop unused factor levels
data.SUB.day.spCH$SiteName<-factor(data.SUB.day.spCH$SiteName)
data.SUB.day.flCH$SiteName<-factor(data.SUB.day.flCH$SiteName)
data.SUB.day.COM.CH  <- rbind(data.SUB.day.spCH,data.SUB.day.flCH)
SpWe_Sites<-unique(data.SUB.day.COM.CH$SiteName)

SpWe_Years<-unique(data.SUB.day.COM.CH$Year)



# * Chiwawa Years Sensitivity ##################################

#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD<- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"

####Looping for number of years in prediction dataset##
Years <- c(2012:2017)
year.count<- c(6:3)
for (x in year.count){
  Years.list <- as.data.frame(replicate(100, sample(Years,x,replace = FALSE)))##replace 7 with y
  
  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
  #############Loop for iterations###
  loop_iterations<-c(1:100)
  for (y in loop_iterations) {
    #selecting years for iteration
    Years.used <- Years.list[,y]
    
    data.SUB.day.spCH$subsetY <- (data.SUB.day.spCH$Year) %in% (Years.used)
    data.SUB.day.spCH.x<- subset(data.SUB.day.spCH, subsetY == "TRUE")
    data.SUB.day.flCH$subsetY <- (data.SUB.day.flCH$Year) %in% (Years.used)
    data.SUB.day.flCH.x<- subset(data.SUB.day.flCH, subsetY == "TRUE")
    
    #spring model Chiwawa
    m1gsCH <- (gam(AvgDailyTemp ~ s(TAVGn5dC, k = 5) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3), data = data.SUB.day.spCH.x))
    
    m1gsLCH <- (lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC, data = data.SUB.day.spCH.x))
    
    #fall model Chiwawa
    m1gfCH <- (gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(JulianDate, by = TAVGn3dC, k = 5), data = data.SUB.day.flCH.x))
    
    m1gfLCH <- (lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = data.SUB.day.flCH.x))
    
    #Prediicting Chiwawa
    data.SUB.NW.spCH$predictGAM<- predict(m1gsCH, newdata = data.SUB.NW.spCH)
    data.SUB.NW.flCH$predictGAM<- predict(m1gfCH, newdata = data.SUB.NW.flCH)
    data.SUB.NW.spCH$predictGAM[data.SUB.NW.spCH$predictGAM < 0] <- 0
    data.SUB.NW.flCH$predictGAM[data.SUB.NW.flCH$predictGAM < 0] <- 0
    data.SUB.NW.spCH$predictLIN<- predict(m1gsLCH, newdata = data.SUB.NW.spCH)
    data.SUB.NW.flCH$predictLIN<- predict(m1gfLCH, newdata = data.SUB.NW.flCH)
    data.SUB.NW.spCH$predictLIN[data.SUB.NW.spCH$predictLIN < 0] <- 0
    data.SUB.NW.flCH$predictLIN[data.SUB.NW.flCH$predictLIN < 0] <- 0
    
    ###calculating residuals 
    data.SUB.NW.spCH$residGAM<- data.SUB.NW.spCH$DailyMean - data.SUB.NW.spCH$predictGAM
    data.SUB.NW.flCH$residGAM<- data.SUB.NW.flCH$DailyMean - data.SUB.NW.flCH$predictGAM
    data.SUB.NW.spCH$Resid2 <- ((data.SUB.NW.spCH$residGAM)^2)
    data.SUB.NW.flCH$Resid2 <- ((data.SUB.NW.flCH$residGAM)^2)
    data.SUB.NW.spCH$residLIN<- data.SUB.NW.spCH$DailyMean - data.SUB.NW.spCH$predictLIN
    data.SUB.NW.flCH$residLIN<- data.SUB.NW.flCH$DailyMean - data.SUB.NW.flCH$predictLIN
    data.SUB.NW.spCH$ResidLIN2 <- ((data.SUB.NW.spCH$residLIN)^2)
    data.SUB.NW.flCH$ResidLIN2 <- ((data.SUB.NW.flCH$residLIN)^2)
    
    ###CHAMP predicting NorWest COMBINED ALL YEAR DATA
    data.SUB.NW.COM.CH <- rbind(data.SUB.NW.spCH,data.SUB.NW.flCH)
    #RMSE for each iteration of sites
    GAM.RMSE <-mean(data.SUB.NW.COM.CH$Resid2)^.5 
    Lin.RMSE <-mean(data.SUB.NW.COM.CH$ResidLIN2)^.5 
    
    #bringing together a list of each site iteration MFJD
    GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
    Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)
    
  }#end of loop for year iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #Chiwawa

GAM.RMSE.MEANS.CHIWyear<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.CHIWyear<- Lin.RMSE.MEANS
GAM.SD.MEANS.CHIWyear<- GAM.SD.MEANS
Lin.SD.MEANS.CHIWyear<- Lin.SD.MEANS

GAM.RMSE.MEANS.CHIWyear
Lin.RMSE.MEANS.CHIWyear
GAM.SD.MEANS.CHIWyear
Lin.SD.MEANS.CHIWyear
#STOP FOR Chiwawa YEAR SENSITIVITY###


###### * Chiwawa Sites Sensitivity ####


#creating items for columns for values of iterations for different number of sites
GAM.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.RMSE.MEANS)<-"RMSE_mean"
Lin.RMSE.MEANS<- matrix(ncol=1, nrow=0)
colnames(Lin.RMSE.MEANS)<-"RMSE_mean"
GAM.SD.MEANS<- matrix(ncol=1, nrow=0)
colnames(GAM.SD.MEANS)<-"SD"
Lin.SD.MEANS <- matrix(ncol=1, nrow=0)
colnames(Lin.SD.MEANS)<-"SD"


#### Loop site sub sampling ##
site.countWE <- c(11)
for (i in site.countWE) {
  
  #making replicates for subsampling number of sites
  SitesList <- as.data.frame(replicate(100, sample(SpWe_Sites,i,replace = FALSE)))
  colnames(SitesList)<-paste0("x",c(loop_iterations))
  
  #creating RMSE item to be filled by iteration values
  GAM.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  Lin.RMSE.list <- matrix(ncol=1, nrow=0)
  colnames(GAM.RMSE.list)<-"RMSE_iterations"
  
  #number of iterations (must match replicate number above)
  loop_iterations<-c(1:100)
  for (z in loop_iterations) {
    #selecting sites for iteration
    We_Sites_sample <- SitesList[,z]
    
    #subsetting for iteration
    data.SUB.day.spCH$subset <- (data.SUB.day.spCH$SiteName) %in% (We_Sites_sample)
    data.SUB.day.spCH.x<- subset(data.SUB.day.spCH, subset == "TRUE")
    data.SUB.day.flCH$subset <- (data.SUB.day.flCH$SiteName) %in% (We_Sites_sample)
    data.SUB.day.flCH.x<- subset(data.SUB.day.flCH, subset == "TRUE")
   
    #spring model Chiwawa
    m1gsCH <- (gam(AvgDailyTemp ~ s(TAVGn5dC, k = 5) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3), data = data.SUB.day.spCH.x))
    
    m1gsLCH <- (lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC, data = data.SUB.day.spCH.x))
    
    #fall model Chiwawa
    m1gfCH <- (gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(JulianDate, by = TAVGn3dC, k = 5), data = data.SUB.day.flCH.x))
    
    m1gfLCH <- (lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = data.SUB.day.flCH.x))
    
    #Prediicting Chiwawa
    data.SUB.NW.spCH$predictGAM<- predict(m1gsCH, newdata = data.SUB.NW.spCH)
    data.SUB.NW.flCH$predictGAM<- predict(m1gfCH, newdata = data.SUB.NW.flCH)
    data.SUB.NW.spCH$predictGAM[data.SUB.NW.spCH$predictGAM < 0] <- 0
    data.SUB.NW.flCH$predictGAM[data.SUB.NW.flCH$predictGAM < 0] <- 0
    data.SUB.NW.spCH$predictLIN<- predict(m1gsLCH, newdata = data.SUB.NW.spCH)
    data.SUB.NW.flCH$predictLIN<- predict(m1gfLCH, newdata = data.SUB.NW.flCH)
    data.SUB.NW.spCH$predictLIN[data.SUB.NW.spCH$predictLIN < 0] <- 0
    data.SUB.NW.flCH$predictLIN[data.SUB.NW.flCH$predictLIN < 0] <- 0
    
    ###calculating residuals 
    data.SUB.NW.spCH$residGAM<- data.SUB.NW.spCH$DailyMean - data.SUB.NW.spCH$predictGAM
    data.SUB.NW.flCH$residGAM<- data.SUB.NW.flCH$DailyMean - data.SUB.NW.flCH$predictGAM
    data.SUB.NW.spCH$Resid2 <- ((data.SUB.NW.spCH$residGAM)^2)
    data.SUB.NW.flCH$Resid2 <- ((data.SUB.NW.flCH$residGAM)^2)
    data.SUB.NW.spCH$residLIN<- data.SUB.NW.spCH$DailyMean - data.SUB.NW.spCH$predictLIN
    data.SUB.NW.flCH$residLIN<- data.SUB.NW.flCH$DailyMean - data.SUB.NW.flCH$predictLIN
    data.SUB.NW.spCH$ResidLIN2 <- ((data.SUB.NW.spCH$residLIN)^2)
    data.SUB.NW.flCH$ResidLIN2 <- ((data.SUB.NW.flCH$residLIN)^2)
    
    ###CHAMP predicting NorWest COMBINED ALL YEAR DATA
    data.SUB.NW.COM.CH <- rbind(data.SUB.NW.spCH,data.SUB.NW.flCH)
    #RMSE for each iteration of sites
    GAM.RMSE <-mean(data.SUB.NW.COM.CH$Resid2)^.5 
    Lin.RMSE <-mean(data.SUB.NW.COM.CH$ResidLIN2)^.5 
    
    #bringing together a list of each site iteration Chiwawa
    GAM.RMSE.list<-rbind(GAM.RMSE.list, GAM.RMSE)
    Lin.RMSE.list<-rbind(Lin.RMSE.list, Lin.RMSE)
    
  }#end of loop for site iterations
  
  
  #Taking mean of all iterations for year/site# combination
  GAM.RMSE.mean<-mean(unlist(GAM.RMSE.list))
  Lin.RMSE.mean<-mean(unlist(Lin.RMSE.list))
  
  #standard deviatoin
  GAM.SD.mean<-sd(unlist(GAM.RMSE.list))
  Lin.SD.mean<-sd(unlist(Lin.RMSE.list))
  
  #adding means to vector of different number of sites (same number of years)
  GAM.RMSE.MEANS<-rbind(GAM.RMSE.MEANS, GAM.RMSE.mean)
  Lin.RMSE.MEANS<-rbind(Lin.RMSE.MEANS, Lin.RMSE.mean)
  
  GAM.SD.MEANS<-rbind(GAM.SD.MEANS, GAM.SD.mean)
  Lin.SD.MEANS<-rbind(Lin.SD.MEANS, Lin.SD.mean)
  
}#end of foorloop for site #

GAM.RMSE.MEANS.CHIWsites<- GAM.RMSE.MEANS
Lin.RMSE.MEANS.CHIWsites<- Lin.RMSE.MEANS
GAM.SD.MEANS.CHIWsites<- GAM.SD.MEANS
Lin.SD.MEANS.CHIWsites<- Lin.SD.MEANS

GAM.RMSE.MEANS.CHIWsites
Lin.RMSE.MEANS.CHIWsites
GAM.SD.MEANS.CHIWsites
Lin.SD.MEANS.CHIWsites

GAM.RMSE.MEANS.CHIWyear
Lin.RMSE.MEANS.CHIWyear
GAM.SD.MEANS.CHIWyear
Lin.SD.MEANS.CHIWyear

############################ STOP CHIWAWA DONE ###


###### ** RMSPE sensitivity analysis (run below section first) ####
GAM.RMSE.MEANS.WNsites
Lin.RMSE.MEANS.WNsites
GAM.SD.MEANS.WNsites
Lin.SD.MEANS.WNsites

GAM.RMSE.MEANS.WNyear
Lin.RMSE.MEANS.WNyear
GAM.SD.MEANS.WNyear
Lin.SD.MEANS.WNyear

GAM.RMSE.MEANS.CHIWsites
Lin.RMSE.MEANS.CHIWsites
GAM.SD.MEANS.CHIWsites
Lin.SD.MEANS.CHIWsites

GAM.RMSE.MEANS.CHIWyear
Lin.RMSE.MEANS.CHIWyear
GAM.SD.MEANS.CHIWyear
Lin.SD.MEANS.CHIWyear

GAM.RMSE.MEANS.MFJDsites
Lin.RMSE.MEANS.MFJDsites
GAM.SD.MEANS.MFJDsites
Lin.SD.MEANS.MFJDsites

GAM.RMSE.MEANS.MFJDyear
Lin.RMSE.MEANS.MFJDyear
GAM.SD.MEANS.MFJDyear
Lin.SD.MEANS.MFJDyear

GAM.RMSE.MEANS.TUCsites
Lin.RMSE.MEANS.TUCsites
GAM.SD.MEANS.TUCsites
Lin.SD.MEANS.TUCsites

GAM.RMSE.MEANS.TUCyear
Lin.RMSE.MEANS.TUCyear
GAM.SD.MEANS.TUCyear
Lin.SD.MEANS.TUCyear

###  Sensitivity done ###






#### FIGURES MAIN ####

# * Figure 3 ####
#Wenatchee  discharge and temp  #Same as Figure_A3
tiff(filename = "Figure_3.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "dailymeanCMS", "TAVGn5dC", plot.type="persp", xlab = "discharge (CMS)", ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "dailymeanCMS", "TAVGn3dC", plot.type="persp", xlab = "discharge (CMS)", ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()

#### * Figure 4 ####
#pred vs measured for all best models monthly and daily #
{
  tiff(filename = "Figure_4.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
  #win.graph(width =12, height = 6)
  par(mfrow= c(2,4), mar=c(2,2,1,1), oma=c(1,2.5,0.5,0.5),cex=.55, ps=10)
  plot(DailyMean~predictGAM,data = data.NW.day.COM, pch = ".", ylab = "Tw (°C)", xlab = "predicted Tw  (°C)", main = "Wenatchee")
  text(1.6,21, "GAM
       RMSE = 1.24
       NSC = 0.94", cex=0.9)
  abline(0,1, col = "red")
  plot(DailyMean~predictLIN, data = data.NW.day.CHCOM, pch = ".",  ylab = "Tw (°C)", xlab = "predicted Tw  (°C)", main = "Chiwawa")
  abline(0,1, col = "red")
  text(1.4,15.7, "Linear
       RMSE = 0.85
       NSC = 0.97", cex=0.9)
  plot(DailyMean~predictGAM, data = data.sub.NW.COM, pch = ".",  ylab = "Tw (°C)", xlab = "predicted Tw  (°C)", main = "M.F.J.D.")
  text(1.8,22.5, "GAM
       RMSE = 1.57
       NSC = 0.89", cex=0.9)
  abline(0,1, col = "red")
  plot(DailyMean~predictCHAMP, data = Tuc_NWmainstem_COM, pch = ".",  ylab = "Tw (°C)", xlab = "predicted Tw  (°C)", main = "Tucannon")
  text(1.8,22, "GAM
       RMSE = 0.94
       NSC = 0.96", cex=0.9)
  abline(0,1, col = "red")
  plot(AvgMonthTemp~predGAM, data = month.avgNW, ylab = "Tw (°C)", xlab = "predicted Tw  (°C)")
  abline(0,1, col = "red")
  text(1.9,19, "GAM
       RMSE = 1.01
       NSC = 0.96", cex=0.9)
  plot(AvgMonthTemp~predLIN, data = month.avgNW.CH, ylab = "Tw (°C)", xlab = "predicted Tw  (°C)")
  text(1.7,14.3, "Linear
       RMSE = 0.61
       NSC = 0.98", cex=0.9)
  abline(0,1, col = "red")
  plot(AvgMonthTemp~predGAM, data = month.avgNWMFJD,  ylab = "Tw (°C)",  xlab = "predicted Tw  (°C)")
  text(1.7,20, "GAM
       RMSE = 1.27
       NSC = 0.92", cex=0.9)
  abline(0,1, col = "red")
  plot(AvgMonthTemp~predCHAMP, data = month.avgTUC, ylab = "Tw (°C)",  xlab = "predicted Tw  (°C)")
  text(3.3,20.4, "GAM
       RMSE = 0.69
       NSC = 0.98", cex=0.9)
  abline(0,1, col = "red")
  mtext("Tw (°C)", side=2, outer=TRUE, cex=0.8, line=1.3)
  mtext("Monthly                                              Daily", side=2, outer=TRUE, cex=0.7)
  mtext("Predicted Tw (°C)", side=1, outer=TRUE,cex=0.8)
  dev.off()
}

#### * Figure 5 ####
#Fit daily by year plots###

#wenatchee
{data.day.COMBINED  <- data.day.COM
  tiff(filename = "Figure_5.tiff", width = 6.5, height = 8.5, units = 'in', res = 150)
  #win.graph(width = 6.5, height = 8)
  par(mfrow= c(9,6), mar=c(0,0,0,0), oma=c(4,5,0.5,0.5),cex=.65, ps=10)
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2003), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2003
       1.30")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2004),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2004
       1.42")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2005), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2005
       1.37")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2006), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2006
       1.25")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2007),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2007
       1.09")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2008),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2008
       1.20")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2009),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2009
       1.08")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2010), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2010
       1.11")
  plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2011), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2011
       1.29")
  plot.new()
  plot.new()
  plot.new()
  #chiwawa
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2003), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2003
       0.71")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2004), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2004
       0.85")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2005),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2005
       0.92")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2006), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2006
       0.70")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2007), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2007
       0.70")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2008),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2008
       0.79")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2009), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2009
       0.83")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2010), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2010
       0.85")
  plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2011), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2011
       1.17")
  plot.new()
  plot.new()
  plot.new()
  #MFJD
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==1997), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    1997
       1.71")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==1998),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    1998
       1.72")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==1999), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    1999
       1.57")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2000),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2000
       1.56")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2001),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2001
       1.71")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2002),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2002
       1.57")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2003), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2003
       1.76")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2004), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2004
       1.52")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2005), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2005
       1.41")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2006), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2006
       1.32")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2007), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2007
       1.49")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2008),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2008
       1.51")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2009), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2009
       1.54")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2010),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2010
       1.42")
  plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2011), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2011
       1.49")
  plot.new()
  plot.new()
  plot.new()
  #Tucannon
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2001),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  text(1,20, "    2001
       0.84")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2002),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2002
       1.09")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2003),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2003
       0.87")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2004), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2004
       0.86")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2005), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2005
       0.94")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2006), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  text(1,20, "    2006
       0.96")
  axis(side=1, seq(0,20, by=5))
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2007),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=2, seq(0,20, by=5))
  axis(side=1, seq(0,20, by=5))
  text(1,20, "    2007
       0.94")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2008),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=1, seq(0,20, by=5))
  text(1,20, "    2008
       1.09")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2009), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=1, seq(0,20, by=5))
  text(1,20, "    2009
       0.89")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2010), yaxt="n",xaxt="n",pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=1, seq(0,20, by=5))
  text(1,20, "    2010
       0.89")
  plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2011),yaxt="n",xaxt="n", pch=".", ylim =c(0,22), xlim = c (0,22))
  abline(0,1, col = "red", pch=".")
  axis(side=1, seq(0,20, by=5))
  plot.new()
  text(1,20, "    2011
       1.05")
  mtext("Tw (°C)", side=2, outer=TRUE,  line=3.6)
  mtext("Tucannon                                               M.F. John Day                            Chiwawa                           Wenatchee", side=2, outer=TRUE, cex=0.95, line=2.3)
  mtext("Predicted Tw (°C)", side=1, outer=TRUE, line=2.3)
  dev.off()
}




#### * Figure 6 ####
### Monthly values for sites 216 and 219 (those with most data in Wenatchee)#

{
  tiff(filename = "Figure_6.tiff", width = 6.5, height = 4.5, units = 'in', res = 150)
  #win.graph(width = 6.5, height = 4.5)
  par(mfrow= c(3,4), mar=c(2.2,2.2,1,.5), oma=c(2,2,0.1,0.1),cex=.65, ps=10)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==1 & SiteName == "216"), ylim =c (0,3), xlim = c(0,3), col = "red", pch = 16, main = "January")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==1 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==2 & SiteName == "216"), ylim =c (1,4), xlim = c(1,4), col = "red", pch = 16, main = "February")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==2 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==3 & SiteName == "216"), ylim =c (2,6), xlim = c(2,6), col = "red", pch = 16, main = "March")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==3 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==4 & SiteName == "216"), ylim =c (3.5,7), xlim = c(3.5,7), col = "red", pch = 16, main = "April")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==4 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==5 & SiteName == "216"), ylim =c (5,10), xlim = c(5,10), col = "red", pch = 16, main = "May")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==5 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==6 & SiteName == "216"), ylim =c (6,16), xlim = c(6,16), col = "red", pch = 16, main = "June")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==6 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==7 & SiteName == "216"), ylim =c (8,18), xlim = c(8,18), col = "red", pch = 16, main = "July")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==7 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==8 & SiteName == "216"), ylim =c (13,19), xlim = c(13,19), col = "red", pch = 16, main = "August")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==8 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==9 & SiteName == "216"), ylim =c (11,15), xlim = c(11,15), col = "red", pch = 16, main = "September")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==9 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==10 & SiteName == "216"), ylim =c (6,12), xlim = c(6,12), col = "red", pch = 16, main = "October")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==10 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==11 & SiteName == "216"), ylim =c (1,6), xlim = c(1,6), col = "red", pch = 16, main = "November")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==11 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==12 & SiteName == "216"),ylim =c (0,3), xlim = c(0,3), col = "red", pch = 16, main = "December")
  points(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==12 & SiteName == "219"), col = "blue4", pch = 17)
  abline(0,1)
  mtext("Tw (°C)", side=2, outer=TRUE)
  mtext("Predicted Tw (°C)", side=1, outer=TRUE)
  dev.off()
}
#### * Figure 7 ####
#site RMSE = 1.140 aprox 70 percentile, median 0.928
{data.day.COMBINED  <- data.day.COM

  tiff(filename = "Figure_7.tiff", width = 6.5, height = 5, units = 'in', res = 150)
  #win.graph(width = 6.5, height = 4.5)
  par(mfrow= c(2,1), mar=c(2,2,1,.5), oma=c(2,2,0.1,0.1),cex=.65, ps=10)
  #win.graph(width = 12, height = 12)
  #par(mfrow= c(2,1), ps=8,mar=c(2,2,1,1) )
  plot(AvgDailyTemp~DateNum,data= subset(data.day.COMBINED, SiteName == "NAU"),xaxt = "n", col = "black", ylab = "Tw (°C)", pch = 20, ylim=c(-3,19),cex=.4)
  axis(1, at=c(40909,41275,41640,42005,42370,42736), labels=c("1/1/2012","1/1/2013","1/1/2014","1/1/2015","1/1/2016","1/1/2017"))
  points((resid)~DateNum,data= subset(data.day.COMBINED, SiteName == "NAU"), pch = 20, col = "grey55",cex=.4)
  points(predict~DateNum,data= subset(data.day.COMBINED, SiteName == "NAU"), col = "red",cex=0.4)
  abline(h=0)
  abline(h=2,lty=3)
  abline(h=-2,lty=3)
  legend(40955,20.6, c("measured","predicted","residual"), pch = c(16,1,20), 
         col= c("black","red","grey55"),cex=1,bty="n",horiz=TRUE)
  text(42600, 17.8, labels="Fitted RMSE = 0.87",cex=0.9)
  text(42600, 19.1, labels="Upper Nason Creek",cex=0.9)
  
  ##RMSE = 0.91,aprox 17%, median site = 1.19, #White River
  plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "220"), xaxt = "n", col = "black", ylab = "Tw (°C)", pch = 20, ylim=c(-3,17),cex=.5)
  axis(1, at=c(37622,	37987,	38353,	38718,	39083,	39448,	39814,	40179,	40544), labels=c("1/1/2003","1/1/2004","1/1/2005","1/1/2006","1/1/2007","1/1/2008","1/1/2009","1/1/2010","1/1/2011"))
  points((residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "220"), pch = 20, col = "grey55",cex=.4)
  points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "220"), col = "red",cex=0.4)
  abline(h=0)
  abline(h=-2,lty=3)
  abline(h=2,lty=3)
  text(40730, 15.8,labels="Val. RMSE = 1.19",cex=0.9)
  text(40730, 17, labels="White River (220)",cex=0.9)
  mtext("Tw (°C)", side=2, outer=TRUE, line=.2)
  mtext("Date", side=1, outer=TRUE, line=.2)
  dev.off()
}

### * Fit by month/month plots ###########

#wenatchee
win.graph(width = 12, height = 8)
par(mfrow= c(3,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==1), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==2), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==3), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==4), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==5), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==6), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==7), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==8), ylim =c(0,22), xlim = c (0,22))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==9), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==10), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==11), ylim =c(0,20), xlim = c (0,20))
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==12),ylim =c(0,20), xlim = c(0,20))
abline(0,1, col = "red")
#Chiwawa
win.graph(width = 12, height = 8)
par(mfrow= c(3,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==1), ylim =c(0,22), xlim = c (0,22), main = "January")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==2), ylim =c(0,22), xlim = c (0,22), main = "February")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==3), ylim =c(0,22), xlim = c (0,22), main = "March")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==4), ylim =c(0,22), xlim = c (0,22), main = "April")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==5), ylim =c(0,22), xlim = c (0,22), main = "May")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==6), ylim =c(0,22), xlim = c (0,22), main = "June")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==7), ylim =c(0,22), xlim = c (0,22), main = "July")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==8), ylim =c(0,22), xlim = c (0,22), main = "August")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==9), ylim =c(0,22), xlim = c (0,22), main = "September")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==10), ylim =c(0,22), xlim = c (0,22), main = "October")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==11), ylim =c(0,22), xlim = c (0,22),main = "November")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW.CH, month==12),ylim =c(0,22), xlim = c(0,22), main = "December")
abline(0,1, col = "red")
#MFJD
win.graph(width = 12, height = 8)
par(mfrow= c(3,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==1), ylim =c(0,22), xlim = c (0,22), main = "January")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==2), ylim =c(0,22), xlim = c (0,22), main = "February")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==3), ylim =c(0,22), xlim = c (0,22), main = "March")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==4), ylim =c(0,22), xlim = c (0,22), main = "April")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==5), ylim =c(0,22), xlim = c (0,22), main = "May")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==6), ylim =c(0,22), xlim = c (0,22), main = "June")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==7), ylim =c(0,22), xlim = c (0,22), main = "July")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==8), ylim =c(0,22), xlim = c (0,22), main = "August")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==9), ylim =c(0,22), xlim = c (0,22), main = "September")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==10), ylim =c(0,22), xlim = c (0,22), main = "October")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==11), ylim =c(0,22), xlim = c (0,22),main = "November")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNWMFJD, month==12),ylim =c(0,22), xlim = c(0,22), main = "December")
abline(0,1, col = "red")


###predictions vs measured over time for single site graphs  
data.NW.day.COM <- data.NW.day.COM[order(data.NW.day.COM$DateNum),]
WenDailyResid.mod <-gam(residGAM~s(DateNum, k=30),data=data.NW.day.COM)
library(visreg)
visreg(WenDailyResid.mod)
abline(h=0)

data.NW.day.CHCOM <- data.NW.day.CHCOM[order(data.NW.day.CHCOM$DateNum),]
ChiwDailyResid.mod <-gam(residGAM~s(DateNum, k=30),data=data.NW.day.CHCOM)
library(visreg)
visreg(ChiwDailyResid.mod)
abline(h=0)

data.sub.NW.COM <- data.sub.NW.COM[order(data.sub.NW.COM$DateNum),]
WMFJDDailyResid.mod <-gam(residGAM~s(DateNum, k=30),data=data.sub.NW.COM)
library(visreg)
visreg(WMFJDDailyResid.mod)
abline(h=0)

Tuc_NWmainstem_COM <- Tuc_NWmainstem_COM[order(Tuc_NWmainstem_COM$DateNum),]
TucDailyResid.mod <-gam(residCHAMP~s(DateNum, k=30),data=Tuc_NWmainstem_COM)
library(visreg)
visreg(TucDailyResid.mod)
abline(h=0)
plot(residCHAMP~JulianDate,Tuc_NWmainstem_COM)
TucDailyResid.mod <-gam(residCHAMP~s(JulianDate, k=10),data=Tuc_NWmainstem_COM)

#Peshastin
data.NW.day.COM <- data.NW.day.COM[order(data.NW.day.COM$DateNum),]

data.day.flSub<-data.day.fl[,1:91]
data.day.spSub<-data.day.sp[,1:91]
data.day.spSub$predict<-predict(m1gs)
data.day.flSub$predict<-predict(m1gf)
data.day.COMBINED<-rbind(data.day.spSub,data.day.flSub)
data.day.COMBINED <- data.day.COMBINED[order(data.day.COMBINED$DateNum),]
table(data.day.COMBINED$SiteName)

#find out percentiles of sites
#NorWest
med<-as.data.frame.table(tapply(data.NW.day.COM$Resid2,data.NW.day.COM$PERMA_FID.x,mean))
colnames(med)<-c("Site","Resid2")
med$RMSE<-med$Resid2^.5
median(med$RMSE)
mean(med$RMSE)
medNWSites<-as.data.frame.table(table(data.NW.day.COM$PERMA_FID.x))
med<-merge(med,medNWSites,by.x = "Site", by.y = "Var1")
med <- med[order(med$RMSE),]
quantile(med$RMSE,  probs = c(10,16,20,50,80,90)/100)
#NorWest
medCh<-as.data.frame.table(tapply(data.day.COMBINED$resid2,data.day.COMBINED$SiteName,mean))
colnames(medCh)<-c("Site","Resid2")
medCh$RMSE<-medCh$Resid2^.5
median(medCh$RMSE)
mean(medCh$RMSE)
medSites<-as.data.frame.table(table(data.day.COMBINED$SiteName))
medCh<-merge(medCh,medSites,by.x = "Site", by.y = "Var1")
quantile(medCh$RMSE,  probs = c(10,20,50,60,70,80,90)/100)
medCh <- medCh[order(medCh$RMSE),]
plot(AvgDailyTemp~DateNum,data= subset(data.day.COMBINED, SiteName == "WC503432-000049"),xaxt = "n", typ = "l", col = "black", ylab = "Tw (°C)", pch = 16, ylim=c(-3,18))
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "216"), xaxt = "n",typ = "l", col = "black", ylab = "Tw (°C)", pch = 16, ylim=c(-3,21))



#### * Other site daily predictions graphs ####

win.graph(width = 12, height = 12)
par(mfrow= c(2,1), ps=8,mar=c(2,2,1,1) )
plot(AvgDailyTemp~JulianDate,data= subset(data.day.COMBINED, SiteName == "NAU"),col=Year,pch=16, xaxt = "n", ylab = "Tw (°C)", ylim=c(-3,17),cex=.5)
points(predict~JulianDate,data= subset(data.day.COMBINED, SiteName == "NAU"), col = Year,cex=0.4)
plot(DailyMean~Julian,data= subset(data.NW.day.COM, PERMA_FID.x == "220" & year >2006),col=year,pch=16, xaxt = "n", ylab = "Tw (°C)", ylim=c(-3,17),cex=.5)
points(predictGAM~Julian,data= subset(data.NW.day.COM, PERMA_FID.x == "220" & year >2006), col = year,cex=0.4)

win.graph(width = 12, height = 8)
par(cex=1, ps=10)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "216"), typ = "l", col = "black", ylab = "Tw (°C)", pch = 16, ylim=c(-3,21))
points((residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "216"), pch = ".", col = "grey55",cex=1)
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "216"), col = "red",cex=0.6)
abline(h=0)
legend(40450,22.5, c("measured","predicted","error"), pch = c(NA,1,20), lty = c(1,NA,NA),
       col= c("black","red","grey55"))

win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "23758"), col = "red", ylab = "Tw (°C)", pch = 16)
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "23758"), col = "blue")
points(abs(residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "23758"), typ = "l")


#Nason
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "219"), col = "red", ylab = "Tw (°C)", pch = 16, ylim=c(0,20))
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "219"), col = "blue")
points(abs(residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "219"), typ = "l")

#Chiwawa
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "222"), col = "red", ylab = "Tw (°C)", pch = 16, ylim=c(-3,16))
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "222"), col = "blue")
points((residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "222"), pch=20, col ="grey40")
abline(h=0)

#Chiwawa Chiwawa model
colnames(data.NW.day.CHCOM)
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.CHCOM, PERMA_FID.x == "222"), col = "black", ylab = "Tw (°C)", pch = 16, ylim=c(-3,16))
points(predictLIN~DateNum,data= subset(data.NW.day.CHCOM, PERMA_FID.x == "222"), col = "red")
points((residLIN)~DateNum,data= subset(data.NW.day.CHCOM, PERMA_FID.x == "222"), col ="grey40")
abline(h=0)

#Mission
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "214"), col = "red", ylab = "Tw (°C)", pch = 16, ylim=c(0,24))
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "214"), col = "blue")
points(abs(residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "214"), typ = "l")

#Little Wenatchee
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "221"), col = "red", ylab = "Tw (°C)", pch = 16, ylim=c(0,18))
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "221"), col = "blue")
points(abs(residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "221"), pch=20, col ="grey40")

#Mainstem
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "208"), ylim=c(-5,30),col = "red", ylab = "Tw (°C)", pch = 16)
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "208"), col = "blue")
points((residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "208"), pch=20, col ="grey40")
abline(h=0)

win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "23749"), col = "red", ylab = "Tw (°C)", pch = 16, ylim = c(0,25))
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "23749"), col = "blue")
points(abs(residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "23749"), pch=20, col ="grey40")

#White River
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "220"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(0,17))
points(predictGAM~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "220"), col = "blue")
points(abs(residGAM)~DateNum,data= subset(data.NW.day.COM, PERMA_FID.x == "220"), pch=20, col ="grey40")


colnames(Tuc_NW_mainstem)
Tuc_NW_mainstem <- Tuc_NW_mainstem[order(data.NW.day.COM$DateNum),]
library(plyr)
count(Tuc_NWmainstem_COM, "PERMA_FID.x")
tapply(Tuc_NWmainstem_COM$catch_area,Tuc_NWmainstem_COM$PERMA_FID.x,mean)/10000000
tapply(Tuc_NWmainstem_COM$PERMA_FID.x,Tuc_NWmainstem_COM$month,unique)

colnames(Tuc_NWmainstem_COM)

#Tucannon
#Area 129.429169
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "20036"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,25))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "20036"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "20036"), pch=20, col ="grey40")
abline(h=0)
#Area 105.024923
win.graph(width = 12, height = 8)
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10167"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,23))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10167"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10167"), pch=20, col ="grey40")
abline(h=0)
win.graph(width = 12, height = 8)#Area 10168
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10168"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,23))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10168"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10168"), pch=20, col ="grey40")
abline(h=0)
#all year
win.graph(width = 12, height = 8)#Area 24.599882
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10177"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,23))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10177"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10177"), pch=20, col ="grey40")
abline(h=0)
win.graph(width = 12, height = 8)#Area 42.041085 #best
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "169"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,23))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "169"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "169"), pch=20, col ="grey40")
abline(h=0)
win.graph(width = 12, height = 8)#Area 17.476594
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "21094"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,15))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "21094"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "21094"), pch=20, col ="grey40")
abline(h=0)
win.graph(width = 12, height = 8)#Area 41.135626
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10173"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,23))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10173"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10173"), pch=20, col ="grey40")
abline(h=0)
win.graph(width = 12, height = 8)#Area 22.516505
plot(DailyMean~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10180"), col = "red", ylab = "Tw (°C)", pch = 16,ylim = c(-3,23))
points(predictCHAMP~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10180"), col = "blue")
points((residCHAMP)~DateNum,data= subset(Tuc_NWmainstem_COM, PERMA_FID.x == "10180"), pch=20, col ="grey40")
abline(h=0)






#### SUPPLEMENTARY FIGURES ####
#### * Environmental comparison graphs #######################
#Discharge data
Dv.day <- read.csv(("Wen_discharge_all.csv"), stringsAsFactors = FALSE)
#Environmental covariates
Steph.env<- read.csv("Steph_env_8d.csv")
colnames(Dv.day)
colnames(Steph.env)
colnames(data.day)
colnames(data.NW.day)

Steph.env$TAVGC<-(Steph.env$TAVG - 32)*5/9
Dv.dayNW <-subset(Dv.day, year < 2012 & year > 2002)
Dv.dayCH <-subset(Dv.day, year > 2011 & year < 2018)
Steph.envNW <-subset(Steph.env, year < 2012 & year > 2002)
Steph.envCH <-subset(Steph.env, year > 2011& year < 2018)

library(mgcv)
smoothCH.Tw <- (gam(AvgDailyTemp~s(JulianDate, k =10), data=data.day))
smoothNW.Ta <- (gam(DailyMean~s(Julian, k =10), data=data.NW.day))
data.day$smoothCH <- predict(smoothCH.Tw, newdata=data.day)
data.NW.day$smoothNW <- predict(smoothNW.Ta, newdata=data.NW.day)

smoothCH <- (gam(dailymeanCMS~s(julian, k =10), data=Dv.dayCH))
smoothNW <- (gam(dailymeanCMS~s(julian, k =10), data=Dv.dayNW))
Dv.day$smoothCH <- predict(smoothCH, newdata=Dv.day)
Dv.day$smoothNW <- predict(smoothNW, newdata=Dv.day)

smoothCH.T <- (gam(TAVGC~s(Julian, k =10), data=Steph.envCH))
smoothNW.T <- (gam(TAVGC~s(Julian, k =10), data=Steph.envNW))
Steph.env$smoothCH.T <- predict(smoothCH.T, newdata=Steph.env)
Steph.env$smoothNW.T <- predict(smoothNW.T, newdata=Steph.env)

smoothCH.S <- (gam(SNWD~s(Julian, k =15), data=Steph.envCH))
smoothNW.S <- (gam(SNWD~s(Julian, k =15), data=Steph.envNW))
Steph.env$smoothCH.S <- predict(smoothCH.S, newdata=Steph.env)
Steph.env$smoothNW.S <- predict(smoothNW.S, newdata=Steph.env)


###plots
tiff(filename = "Figure_S1.tiff", width = 6.5, height = 4.7, units = 'in', res = 200)
par(cex=.7, ps=10)
plot(TAVGC~Julian, data = subset(Steph.env, year == 2003),xlim = c(0,366),ylim=c(-10,25),col = "blue4",ylab = "Air temperature (°C)", xlab = "Day of year", type ="l", lwd =1)
lines(TAVGC~Julian, pch = 16, col = "blue3", lwd =1,data = subset(Steph.env, year == 2004))
lines(TAVGC~Julian, pch = 16, col = "blue", lwd =1,data = subset(Steph.env, year == 2005))
lines(TAVGC~Julian, pch = 16, col = "dodgerblue3", lwd =1,data = subset(Steph.env, year == 2006))
lines(TAVGC~Julian, pch = 16, col = "dodgerblue", lwd =1,data = subset(Steph.env, year == 2007))
lines(TAVGC~Julian, pch = 16, col = "deepskyblue3", lwd =1,data = subset(Steph.env, year == 2008))
lines(TAVGC~Julian, pch = 16, col = "deepskyblue", lwd =1,data = subset(Steph.env, year == 2009))
lines(TAVGC~Julian, pch = 16, col = "skyblue3", lwd =1,data = subset(Steph.env, year == 2010))
lines(TAVGC~Julian, pch = 16, col = "skyblue", lwd =1,data = subset(Steph.env, year == 2011))
lines(TAVGC~Julian, pch = 16, col = "red", lwd =1,data = subset(Steph.env, year == 2012))
lines(TAVGC~Julian, pch = 16, col = "red1", lwd =1,data = subset(Steph.env, year == 2013))
lines(TAVGC~Julian, pch = 16, col = "red2", lwd =1,data = subset(Steph.env, year == 2014))
lines(TAVGC~Julian, pch = 16, col = "red3", lwd =1,data = subset(Steph.env, year == 2015))
lines(TAVGC~Julian, pch = 16, col = "red4", lwd =1,data = subset(Steph.env, year == 2016))
lines(TAVGC~Julian, pch = 16, col = "firebrick4", lwd =1,data = subset(Steph.env, year == 2017))
points(smoothCH.T~Julian, pch = 16, col = "red",data = Steph.env)
points(smoothNW.T~Julian, pch = 16, col = "blue",data = Steph.env)
legend(337,26.4, c("2003","2004","2005","2006","2007","2008","2009", "2010","2011","2012","2013","2014","2015","2016","2017"), lty=c(1,1,1,1,1,1,1,1,1),lwd =c(1,1,1,1,1,1,1,1,1), 
       col= c("blue4","blue3","blue","dodgerblue3","dodgerblue","deepskyblue3","deepskyblue","skyblue3","skyblue","red","red1","red2","red3","red4","firebrick4"),cex=0.9)
abline(h=0)
dev.off()

tiff(filename = "Figure_S2.tiff", width = 6.5, height = 4.7, units = 'in', res = 200)
par(cex=.7, ps=10)
plot(SNWD~Julian, data = subset(Steph.env, year == 2003),xlim = c(0,366),ylim=c(0,160),col = "blue4",ylab = "Snow depth (cm)", xlab = "Day of year", type ="l", lwd =1)
lines(SNWD~Julian, pch = 16, col = "blue3", lwd =1,data = subset(Steph.env, year == 2004))
lines(SNWD~Julian, pch = 16, col = "blue", lwd =1,data = subset(Steph.env, year == 2005))
lines(SNWD~Julian, pch = 16, col = "dodgerblue3", lwd =1,data = subset(Steph.env, year == 2006))
lines(SNWD~Julian, pch = 16, col = "dodgerblue", lwd =1,data = subset(Steph.env, year == 2007))
lines(SNWD~Julian, pch = 16, col = "deepskyblue3", lwd =1,data = subset(Steph.env, year == 2008))
lines(SNWD~Julian, pch = 16, col = "deepskyblue", lwd =1,data = subset(Steph.env, year == 2009))
lines(SNWD~Julian, pch = 16, col = "skyblue3", lwd =1,data = subset(Steph.env, year == 2010))
lines(SNWD~Julian, pch = 16, col = "skyblue", lwd =1,data = subset(Steph.env, year == 2011))
lines(SNWD~Julian, pch = 16, col = "red", lwd =1,data = subset(Steph.env, year == 2012))
lines(SNWD~Julian, pch = 16, col = "red1", lwd =1,data = subset(Steph.env, year == 2013))
lines(SNWD~Julian, pch = 16, col = "red2", lwd =1,data = subset(Steph.env, year == 2014))
lines(SNWD~Julian, pch = 16, col = "red3", lwd =1,data = subset(Steph.env, year == 2015))
lines(SNWD~Julian, pch = 16, col = "red4", lwd =1,data = subset(Steph.env, year == 2016))
lines(SNWD~Julian, pch = 16, col = "firebrick4", lwd =1,data = subset(Steph.env, year == 2017))
points(smoothCH.S~Julian, pch = 16, col = "red",data = Steph.env)
points(smoothNW.S~Julian, pch = 16, col = "blue",data = Steph.env)
legend(200,166.5, c("2003","2004","2005","2006","2007","2008","2009", "2010","2011","2012","2013","2014","2015","2016","2017"), lty=c(1,1,1,1,1,1,1,1,1),lwd =c(1,1,1,1,1,1,1,1,1), 
       col= c("blue4","blue3","blue","dodgerblue3","dodgerblue","deepskyblue3","deepskyblue","skyblue3","skyblue","red","red1","red2","red3","red4","firebrick4"))
dev.off()


win.graph(width =11, height = 8)
tiff(filename = "Figure_S3.tiff", width = 6.5, height = 4.7, units = 'in', res = 200)
par(cex=.7, ps=10)
plot(dailymeanCMS~julian, data = subset(Dv.day, year == 2003),xlim = c(0,366),ylim=c(0,700),col = "blue4",ylab = "Discharge (CMS)", xlab = "Day of year", type ="l", lwd = 1)
lines(dailymeanCMS~julian, pch = 16, col = "blue3", lwd = 1,data = subset(Dv.day, year == 2004))
lines(dailymeanCMS~julian, pch = 16, col = "blue", lwd = 1,data = subset(Dv.day, year == 2005))
lines(dailymeanCMS~julian, pch = 16, col = "dodgerblue3", lwd = 1,data = subset(Dv.day, year == 2006))
lines(dailymeanCMS~julian, pch = 16, col = "dodgerblue", lwd = 1,data = subset(Dv.day, year == 2007))
lines(dailymeanCMS~julian, pch = 16, col = "deepskyblue3", lwd = 1,data = subset(Dv.day, year == 2008))
lines(dailymeanCMS~julian, pch = 16, col = "deepskyblue", lwd = 1,data = subset(Dv.day, year == 2009))
lines(dailymeanCMS~julian, pch = 16, col = "skyblue3", lwd = 1,data = subset(Dv.day, year == 2010))
lines(dailymeanCMS~julian, pch = 16, col = "skyblue", lwd = 1,data = subset(Dv.day, year == 2011))
lines(dailymeanCMS~julian, pch = 16, col = "red", lwd = 1,data = subset(Dv.day, year == 2012))
lines(dailymeanCMS~julian, pch = 16, col = "red1", lwd = 1,data = subset(Dv.day, year == 2013))
lines(dailymeanCMS~julian, pch = 16, col = "red2", lwd = 1,data = subset(Dv.day, year == 2014))
lines(dailymeanCMS~julian, pch = 16, col = "red3", lwd = 1,data = subset(Dv.day, year == 2015))
lines(dailymeanCMS~julian, pch = 16, col = "red4", lwd = 1,data = subset(Dv.day, year == 2016))
lines(dailymeanCMS~julian, pch = 16, col = "firebrick4", lwd = 1,data = subset(Dv.day, year == 2017))
points(smoothCH~julian, pch = 16, col = "red", lwd =2,data = Dv.day)
points(smoothNW~julian, pch = 16, col = "blue", lwd =2,data = Dv.day)
legend(337,728, c("2003","2004","2005","2006","2007","2008","2009", "2010","2011","2012","2013","2014","2015","2016","2017"), lty=c(1,1,1,1,1,1,1,1,1),lwd =c(1,1,1,1,1,1,1,1,1), 
       col= c("blue4","blue3","blue","dodgerblue3","dodgerblue","deepskyblue3","deepskyblue","skyblue3","skyblue","red","red1","red2","red3","red4","firebrick4"), cex=0.9)
dev.off()

#### * Figures S4 and S5 ####

####Figure S4
#Finding the average day of peak stream temperature during fitting years (CHAMP) to split model at
data.day$predSmooth <- predict(gam(AvgDailyTemp~s(JulianDate, k=10), data = data.day))
subset(data.day,predSmooth==max(data.day$predSmooth)) #julian date 16 is the date of max predicted temps
{
  tiff(filename = "Figure_S4.tiff", width = 4, height = 3, units = 'in', res = 200)
  par(cex=.7, ps=10)
  plot(data.day$JulianDate,data.day$AvgDailyTemp,ylab = "Tw (°C)", xlab = "Day of year",pch=".")
  points(data.day$JulianDate,data.day$predSmooth, pch = 16, col = "red")
  abline(v = 216, col = "blue")
dev.off()
}

### Figure S5
####Crumpled cutain effect graph
gsAE1 <- (gam(AvgDailyTemp ~ s(ae, by = TAVGn5dC), data = data.day.sp))
gsAE2 <- (gam(AvgDailyTemp ~ s(ae, by = TAVGn5dC, k = 3), data = data.day.sp))


tiff(filename = "Figure_S5.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(gsAE1, "ae", "TAVGn5dC", plot.type="persp", xlab = "Elevation catchment (m)", ylab = "T5a (°C)", zlab = "Tw (°C)",nn=22, main= "a")  
visreg2d(gsAE2, "ae", "TAVGn5dC", plot.type="persp",xlab = "Elevation catchment (m)", ylab = "T5a (°C)", zlab = "Tw (°C)",nn=22, main= "b")  
dev.off()




################################# APPENDIX GRAPHS ###################################
library(mgcv)
library(visreg)

###### * Climate vs Climate #######
#Wenatchee julian and temp graph
tiff(filename = "Figure_A2.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "JulianDate", "TAVGn5dC", plot.type="persp", xlab = "day of year", ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "JulianDate", "TAVGn3dC", plot.type="persp", xlab = "day of year", ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()

#Wenatchee temp vs temp graph
tiff(filename = "Figure_A1.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "Tchange5C", "TAVGn5dC", plot.type="persp", xlab = "T?? (°C)", ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "Tchange3C", "TAVGn3dC", plot.type="persp", xlab = "T?? (°C)", ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()

#Wenatchee  discharge and temp
tiff(filename = "Figure_A3.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "dailymeanCMS", "TAVGn5dC", plot.type="persp", xlab = "discharge (CMS)", ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "dailymeanCMS", "TAVGn3dC", plot.type="persp", xlab = "discharge (CMS)", ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()


#Tuc snow depth and temp
tiff(filename = "Figure_A4.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(TucGsp, "JulianDate", "A1Snow", plot.type="persp",  xlab = "day of year",ylab = "April 1st snowpack depth (cm)", zlab = "Tw (°C)", main = "spring",nn=22)  
plot.new()
dev.off()

#MFJD snow depth and temp
tiff(filename = "Figure_A5.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
plot.new()
visreg2d(MFJDgfCH, "SNWD", "TAVGn3d", plot.type="persp",  xlab = "snowpack depth (cm)",ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()



#### * Climate vs Spatial #######
win.graph(width = 12, height = 8)

#Elevation (catch)	Day Wenatchee GAM
tiff(filename = "Figure_A6.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "JulianDate", "ae", plot.type="persp",  xlab = "day of year",ylab = "elevation catchment (m)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "JulianDate", "ae", plot.type="persp",  xlab = "day of year",ylab = "elevation catchment (m)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()

#Elevation (change)	T avg.  Wenatchee GAM
tiff(filename = "Figure_A7.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "Echange2", "TAVGn5dC",plot.type="persp", xlab = "elevation change (m)",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22) 
visreg2d(m1gf, "Echange2", "TAVGn3dC",plot.type="persp", xlab = "elevation change (m)",ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22) 
dev.off()

#Area	Elevation (catch) vs catchment area Wenatchee GaM
tiff(filename = "Figure_A8.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "catch_area", "ae", plot.type="persp", xlab = "catchment area (m^2)",ylab = "elevation catchment (m)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "catch_area", "ae", plot.type="persp", xlab = "catchment area (m^2)",ylab = "elevation catchment (m)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()

#Area	Vs T avg. Tucannon Linear
tiff(filename = "Figure_A9.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gsL, "catch_area", "TAVGn5dC", plot.type="persp",xlab = "catchment area (m^2)",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  #not sure
visreg2d(m1gfL, "catch_area", "TAVGn3dC", plot.type="persp",xlab = "catchment area (m^2)",ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  #not sure
dev.off()


#Area	Discharge #Wenatchee Linear
tiff(filename = "Figure_A10.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gsL, "catch_area", "dailymeanCMS", plot.type="persp",xlab = "catchment area (m^2)",ylab = "discharge", zlab = "Tw (°C)", main = "spring",nn=22)  #not so sure about this
visreg2d(m1gfL, "catch_area", "dailymeanCMS", plot.type="persp",xlab = "catchment area (m^2)",ylab = "discharge", zlab = "Tw (°C)", main = "fall",nn=22)  #not so sure about this
dev.off()

#Elevation (catch)	Snow Depth
tiff(filename = "Figure_A11.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
plot.new()
visreg2d(m1gfLCH, "ae", "SNWD", plot.type="persp",xlab = "elevation catchment (m)",ylab = "snow depth (cm)", zlab = "Tw (°C)", main = "fall",nn=22)  #not sure this helps much
dev.off()
#no spring

#Base Flow Index	T avg. MFJD(spring), Wenatchee(fall) 
tiff(filename = "Figure_A12.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(MFJDgsCH, "BFI", "TAVGn5d",plot.type="persp", xlab = "BFI",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "BFI", "TAVGn3dC",plot.type="persp", xlab = "BFI",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()

#Forest cover (catch)	T avg. MFJD GAM
tiff(filename = "Figure_A13.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(MFJDgsCH, "TreeDens_1", "TAVGn5d", plot.type="persp", xlab = "forest cover proportion",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22)  #not sure this helps much
visreg2d(MFJDgfCH, "TreeDens_1", "TAVGn3d", plot.type="persp", xlab = "forest cover proportion",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "fall",nn=22)  #not sure this helps much
dev.off()

#Slope vs	T avg. Wenatchee Linear
tiff(filename = "Figure_A14.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gsL, "SLOPE", "TAVGn5dC", plot.type="persp", xlab = "reach slope",ylab = "T Avg5 (°C)", zlab = "Tw (°C)", main = "spring",nn=22) 
visreg2d(m1gfL, "SLOPE", "TAVGn3dC", plot.type="persp", xlab = "reach slope",ylab = "T Avg3 (°C)", zlab = "Tw (°C)", main = "fall",nn=22) 
dev.off()

#Lakes	T avg.Wenatchee
tiff(filename = "Figure_A15.tiff", width = 6.5, height = 3.25, units = 'in', res = 200)
par(mfrow= c(1,2), mar=c(1,1,1,1), cex=.7, ps=10)
visreg2d(m1gs, "JulianDate", "Lake_perc", plot.type="persp", ylab = "lake proportion",xlab = "day of year", zlab = "Tw (°C)", main = "spring",nn=22)  
visreg2d(m1gf, "JulianDate", "Lake_perc", plot.type="persp", ylab = "lake proportion",xlab = "day of year", zlab = "Tw (°C)", main = "fall",nn=22)  
dev.off()


#Glaciers	Vs  T avg.
  #not utilized








#### SAVE DATA ####
#save.image(file = "All_models.RData")
