#### SET WORKING DIRECTORY ####
WD <-"C:/Users/deraj/Documents/South Fork/Temperature models/Manuscript_clean/"
#put all CSV files in directory
setwd(WD)

#### ............... ####
#### WENATCHEE & CHIWAWA MODELS ####

# * Load data ####
setwd(WD)
#write.csv(Monthly_Table,"monthly_RMSE_NSC.csv")
#norwest data for predicting
data.NW.day <- read.csv("Wen_NWdata_day.csv")
#CHAMP and MRR data for model fitting
data.day <- read.csv("Wen_data_day.csv")
summary(data.NW.day$year)
#adding variables that had different names
data.NW.day$JulianDate <- data.NW.day$Julian
data.NW.day$AvgDailyTemp <- data.NW.day$DailyMean


FittingSites <- as.data.frame(unique(data.day$SiteName))
colnames(FittingSites)<- "SiteNameUsed"


##
library(mgcv)
library(gamm4)
library(visreg)

#Max temp at day 216, used to split fall and spring periods
win.graph(5, height = 4)
par(cex=.8, ps=11)
data.day$predSmooth <- predict(gam(AvgDailyTemp~s(JulianDate, k=10), data = data.day))
plot(data.day$JulianDate,data.day$AvgDailyTemp,ylab = "Tw (C°)", xlab = "day of year",pch=".")
points(data.day$JulianDate,data.day$predSmooth, pch = 16, col = "red")
abline(v = 216, col = "blue")


#### * Clean data  ####
#remove Chikamin creek site from Nor West dataset that is clear outlier,  likely groundwater site
win.graph(5, height = 4)
plot(data.NW.day$Julian,data.NW.day$AvgDailyTemp, pch =".", col = "black", ylab = "stream temperature (C°)", xlab = "day of year")
points(DailyMean~Julian,data=subset(data.NW.day, PERMA_FID.x == "24155"),pch=".",  col="red")
BadSites<-c("24155")#this site is a clear outlier with max temps of 6C in the summer, must be springwater site
data.NW.day2 <- subset(data.NW.day, !(PERMA_FID.x %in% BadSites))


###converting temp variables to Celsius from Farenheight
data.NW.day2$TAVGn7dC<-(data.NW.day2$TAVGn7d - 32)*5/9
data.NW.day2$TAVGn5dC<-(data.NW.day2$TAVGn5d - 32)*5/9
data.NW.day2$TAVGn3dC<-(data.NW.day2$TAVGn3d - 32)*5/9
data.NW.day2$TAVGn1dC<-(data.NW.day2$TAVGn1d - 32)*5/9
data.NW.day2$Tchange7C<-(data.NW.day2$Tchange7)*5/9
data.NW.day2$Tchange5C<-(data.NW.day2$Tchange5)*5/9
data.NW.day2$Tchange3C<-(data.NW.day2$Tchange3)*5/9
data.NW.day2$Tchange1C<-(data.NW.day2$Tchange1)*5/9

data.day$TAVGn7dC<-(data.day$TAVGn7d - 32)*5/9
data.day$TAVGn5dC<-(data.day$TAVGn5d - 32)*5/9
data.day$TAVGn3dC<-(data.day$TAVGn3d - 32)*5/9
data.day$TAVGn1dC<-(data.day$TAVGn1d - 32)*5/9
data.day$Tchange7C<-(data.day$Tchange7)*5/9
data.day$Tchange5C<-(data.day$Tchange5)*5/9
data.day$Tchange3C<-(data.day$Tchange3)*5/9
data.day$Tchange1C<-(data.day$Tchange1)*5/9


##
#Will also do analysis just for Chiwawa River
data.dayCH <- subset(data.day, TempRegion == "Chiwawa River")
data.NW.dayCH <- subset(data.NW.day2, TempRegion == "Chiwawa River")

#splitting data into spring and fall 
data.NW.day.sp <- subset(data.NW.day2, Julian < 217)
data.NW.day.fl <- subset(data.NW.day2, Julian > 216)
data.day.sp <- subset(data.day, JulianDate < 217)
data.day.fl <- subset(data.day, JulianDate > 216)

data.NW.day.spCH <- subset(data.NW.dayCH, Julian < 217)
data.NW.day.flCH <- subset(data.NW.dayCH, Julian > 216)
data.day.spCH <- subset(data.dayCH, JulianDate < 217)
data.day.flCH <- subset(data.dayCH, JulianDate > 216)








##### * Model fitting and metrics Wenatchee####

library(ISLR)
library(boot)
##### Fitting models
#spring GAM model
library(mgcv)
library(visreg)
library(gamm4)
library(lme4)

#Spring GAM
m1gs <- (gam(AvgDailyTemp ~ s(TAVGn5dC, k = 4) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVGn5dC, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+s(BFI, by = TAVGn5dC, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3)+s(for_cover, by = TAVGn5dC, k = 3), data = data.day.sp))
summary(m1gs)

#Spring linear
m1gsL <- (lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn5dC+catch_area*dailymeanCMS+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC+for_cover*TAVGn5dC, data = data.day.sp))
summary(m1gsL)
#fall model
m1gf <- (gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = TAVGn3dC, k = 3)+s(ae, by = TAVGn3dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(BFI, by = TAVGn3dC, k = 3)+ s(Echange2, by = TAVGn3dC, k = 3)+ s(JulianDate, k = 5), data = data.day.fl))
summary(m1gf)

m1gfL <- (lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn3dC+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+SLOPE*TAVGn3dC+BFI*TAVGn3dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = data.day.fl))
summary(m1gfL)


summary(m1gs)#0.9325,0.955, 
summary(m1gsL)#0.9756,0.9537

summary(m1gf)#1.2483,0.951
summary(m1gfL)#1.146,0.9487

data.day.sp$predict<-predict(m1gs)
data.day.sp$resid<-resid(m1gs)
data.day.sp$resid2<-(data.day.sp$resid)^2
mean(data.day.sp$resid2)^.5 #0.9644755
data.day.sp$residLin<-resid(m1gsL)
data.day.sp$residLin2<-(data.day.sp$residLin)^2
mean(data.day.sp$residLin2)^.5 #0.9752422

data.day.fl$predict<-predict(m1gf)
data.day.fl$resid<-resid(m1gf)
data.day.fl$resid2<-(data.day.fl$resid)^2
(mean(data.day.fl$resid2))^.5 #1.105857
data.day.fl$residLin<-resid(m1gfL)
data.day.fl$residLin2<-(data.day.fl$residLin)^2
mean(data.day.fl$residLin2)^.5 #1.145639


###### # * Relationship explorations Wenathcee ##########################

library(visreg)
#spring visreg
visreg2d(m1gs, "TAVGn5dC", "Tchange5C", plot.type="persp",nn=12)  
visreg2d(m1gsTE, "TAVGn5dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gs, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gs, "JulianDate", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsTE, "dailymeanCMS", "TAVGn5dC", plot.type="image",nn=12)  
visreg2d(m1gs, "catch_area", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gs, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gsTE, "catch_area", "dailymeanCMS", plot.type="persp",nn=12) 
visreg2d(m1gs, "Echange2", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gs, "for_cover", "TAVGn5dC", plot.type="persp",nn=12) 
visreg2d(m1gs, "FCa", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gs, "Lake_perc", "JulianDate", plot.type="persp",nn=10)  
visreg2d(m1gs, "SLOPE", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gs, "BFI", "TAVGn5dC", plot.type="persp",nn=12)
visreg2d(m1gs, "ae", "JulianDate", plot.type="persp",nn=12)  

#spring visreg Linear
visreg2d(m1gsL, "TAVGn5dC", "Tchange5C", plot.type="persp",nn=12)  
visreg2d(m1gsL, "TAVGn5dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gsL, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gsL, "JulianDate", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsL, "dailymeanCMS", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsL, "catch_area", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsL, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gsL, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  #
visreg2d(m1gsL, "Echange2", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsL, "for_cover", "TAVGn5dC", plot.type="persp",nn=12) 
visreg2d(m1gsL, "FCa", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsL, "Lake_perc", "JulianDate", plot.type="persp",nn=10)  
visreg2d(m1gsL, "SLOPE", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsL, "BFI", "TAVGn5dC", plot.type="persp",nn=12)  
win.graph()
#fall visreg
visreg2d(m1gf, "TAVGn3dC", "Tchange3C", plot.type="persp",nn=12)  
visreg2d(m1gf, "TAVGn3dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gf, "SNWD", "TAVGn3dC", plot.type="persp",nn=12)  #
visreg2d(m1gf, "dailymeanCMS", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gf, "catch_area", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gf, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gf, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  #
visreg2d(m1gf, "ae", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gf, "SNWD", "ae", plot.type="persp",nn=12) #
visreg2d(m1gf, "Echange2", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gf, "Lake_perc", "JulianDate", plot.type="persp", nn=10)  
visreg2d(m1gf, "BFI", "TAVGn3dC", plot.type="persp",nn=12)  
visreg(m1gf, "Lake_perc")  
summary(m1gf)
#LINEAR WENATCHEE
visreg2d(m1gfL, "TAVGn3dC", "Tchange3C", plot.type="persp",nn=12)  
visreg2d(m1gfL, "TAVGn3dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gfL, "SNWD", "TAVGn3dC", plot.type="persp",nn=12)  #
visreg2d(m1gfL, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gfL, "JulianDate", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfL, "dailymeanCMS", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfL, "catch_area", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfL, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gfL, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  #
visreg2d(m1gfL, "ae", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfL, "SNWD", "ae", plot.type="persp",nn=12) # 
visreg2d(m1gfL, "Echange2", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfL, "Lake_perc", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfL, "SLOPE", "TAVGn3dC", plot.type="persp",nn=12) 
visreg2d(m1gfL, "BFI", "TAVGn3dC", plot.type="persp",nn=12)  
######### * LOOCV Wenatchee #########################################
###

library(mgcv)
####LOOCV spring Wen GAM  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.sp$SiteName <- factor(data.day.sp$SiteName)
Sites_springWen <- unique(data.day.sp$SiteName)

Sites_springWen<-Sites_springWen[Sites_springWen != "LWE"]#lower wenatchee not removed because it is an outlier and we wouldn't predict it with a model without it
for(i in Sites_springWen) {
  Wen_Champsp_Ni <- subset(data.day.sp, SiteName!=i)
  Wen_Champsp_i <- subset(data.day.sp, SiteName == i)
  mdl <- gam(AvgDailyTemp ~ s(TAVGn5dC, k = 4) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVGn5dC, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+s(BFI, by = TAVGn5dC, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3)+s(for_cover, by = TAVGn5dC, k = 3), data = Wen_Champsp_Ni) # leave i'th site out and refit model
  Wen_Champsp_i$y_pred <- predict(mdl, newdata = Wen_Champsp_i) #predict site data without site info
  Wen_Champsp_i$y_pred[Wen_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(Wen_Champsp_i$AvgDailyTemp  - Wen_Champsp_i$y_pred) #RMSPE
  resid_list<- rbind(resid_list, resid)
}
RMSPE_WenSp_GAM  <- (mean(resid_list^2))^.5

####LOOCV fall Wen GAM  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.fl$SiteName <- factor(data.day.fl$SiteName)
Sites_fallWen <- unique(data.day.fl$SiteName)
Sites_fallWen<-Sites_fallWen[Sites_fallWen != "LWE"]#lower wenatchee not removed because it is an outlier and we wouldn't predict it with a model without it
for(i in Sites_fallWen) {
  Wen_Champfl_Ni <- subset(data.day.fl, SiteName!=i)  #subset without site i
  Wen_Champfl_i <- subset(data.day.fl, SiteName == i) #subset only site i
  mdl <- gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = TAVGn3dC, k = 3)+s(ae, by = TAVGn3dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(BFI, by = TAVGn3dC, k = 3)+ s(Echange2, by = TAVGn3dC, k = 3)+ s(JulianDate, k = 5), data = Wen_Champfl_Ni)# leave i'th site out and refit model
  Wen_Champfl_i$y_pred <- predict(mdl, newdata = Wen_Champfl_i) #predict site data without site info
  Wen_Champfl_i$y_pred[Wen_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(Wen_Champfl_i$AvgDailyTemp  - Wen_Champfl_i$y_pred) #RMSPE
  resid_list<- rbind(resid_list, resid)
}
RMSPE_WenFl_GAM  <- (mean(resid_list^2))^.5

####LOOCV spring WenLinear  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.sp$SiteName <- factor(data.day.sp$SiteName)
Sites_springWen <- unique(data.day.sp$SiteName)
Sites_springWen<-Sites_springWen[Sites_springWen != "LWE"]#lower wenatchee not removed because it is an outlier and we wouldn't predict it with a model without it
for(i in Sites_springWen) {
  Wen_Champsp_Ni <- subset(data.day.sp, SiteName!=i)
  Wen_Champsp_i <- subset(data.day.sp, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn5dC+catch_area*dailymeanCMS+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC+for_cover*TAVGn5dC, data = Wen_Champsp_Ni) # leave i'th site out and refit model
  Wen_Champsp_i$y_pred <- predict(mdl, newdata = Wen_Champsp_i) #predict site data without site info
  Wen_Champsp_i$y_pred[Wen_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(Wen_Champsp_i$AvgDailyTemp  - Wen_Champsp_i$y_pred) #RMSPE
  resid_list<- rbind(resid_list, resid)
}
RMSPE_WenSp_linear  <- (mean(resid_list^2))^.5

####LOOCV fall Wenannon Linear  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.fl$SiteName <- factor(data.day.fl$SiteName)
Sites_fallWen <- unique(data.day.fl$SiteName)
Sites_fallWen<-Sites_fallWen[Sites_fallWen != "LWE"]#lower wenatchee not removed because it is an outlier and we wouldn't predict it with a model without it
for(i in Sites_fallWen) {
  Wen_Champfl_Ni <- subset(data.day.fl, SiteName!=i)
  Wen_Champfl_i <- subset(data.day.fl, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*TAVGn3dC+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+SLOPE*TAVGn3dC+BFI*TAVGn3dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = Wen_Champfl_Ni) # leave i'th site out and refit model
  Wen_Champfl_i$y_pred <- predict(mdl, newdata = Wen_Champfl_i) #predict site data without site info
  Wen_Champfl_i$y_pred[Wen_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(Wen_Champfl_i$AvgDailyTemp  - Wen_Champfl_i$y_pred) #RMSPE
  resid_list<- rbind(resid_list, resid)
  
}
RMSPE_WenFl_linear  <- (mean(resid_list^2))^.5


RMSPE_WenSp_GAM #1.152967
RMSPE_WenSp_linear #1.056864
RMSPE_WenFl_GAM #1.329183
RMSPE_WenFl_linear #1.357472




#### * Model fitting & metrics Chiwawa  ####

#spring model Chiwawa
m1gsCH <- (gam(AvgDailyTemp ~ s(TAVGn5dC, k = 5) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3), data = data.day.spCH))

m1gsLCH <- (lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC, data = data.day.spCH))

#fall model Chiwawa
m1gfCH <- (gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(JulianDate, by = TAVGn3dC, k = 5), data = data.day.flCH))

m1gfLCH <- (lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = data.day.flCH))


summary(m1gsCH)#0.975, 0.38726
summary(m1gsLCH)#0.6446,0.9734

summary(m1gfCH)#0.971,0.62207
summary(m1gfLCH)#0.9688,0.8117


data.day.spCH$resid<-resid(m1gsCH)
data.day.spCH$residLin<-resid(m1gsLCH)
data.day.spCH$resid2<-(data.day.spCH$resid)^2
data.day.spCH$residLin2<-(data.day.spCH$residLin)^2
mean(data.day.spCH$resid2)^.5 #0.6209291
mean(data.day.spCH$residLin2)^.5 #0.6439054

data.day.flCH$resid<-resid(m1gfCH)
data.day.flCH$residLin<-resid(m1gfLCH)
data.day.flCH$resid2<-(data.day.flCH$resid)^2
data.day.flCH$residLin2<-(data.day.flCH$residLin)^2
(mean(data.day.flCH$resid2))^.5 #0.7842462
(mean(data.day.flCH$residLin2))^.5 #0.8100583

###### # * Relationship explorations Chiwawa ##########################


#spring visreg CHIWAWA
visreg2d(m1gsCH, "TAVGn5dC", "Tchange5C", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "TAVGn5dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gsCH, "JulianDate", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "dailymeanCMS", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "catch_area", "TAVGn5dC", plot.type="persp",nn=12)  ##remove
visreg2d(m1gsCH, "catch_area", "ae", plot.type="persp",nn=12)  #remove
visreg2d(m1gsCH, "ae", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "Echange2", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "Lake_perc", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "SLOPE", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "BFI", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsCH, "FCa", "TAVGn5dC", plot.type="persp",nn=12)  

###spring visreg Linear
visreg2d(m1gsLCH, "TAVGn5dC", "Tchange5C", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "TAVGn5dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gsLCH, "JulianDate", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "dailymeanCMS", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "catch_area", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)##  
visreg2d(m1gsLCH, "Echange2", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "for_cover", "TAVGn5dC", plot.type="persp",nn=12)##  
visreg2d(m1gsLCH, "Lake_perc", "TAVGn5dC", plot.type="persp",nn=12)  
visreg2d(m1gsLCH, "SLOPE", "TAVGn5dC", plot.type="persp",nn=12)  

#FALL VISREG CHIWAWA GAM
visreg2d(m1gfCH, "TAVGn3dC", "Tchange3C", plot.type="persp",nn=12)  
visreg2d(m1gfCH, "TAVGn3dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gfCH, "SNWD", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfCH, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gfCH, "dailymeanCMS", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfCH, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gfCH, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  #
visreg2d(m1gfCH, "ae", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfCH, "SNWD", "ae", plot.type="persp",nn=12) 
visreg2d(m1gfCH, "Lake_perc", "TAVGn3dC", plot.type="persp",nn=12)  
summary(m1gfCH)
#LINEAR CHIWAWA
visreg2d(m1gfLCH, "TAVGn3dC", "Tchange3C", plot.type="persp",nn=12)  
visreg2d(m1gfLCH, "TAVGn3dC", "JulianDate", plot.type="persp",nn=12)  
visreg2d(m1gfLCH, "SNWD", "TAVGn3dC", plot.type="persp",nn=12) 
visreg2d(m1gfLCH, "A1Snow", "JulianDate", plot.type="persp",nn=12) 
visreg2d(m1gfLCH, "JulianDate", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfLCH, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(m1gfLCH, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)#
visreg2d(m1gfLCH, "ae", "TAVGn3dC", plot.type="persp",nn=12)  
visreg2d(m1gfLCH, "SNWD", "ae", plot.type="persp",nn=12)  
visreg2d(m1gfLCH, "Echange2", "TAVGn3dC", plot.type="persp",nn=12) 
visreg2d(m1gfLCH, "Lake_perc", "TAVGn3dC", plot.type="persp",nn=12)  
summary(m1gfLCH)


######### * LOOCV Chiwawa #########################################
###
library(mgcv)

####LOOCV spring Chi GAM  
resid_list <- matrix(ncol = 2, nrow = 0)
data.day.spCH$SiteName <- factor(data.day.spCH$SiteName)
Sites_springChi <- unique(data.day.spCH$SiteName)
for(i in Sites_springChi) {
  Chi_Champsp_Ni <- subset(data.day.spCH, SiteName!=i)
  Chi_Champsp_i <- subset(data.day.spCH, SiteName == i)
  mdl <- gam(AvgDailyTemp ~ s(TAVGn5dC, k = 5) +s(Tchange5C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = JulianDate, k = 3)+s(SLOPE, by = TAVGn5dC, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn5dC, k = 3) + s(JulianDate, by = TAVGn5dC, k = 5)+ s(Echange2, by = TAVGn5dC, k = 3), data = Chi_Champsp_Ni) # leave i'th site out and refit model
  Chi_Champsp_i$y_pred <- predict(mdl, newdata = Chi_Champsp_i) #predict site data without site info
  Chi_Champsp_i$y_pred[Chi_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(Chi_Champsp_i$AvgDailyTemp  - Chi_Champsp_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
  
}
colnames(resid_list)<-c("resid","site")
RMSPE_ChiSp_GAM  <- (mean(resid_list$resid^2))^.5
tapply(abs(resid_list$resid),resid_list$site,mean)

####LOOCV fall Chi GAM  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.flCH$SiteName <- factor(data.day.flCH$SiteName)
Sites_fallChi <- unique(data.day.flCH$SiteName)
for(i in Sites_fallChi) {
  Chi_Champfl_Ni <- subset(data.day.flCH, SiteName!=i)  #subset without site i
  Chi_Champfl_i <- subset(data.day.flCH, SiteName == i) #subset only site i
  mdl <- gam(AvgDailyTemp ~ s(TAVGn3dC, k = 5) +s(Tchange3C, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(SNWD,by = TAVGn3dC, k = 3)+s(catch_area, by = ae, k = 3)+s(ae, by = SNWD, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(Lake_perc, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3dC, k = 3)+ s(JulianDate, by = TAVGn3dC, k = 5), data = Chi_Champfl_Ni)# leave i'th site out and refit model
  Chi_Champfl_i$y_pred <- predict(mdl, newdata = Chi_Champfl_i) #predict site data without site info
  Chi_Champfl_i$y_pred[Chi_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(Chi_Champfl_i$AvgDailyTemp  - Chi_Champfl_i$y_pred) #resid
  resid_list<- rbind(resid_list, resid)
}
RMSPE_ChiFl_GAM  <- (mean(resid_list^2))^.5


####LOOCV spring ChiLinear  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.spCH$SiteName <- factor(data.day.spCH$SiteName)
Sites_springChi <- unique(data.day.spCH$SiteName)
for(i in Sites_springChi) {
  Chi_Champsp_Ni <- subset(data.day.spCH, SiteName!=i)
  Chi_Champsp_i <- subset(data.day.spCH, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*JulianDate+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC, data = Chi_Champsp_Ni) # leave i'th site out and refit model
  Chi_Champsp_i$y_pred <- predict(mdl, newdata = Chi_Champsp_i) #predict site data without site info
  Chi_Champsp_i$y_pred[Chi_Champsp_i$y_pred < 0] <- 0
  resid <- as.data.frame(Chi_Champsp_i$AvgDailyTemp  - Chi_Champsp_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_ChiSp_linear  <- (mean(resid_list$resid^2))^.5
tapply(abs(resid_list$resid),resid_list$site,mean)
head(resid_list)

Chi_Champsp_195 <- subset(data.day.spCH, SiteName == "WENMASTER-000195")
Chi_Champsp_N195 <- subset(data.day.spCH, SiteName != "WENMASTER-000195")
mdl <- lm(AvgDailyTemp ~ TAVGn5dC+Tchange5C+A1Snow*JulianDate+catch_area*ae+ae*JulianDate+SLOPE*TAVGn5dC+Lake_perc*TAVGn5dC+dailymeanCMS*TAVGn5dC+JulianDate*TAVGn5dC+Echange2*TAVGn5dC, data = Chi_Champsp_N195) # leave i'th site out and refit model
Chi_Champsp_195$y_pred <- predict(mdl, newdata = Chi_Champsp_195) #predict site data without site info

head(Chi_Champsp_195)
plot(AvgDailyTemp~DateNum,Chi_Champsp_195)
points(y_pred~DateNum,Chi_Champsp_195, col = "red")
WENMASTER-000195
####LOOCV fall Chiannon Linear  
resid_list <- (matrix(ncol = 2, nrow = 0))
data.day.flCH$SiteName <- factor(data.day.flCH$SiteName)
Sites_fallChi <- unique(data.day.flCH$SiteName)
for(i in Sites_fallChi) {
  Chi_Champfl_Ni <- subset(data.day.flCH, SiteName!=i)
  Chi_Champfl_i <- subset(data.day.flCH, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVGn3dC+Tchange3C+SNWD*TAVGn3dC+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+ae*JulianDate+ae*SNWD+Lake_perc*JulianDate+dailymeanCMS*TAVGn3dC+JulianDate*TAVGn3dC+Echange2*TAVGn3dC, data = Chi_Champfl_Ni) # leave i'th site out and refit model
  Chi_Champfl_i$y_pred <- predict(mdl, newdata = Chi_Champfl_i) #predict site data without site info
  Chi_Champfl_i$y_pred[Chi_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(Chi_Champfl_i$AvgDailyTemp  - Chi_Champfl_i$y_pred) #resid
  resid_list<- rbind(resid_list, resid)
}

RMSPE_ChiFl_linear  <- (mean(resid_list^2))^.5
head(resid_list)

RMSPE_ChiSp_GAM #0.6696353
RMSPE_ChiSp_linear #1.211801
RMSPE_ChiFl_GAM #0.837921
RMSPE_ChiFl_linear #0.8710685








# * Validation dataset predictions (Norwest) Wenatchee/Chiwawa #### 


#predicting all NorWest
data.NW.day.sp$predictGAM<- predict(m1gs, newdata = data.NW.day.sp)
data.NW.day.fl$predictGAM<- predict(m1gf, newdata = data.NW.day.fl)
data.NW.day.sp$predictGAM[data.NW.day.sp$predictGAM < 0] <- 0
data.NW.day.fl$predictGAM[data.NW.day.fl$predictGAM < 0] <- 0

data.NW.day.sp$predictLIN<- predict(m1gsL, newdata = data.NW.day.sp)
data.NW.day.fl$predictLIN<- predict(m1gfL, newdata = data.NW.day.fl)
data.NW.day.sp$predictLIN[data.NW.day.sp$predictLIN < 0] <- 0
data.NW.day.fl$predictLIN[data.NW.day.fl$predictLIN < 0] <- 0

#predicting just Chiwawa
data.NW.day.spCH$predictGAM<- predict(m1gsCH, newdata = data.NW.day.spCH)
data.NW.day.flCH$predictGAM<- predict(m1gfCH, newdata = data.NW.day.flCH)
data.NW.day.spCH$predictGAM[data.NW.day.spCH$predictGAM < 0] <- 0
data.NW.day.flCH$predictGAM[data.NW.day.flCH$predictGAM < 0] <- 0

data.NW.day.spCH$predictLIN<- predict(m1gsLCH, newdata = data.NW.day.spCH)
data.NW.day.flCH$predictLIN<- predict(m1gfLCH, newdata = data.NW.day.flCH)
data.NW.day.spCH$predictLIN[data.NW.day.spCH$predictLIN < 0] <- 0
data.NW.day.flCH$predictLIN[data.NW.day.flCH$predictLIN < 0] <- 0


#predict vs measured model summaries
##NORWEST PREDICTED BY CHAMP
#GAM
summary(lm(data.NW.day.sp$DailyMean~data.NW.day.sp$predictGAM))#1.162,0.9437
summary(lm(data.NW.day.fl$DailyMean~data.NW.day.fl$predictGAM))#1.281,0.9315
#Linear
summary(lm(data.NW.day.sp$DailyMean~data.NW.day.sp$predictLIN))#1.278,0.9319
summary(lm(data.NW.day.fl$DailyMean~data.NW.day.fl$predictLIN))#1.452,0.912

#Chiwawa GAM 
summary(lm(data.NW.day.spCH$DailyMean~data.NW.day.spCH$predictGAM))#0.7486,0.9721  
summary(lm(data.NW.day.flCH$DailyMean~data.NW.day.flCH$predictGAM))#0.9132,0.9561

summary(lm(data.NW.day.spCH$DailyMean~data.NW.day.spCH$predictLIN))#0.7973,0.9684  
summary(lm(data.NW.day.flCH$DailyMean~data.NW.day.flCH$predictLIN))#0.8622,0.9609

colnames(data.NW.day.fl)
###calculating residuals 
data.NW.day.sp$residGAM<- data.NW.day.sp$DailyMean - data.NW.day.sp$predictGAM
data.NW.day.fl$residGAM<- data.NW.day.fl$DailyMean - data.NW.day.fl$predictGAM
data.NW.day.sp$Resid2 <- ((data.NW.day.sp$residGAM)^2)
data.NW.day.fl$Resid2 <- ((data.NW.day.fl$residGAM)^2)
data.NW.day.sp$residLIN<- data.NW.day.sp$DailyMean - data.NW.day.sp$predictLIN
data.NW.day.fl$residLIN<- data.NW.day.fl$DailyMean - data.NW.day.fl$predictLIN
data.NW.day.sp$ResidLIN2 <- ((data.NW.day.sp$residLIN)^2)
data.NW.day.fl$ResidLIN2 <- ((data.NW.day.fl$residLIN)^2)

data.NW.day.spCH$residGAM<- data.NW.day.spCH$DailyMean - data.NW.day.spCH$predictGAM
data.NW.day.flCH$residGAM<- data.NW.day.flCH$DailyMean - data.NW.day.flCH$predictGAM
data.NW.day.spCH$Resid2 <- ((data.NW.day.spCH$residGAM)^2)
data.NW.day.flCH$Resid2 <- ((data.NW.day.flCH$residGAM)^2)
data.NW.day.spCH$residLIN<- data.NW.day.spCH$DailyMean - data.NW.day.spCH$predictLIN
data.NW.day.flCH$residLIN<- data.NW.day.flCH$DailyMean - data.NW.day.flCH$predictLIN
data.NW.day.spCH$ResidLIN2 <- ((data.NW.day.spCH$residLIN)^2)
data.NW.day.flCH$ResidLIN2 <- ((data.NW.day.flCH$residLIN)^2)


#RMSE for all years by season 
mean(data.NW.day.sp$Resid2)^.5 #1.186247
mean(data.NW.day.fl$Resid2)^.5 #1.293283

mean(data.NW.day.sp$ResidLIN2)^.5 #1.305757
mean(data.NW.day.fl$ResidLIN2)^.5 #1.453089
#chiwawa only
mean(data.NW.day.spCH$Resid2)^.5 #0.7589377
mean(data.NW.day.flCH$Resid2)^.5 #0.9711719

mean(data.NW.day.spCH$ResidLIN2)^.5 #0.8059923
mean(data.NW.day.flCH$ResidLIN2)^.5 #0.8953167


#### * RMSE by year ####

data.NW.day.sp.11 <- subset(data.NW.day.sp, year == 2011)
data.NW.day.sp.10 <- subset(data.NW.day.sp, year == 2010)
data.NW.day.sp.9 <- subset(data.NW.day.sp, year == 2009)
data.NW.day.sp.8 <- subset(data.NW.day.sp, year == 2008)
data.NW.day.sp.7 <- subset(data.NW.day.sp, year == 2007)
data.NW.day.sp.6 <- subset(data.NW.day.sp, year == 2006)
data.NW.day.sp.5 <- subset(data.NW.day.sp, year == 2005)
data.NW.day.sp.4 <- subset(data.NW.day.sp, year == 2004)
data.NW.day.sp.3 <- subset(data.NW.day.sp, year == 2003)
data.NW.day.fl.11 <- subset(data.NW.day.fl, year == 2011)
data.NW.day.fl.10 <- subset(data.NW.day.fl, year == 2010)
data.NW.day.fl.9 <- subset(data.NW.day.fl, year == 2009)
data.NW.day.fl.8 <- subset(data.NW.day.fl, year == 2008)
data.NW.day.fl.7 <- subset(data.NW.day.fl, year == 2007)
data.NW.day.fl.6 <- subset(data.NW.day.fl, year == 2006)
data.NW.day.fl.5 <- subset(data.NW.day.fl, year == 2005)
data.NW.day.fl.4 <- subset(data.NW.day.fl, year == 2004)
data.NW.day.fl.3 <- subset(data.NW.day.fl, year == 2003)

#RMSE seasons by year
WenatcheeYearRMSE <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(WenatcheeYearRMSE)<- c("year","springGAM","fallGAM","CombinedGAM","MonthGAM")
WenatcheeYearRMSE$year<-c(2003:2011,"All")
WenatcheeYearRMSE[10,2]<-mean(data.NW.day.sp$Resid2)^.5 #1.183344
WenatcheeYearRMSE[9,2]<- mean(data.NW.day.sp.11$Resid2)^.5 #1.121367
WenatcheeYearRMSE[8,2]<-mean(data.NW.day.sp.10$Resid2)^.5 #1.088145
WenatcheeYearRMSE[7,2]<-mean(data.NW.day.sp.9$Resid2)^.5 #1.014377
WenatcheeYearRMSE[6,2]<-mean(data.NW.day.sp.8$Resid2)^.5 #1.170023
WenatcheeYearRMSE[5,2]<-mean(data.NW.day.sp.7$Resid2)^.5 #0.9515196
WenatcheeYearRMSE[4,2]<-mean(data.NW.day.sp.6$Resid2)^.5 #1.254017
WenatcheeYearRMSE[3,2]<-mean(data.NW.day.sp.5$Resid2)^.5 #1.301651
WenatcheeYearRMSE[2,2]<-mean(data.NW.day.sp.4$Resid2)^.5 #1.513557
WenatcheeYearRMSE[1,2]<-mean(data.NW.day.sp.3$Resid2)^.5 #1.162988
WenatcheeYearRMSE[10,3]<-mean(data.NW.day.fl$Resid2)^.5 #1.401051
WenatcheeYearRMSE[9,3]<-mean(data.NW.day.fl.11$Resid2)^.5 #1.636073
WenatcheeYearRMSE[8,3]<-mean(data.NW.day.fl.10$Resid2)^.5 #1.334608
WenatcheeYearRMSE[7,3]<-mean(data.NW.day.fl.9$Resid2)^.5 #1.337806
WenatcheeYearRMSE[6,3]<-mean(data.NW.day.fl.8$Resid2)^.5 #1.340005
WenatcheeYearRMSE[5,3]<-mean(data.NW.day.fl.7$Resid2)^.5 #1.362343
WenatcheeYearRMSE[4,3]<-mean(data.NW.day.fl.6$Resid2)^.5 #1.375101
WenatcheeYearRMSE[3,3]<-mean(data.NW.day.fl.5$Resid2)^.5 #1.503524
WenatcheeYearRMSE[2,3]<-mean(data.NW.day.fl.4$Resid2)^.5 #1.331061
WenatcheeYearRMSE[1,3]<-mean(data.NW.day.fl.3$Resid2)^.5 #1.301322

WenatcheeYearRMSELin <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(WenatcheeYearRMSELin)<- c("year","springLin","fallLin","CombinedLin","LinMonth")
WenatcheeYearRMSELin$year<-c(2003:2011,"All")
WenatcheeYearRMSELin[10,2]<-mean(data.NW.day.sp$ResidLIN2)^.5 #1.323663
WenatcheeYearRMSELin[9,2]<- mean(data.NW.day.sp.11$ResidLIN2)^.5 #1.207868
WenatcheeYearRMSELin[8,2]<- mean(data.NW.day.sp.10$ResidLIN2)^.5 #1.200784
WenatcheeYearRMSELin[7,2]<- mean(data.NW.day.sp.9$ResidLIN2)^.5 #1.122813
WenatcheeYearRMSELin[6,2]<- mean(data.NW.day.sp.8$ResidLIN2)^.5 #1.262863
WenatcheeYearRMSELin[5,2]<- mean(data.NW.day.sp.7$ResidLIN2)^.5 #1.009128
WenatcheeYearRMSELin[4,2]<- mean(data.NW.day.sp.6$ResidLIN2)^.5 #1.350245
WenatcheeYearRMSELin[3,2]<- mean(data.NW.day.sp.5$ResidLIN2)^.5 #1.47475
WenatcheeYearRMSELin[2,2]<- mean(data.NW.day.sp.4$ResidLIN2)^.5 #1.823033
WenatcheeYearRMSELin[1,2]<- mean(data.NW.day.sp.3$ResidLIN2)^.5 #1.338922
WenatcheeYearRMSELin[10,3]<-mean(data.NW.day.fl$ResidLIN2)^.5 #1.453179
WenatcheeYearRMSELin[9,3]<- mean(data.NW.day.fl.11$ResidLIN2)^.5 #1.546711
WenatcheeYearRMSELin[8,3]<-mean(data.NW.day.fl.10$ResidLIN2)^.5 #1.284862
WenatcheeYearRMSELin[7,3]<-mean(data.NW.day.fl.9$ResidLIN2)^.5 #1.375536
WenatcheeYearRMSELin[6,3]<-mean(data.NW.day.fl.8$ResidLIN2)^.5 #1.359144
WenatcheeYearRMSELin[5,3]<-mean(data.NW.day.fl.7$ResidLIN2)^.5 #1.379206
WenatcheeYearRMSELin[4,3]<-mean(data.NW.day.fl.6$ResidLIN2)^.5 #1.429625
WenatcheeYearRMSELin[3,3]<-mean(data.NW.day.fl.5$ResidLIN2)^.5 #1.611911
WenatcheeYearRMSELin[2,3]<-mean(data.NW.day.fl.4$ResidLIN2)^.5 #1.564846
WenatcheeYearRMSELin[1,3]<-mean(data.NW.day.fl.3$ResidLIN2)^.5 #1.478439


data.NW.day.spCH.11 <- subset(data.NW.day.spCH, year == 2011)
data.NW.day.spCH.10 <- subset(data.NW.day.spCH, year == 2010)
data.NW.day.spCH.9 <- subset(data.NW.day.spCH, year == 2009)
data.NW.day.spCH.8 <- subset(data.NW.day.spCH, year == 2008)
data.NW.day.spCH.7 <- subset(data.NW.day.spCH, year == 2007)
data.NW.day.spCH.6 <- subset(data.NW.day.spCH, year == 2006)
data.NW.day.spCH.5 <- subset(data.NW.day.spCH, year == 2005)
data.NW.day.spCH.4 <- subset(data.NW.day.spCH, year == 2004)
data.NW.day.spCH.3 <- subset(data.NW.day.spCH, year == 2003)
data.NW.day.flCH.11 <- subset(data.NW.day.flCH, year == 2011)
data.NW.day.flCH.10 <- subset(data.NW.day.flCH, year == 2010)
data.NW.day.flCH.9 <- subset(data.NW.day.flCH, year == 2009)
data.NW.day.flCH.8 <- subset(data.NW.day.flCH, year == 2008)
data.NW.day.flCH.7 <- subset(data.NW.day.flCH, year == 2007)
data.NW.day.flCH.6 <- subset(data.NW.day.flCH, year == 2006)
data.NW.day.flCH.5 <- subset(data.NW.day.flCH, year == 2005)
data.NW.day.flCH.4 <- subset(data.NW.day.flCH, year == 2004)
data.NW.day.flCH.3 <- subset(data.NW.day.flCH, year == 2003)

#RMSE seasons by year
ChiwawaYearRMSE <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(ChiwawaYearRMSE)<- c("year","springGAM","fallGAM","CombinedGAM","MonthGAM")
ChiwawaYearRMSE$year<-c(2003:2011,"All")
ChiwawaYearRMSE[10,2]<-mean(data.NW.day.spCH$Resid2)^.5 #0.7968539
ChiwawaYearRMSE[9,2]<-mean(data.NW.day.spCH.11$Resid2)^.5 #0.8842574
ChiwawaYearRMSE[8,2]<-mean(data.NW.day.spCH.10$Resid2)^.5 #0.788395
ChiwawaYearRMSE[7,2]<-mean(data.NW.day.spCH.9$Resid2)^.5 #0.7452477
ChiwawaYearRMSE[6,2]<-mean(data.NW.day.spCH.8$Resid2)^.5 #0.5636819
ChiwawaYearRMSE[5,2]<-mean(data.NW.day.spCH.7$Resid2)^.5 #0.5912394
ChiwawaYearRMSE[4,2]<-mean(data.NW.day.spCH.6$Resid2)^.5 #0.6820043
ChiwawaYearRMSE[3,2]<-mean(data.NW.day.spCH.5$Resid2)^.5 #1.011454
ChiwawaYearRMSE[2,2]<-mean(data.NW.day.spCH.4$Resid2)^.5 #1.000373
ChiwawaYearRMSE[1,2]<-mean(data.NW.day.spCH.3$Resid2)^.5 #0.7228097
ChiwawaYearRMSE[10,3]<-mean(data.NW.day.flCH$Resid2)^.5 #0.971248
ChiwawaYearRMSE[9,3]<-mean(data.NW.day.flCH.11$Resid2)^.5 #1.468599
ChiwawaYearRMSE[8,3]<-mean(data.NW.day.flCH.10$Resid2)^.5 #0.8728966
ChiwawaYearRMSE[7,3]<-mean(data.NW.day.flCH.9$Resid2)^.5 #0.8728215
ChiwawaYearRMSE[6,3]<-mean(data.NW.day.flCH.8$Resid2)^.5 #0.9541952
ChiwawaYearRMSE[5,3]<-mean(data.NW.day.flCH.7$Resid2)^.5 #0.7696129
ChiwawaYearRMSE[4,3]<-mean(data.NW.day.flCH.6$Resid2)^.5 #0.8533864
ChiwawaYearRMSE[3,3]<-mean(data.NW.day.flCH.5$Resid2)^.5 #1.036045
ChiwawaYearRMSE[2,3]<-mean(data.NW.day.flCH.4$Resid2)^.5 #1.047521
ChiwawaYearRMSE[1,3]<-mean(data.NW.day.flCH.3$Resid2)^.5 #0.679463


ChiwawaYearRMSELin <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(ChiwawaYearRMSELin)<- c("year","springLin","fallLin","CombinedLin","LinMonth")
ChiwawaYearRMSELin$year<-c(2003:2011,"All")
ChiwawaYearRMSELin[10,2]<-mean(data.NW.day.spCH$ResidLIN2)^.5 #0.8364344
ChiwawaYearRMSELin[9,2]<-mean(data.NW.day.spCH.11$ResidLIN2)^.5 #0.8842574
ChiwawaYearRMSELin[8,2]<-mean(data.NW.day.spCH.10$ResidLIN2)^.5 #0.788395
ChiwawaYearRMSELin[7,2]<-mean(data.NW.day.spCH.9$ResidLIN2)^.5 #0.7452477
ChiwawaYearRMSELin[6,2]<-mean(data.NW.day.spCH.8$ResidLIN2)^.5 #0.5636819
ChiwawaYearRMSELin[5,2]<-mean(data.NW.day.spCH.7$ResidLIN2)^.5 #0.5912394
ChiwawaYearRMSELin[4,2]<-mean(data.NW.day.spCH.6$ResidLIN2)^.5 #0.6820043
ChiwawaYearRMSELin[3,2]<-mean(data.NW.day.spCH.5$ResidLIN2)^.5 #1.011454
ChiwawaYearRMSELin[2,2]<-mean(data.NW.day.spCH.4$ResidLIN2)^.5 #1.000373
ChiwawaYearRMSELin[1,2]<-mean(data.NW.day.spCH.3$ResidLIN2)^.5 #0.7228097
ChiwawaYearRMSELin[10,3]<-mean(data.NW.day.flCH$ResidLIN2)^.5 #0.9095072
ChiwawaYearRMSELin[9,3]<-mean(data.NW.day.flCH.11$ResidLIN2)^.5 #1.468599
ChiwawaYearRMSELin[8,3]<-mean(data.NW.day.flCH.10$ResidLIN2)^.5 #0.8728966
ChiwawaYearRMSELin[7,3]<-mean(data.NW.day.flCH.9$ResidLIN2)^.5 #0.8728215
ChiwawaYearRMSELin[6,3]<-mean(data.NW.day.flCH.8$ResidLIN2)^.5 #0.9541952
ChiwawaYearRMSELin[5,3]<-mean(data.NW.day.flCH.7$ResidLIN2)^.5 #0.7696129
ChiwawaYearRMSELin[4,3]<-mean(data.NW.day.flCH.6$ResidLIN2)^.5 #0.8533864
ChiwawaYearRMSELin[3,3]<-mean(data.NW.day.flCH.5$ResidLIN2)^.5 #1.036045
ChiwawaYearRMSELin[2,3]<-mean(data.NW.day.flCH.4$ResidLIN2)^.5 #1.047521
ChiwawaYearRMSELin[1,3]<-mean(data.NW.day.flCH.3$ResidLIN2)^.5 #0.679463




###CHAMP and MRR predicting NorWest COMBINED ALL YEAR DATA
data.NW.day.COM <- rbind(data.NW.day.sp,data.NW.day.fl)
#RMSE for all data
mean(data.NW.day.COM$Resid2)^.5 #1.241314
mean(data.NW.day.COM$ResidLIN2)^.5 #1.381931


med<-as.data.frame.table(tapply(data.NW.day.COM$Resid2,data.NW.day.COM$PERMA_FID.x,mean))
colnames(med)<-c("Site","Resid2")
med$RMSE<-med$Resid2^.5
median(med$RMSE)
mean(med$RMSE)
med <- med[order(med$RMSE),]

win.graph(width =5, height = 4.5)
par(cex=.9, ps=10)
plot(DailyMean~predictGAM, data = data.NW.day.COM, pch = ".", ylab = "Tw (°C)", xlab = "predicted Tw  (°C)")
abline(0,1, col = "red")

##Chiwawa RMSE
data.NW.day.CHCOM <- rbind(data.NW.day.spCH,data.NW.day.flCH)
mean(data.NW.day.CHCOM$Resid2)^.5 #0.8831881 
mean(data.NW.day.CHCOM$ResidLIN2)^.5 #0.8527778


plot(DailyMean~predictGAM, data = data.NW.day.CHCOM)
abline(0,1, col = "red")
plot(DailyMean~predictLIN, data = data.NW.day.CHCOM)
abline(0,1, col = "red")



#RMSE by year 
data.NW.day.COM.11 <- subset(data.NW.day.COM, year == 2011)
data.NW.day.COM.10 <- subset(data.NW.day.COM, year == 2010)
data.NW.day.COM.9 <- subset(data.NW.day.COM, year == 2009)
data.NW.day.COM.8 <- subset(data.NW.day.COM, year == 2008)
data.NW.day.COM.7 <- subset(data.NW.day.COM, year == 2007)
data.NW.day.COM.6 <- subset(data.NW.day.COM, year == 2006)
data.NW.day.COM.5 <- subset(data.NW.day.COM, year == 2005)
data.NW.day.COM.4 <- subset(data.NW.day.COM, year == 2004)
data.NW.day.COM.3 <- subset(data.NW.day.COM, year == 2003)

WenatcheeYearRMSE[10,4]<-mean(data.NW.day.COM$Resid2)^.5
WenatcheeYearRMSE[9,4]<-mean(data.NW.day.COM.11$Resid2)^.5 #1.399476
WenatcheeYearRMSE[8,4]<-mean(data.NW.day.COM.10$Resid2)^.5 #1.214873
WenatcheeYearRMSE[7,4]<-mean(data.NW.day.COM.9$Resid2)^.5 #1.180414
WenatcheeYearRMSE[6,4]<-mean(data.NW.day.COM.8$Resid2)^.5 #1.258808
WenatcheeYearRMSE[5,4]<-mean(data.NW.day.COM.7$Resid2)^.5 #1.169534
WenatcheeYearRMSE[4,4]<-mean(data.NW.day.COM.6$Resid2)^.5 #1.3192
WenatcheeYearRMSE[3,4]<-mean(data.NW.day.COM.5$Resid2)^.5 #1.394725
WenatcheeYearRMSE[2,4]<-mean(data.NW.day.COM.4$Resid2)^.5 #1.413072
WenatcheeYearRMSE[1,4]<-mean(data.NW.day.COM.3$Resid2)^.5 #1.246328

WenatcheeYearRMSELin[10,4]<-mean(data.NW.day.COM$ResidLIN2)^.5 #1.389129
WenatcheeYearRMSELin[9,4]<-mean(data.NW.day.COM.11$ResidLIN2)^.5 #1.38564
WenatcheeYearRMSELin[8,4]<-mean(data.NW.day.COM.10$ResidLIN2)^.5 #1.24259
WenatcheeYearRMSELin[7,4]<-mean(data.NW.day.COM.9$ResidLIN2)^.5 #1.250262
WenatcheeYearRMSELin[6,4]<-mean(data.NW.day.COM.8$ResidLIN2)^.5 #1.312409
WenatcheeYearRMSELin[5,4]<-mean(data.NW.day.COM.7$ResidLIN2)^.5 #1.203455
WenatcheeYearRMSELin[4,4]<-mean(data.NW.day.COM.6$ResidLIN2)^.5 #1.392633
WenatcheeYearRMSELin[3,4]<-mean(data.NW.day.COM.5$ResidLIN2)^.5 #1.537047
WenatcheeYearRMSELin[2,4]<-mean(data.NW.day.COM.4$ResidLIN2)^.5 #1.681647
WenatcheeYearRMSELin[1,4]<-mean(data.NW.day.COM.3$ResidLIN2)^.5 #1.246328
#RMSE by year 
data.NW.day.CHCOM.11 <- subset(data.NW.day.CHCOM, year == 2011)
data.NW.day.CHCOM.10 <- subset(data.NW.day.CHCOM, year == 2010)
data.NW.day.CHCOM.9 <- subset(data.NW.day.CHCOM, year == 2009)
data.NW.day.CHCOM.8 <- subset(data.NW.day.CHCOM, year == 2008)
data.NW.day.CHCOM.7 <- subset(data.NW.day.CHCOM, year == 2007)
data.NW.day.CHCOM.6 <- subset(data.NW.day.CHCOM, year == 2006)
data.NW.day.CHCOM.5 <- subset(data.NW.day.CHCOM, year == 2005)
data.NW.day.CHCOM.4 <- subset(data.NW.day.CHCOM, year == 2004)
data.NW.day.CHCOM.3 <- subset(data.NW.day.CHCOM, year == 2003)
ChiwawaYearRMSE[10,4]<-mean(data.NW.day.CHCOM$Resid2)^.5 #0.8920156 #NOT BAD!!!!
ChiwawaYearRMSE[9,4]<-mean(data.NW.day.CHCOM.11$Resid2)^.5 #1.180414
ChiwawaYearRMSE[8,4]<-mean(data.NW.day.CHCOM.10$Resid2)^.5 #1.180414
ChiwawaYearRMSE[7,4]<-mean(data.NW.day.CHCOM.9$Resid2)^.5 #1.180414
ChiwawaYearRMSE[6,4]<-mean(data.NW.day.CHCOM.8$Resid2)^.5 #1.180414
ChiwawaYearRMSE[5,4]<-mean(data.NW.day.CHCOM.7$Resid2)^.5 #1.169534
ChiwawaYearRMSE[4,4]<-mean(data.NW.day.CHCOM.6$Resid2)^.5 #1.3192
ChiwawaYearRMSE[3,4]<-mean(data.NW.day.CHCOM.5$Resid2)^.5 #1.394725
ChiwawaYearRMSE[2,4]<-mean(data.NW.day.CHCOM.4$Resid2)^.5 #1.413072
ChiwawaYearRMSE[1,4]<-mean(data.NW.day.CHCOM.3$Resid2)^.5 #1.246328

ChiwawaYearRMSELin[10,4]<-mean(data.NW.day.CHCOM$ResidLIN2)^.5 #0.8920156 #NOT BAD!!!!
ChiwawaYearRMSELin[9,4]<-mean(data.NW.day.CHCOM.11$ResidLIN2)^.5 #1.180414
ChiwawaYearRMSELin[8,4]<-mean(data.NW.day.CHCOM.10$ResidLIN2)^.5 #1.180414
ChiwawaYearRMSELin[7,4]<-mean(data.NW.day.CHCOM.9$ResidLIN2)^.5 #1.180414
ChiwawaYearRMSELin[6,4]<-mean(data.NW.day.CHCOM.8$ResidLIN2)^.5 #1.180414
ChiwawaYearRMSELin[5,4]<-mean(data.NW.day.CHCOM.7$ResidLIN2)^.5 #1.169534
ChiwawaYearRMSELin[4,4]<-mean(data.NW.day.CHCOM.6$ResidLIN2)^.5 #1.3192
ChiwawaYearRMSELin[3,4]<-mean(data.NW.day.CHCOM.5$ResidLIN2)^.5 #1.394725
ChiwawaYearRMSELin[2,4]<-mean(data.NW.day.CHCOM.4$ResidLIN2)^.5 #1.413072
ChiwawaYearRMSELin[1,4]<-mean(data.NW.day.CHCOM.3$ResidLIN2)^.5 #1.246328




#### * FIT PLOTS #

win.graph(width = 12, height = 12)
par(mfrow= c(3,3), mar=c(2,2,1,1), cex=.8, ps=10)
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2003), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2003")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2004), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2004")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2005), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2005")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2006), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2006")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2007), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2007")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2008), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2008")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2009), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2009")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2010), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2010")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.NW.day.COM, year==2011), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2011")
abline(0,1, col = "red", pch=".")

win.graph(width = 12, height = 12)
par(mfrow= c(3,3), mar=c(2,2,1,1), cex=.8, ps=10)
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2003), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2003")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2004), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2004")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2005), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2005")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2006), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2006")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2007), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2007")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2008), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2008")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2009), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2009")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2010), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2010")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictLIN,data= subset(data.NW.day.CHCOM, year==2011), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2011")
abline(0,1, col = "red", pch=".")

library(lattice)
xyplot(DailyMean~predictGAM|factor(month), data = data.NW.day.COM,
       layout=c(6,2),panel=function(...){panel.xyplot(...)
         panel.abline(0,1)
       },
       xlab='Predict',ylab='Stream Temperature')






#### * Monthly predictions ####


data.NW.day.COM$year_month_site <- paste(data.NW.day.COM$year,"_",data.NW.day.COM$month, "_",data.NW.day.COM$PERMA_FID.x,sep = "")
month.avgNW <- as.data.frame.table(tapply(data.NW.day.COM$AvgDailyTemp, data.NW.day.COM$year_month_site,  mean))
colnames(month.avgNW)<- c("year_month_site","AvgMonthTemp")
month.avgNW$SiteName<- sub(".*_", "", month.avgNW$year_month_site)
head(month.avgNW)
monthNW <- as.data.frame.table(tapply(data.NW.day.COM$month, data.NW.day.COM$year_month_site,  mean))
yearNW <- as.data.frame.table(tapply(data.NW.day.COM$year, data.NW.day.COM$year_month_site,  mean))
month.avgNW$year <- yearNW$Freq
month.avgNW$month <- monthNW$Freq
month.avgNW.GAM <- as.data.frame.table(tapply(data.NW.day.COM$predictGAM, data.NW.day.COM$year_month_site,  mean))
month.avgNW.LIN <- as.data.frame.table(tapply(data.NW.day.COM$predictLIN, data.NW.day.COM$year_month_site,  mean))
month.avgNW$predGAM <- month.avgNW.GAM$Freq
month.avgNW$predLIN<- month.avgNW.LIN$Freq
month.avgNW$resid <- month.avgNW$predGAM - month.avgNW$AvgMonthTemp
month.avgNW$residLin <- month.avgNW$predLIN - month.avgNW$AvgMonthTemp
month.avgNW$resid2 <- (month.avgNW$resid)^2
month.avgNW$residLin2 <- (month.avgNW$residLin)^2
mean(month.avgNW$resid2)^.5 #1.061324
mean(month.avgNW$residLin2)^.5 #1.158937

month.avgNW.11 <- subset(month.avgNW, year == 2011)
month.avgNW.10 <- subset(month.avgNW, year == 2010)
month.avgNW.9 <- subset(month.avgNW, year == 2009)
month.avgNW.8 <- subset(month.avgNW, year == 2008)
month.avgNW.7 <- subset(month.avgNW, year == 2007)
month.avgNW.6 <- subset(month.avgNW, year == 2006)
month.avgNW.5 <- subset(month.avgNW, year == 2005)
month.avgNW.4 <- subset(month.avgNW, year == 2004)
month.avgNW.3 <- subset(month.avgNW, year == 2003)

WenatcheeYearRMSE[10,5]<-mean(month.avgNW$resid2)^.5 #1.061324
WenatcheeYearRMSE[9,5]<-mean(month.avgNW.11$resid2)^.5 #1.184645
WenatcheeYearRMSE[8,5]<-mean(month.avgNW.10$resid2)^.5 #1.005682
WenatcheeYearRMSE[7,5]<-mean(month.avgNW.9$resid2)^.5 #0.9335747
WenatcheeYearRMSE[6,5]<-mean(month.avgNW.8$resid2)^.5 #0.9727397
WenatcheeYearRMSE[5,5]<-mean(month.avgNW.7$resid2)^.5 #0.9307822
WenatcheeYearRMSE[4,5]<-mean(month.avgNW.6$resid2)^.5 #1.070991
WenatcheeYearRMSE[3,5]<-mean(month.avgNW.5$resid2)^.5 #1.211996
WenatcheeYearRMSE[2,5]<-mean(month.avgNW.4$resid2)^.5 #1.157756
WenatcheeYearRMSE[1,5]<-mean(month.avgNW.3$resid2)^.5 #1.005839

WenatcheeYearRMSELin[10,5]<-mean(month.avgNW$residLin2)^.5 #1.061324
WenatcheeYearRMSELin[9,5]<- mean(month.avgNW.11$residLin2)^.5 #1.183493
WenatcheeYearRMSELin[8,5]<-mean(month.avgNW.10$residLin2)^.5 #1.035941
WenatcheeYearRMSELin[7,5]<-mean(month.avgNW.9$residLin2)^.5 #1.024913
WenatcheeYearRMSELin[6,5]<-mean(month.avgNW.8$residLin2)^.5 #1.044756
WenatcheeYearRMSELin[5,5]<-mean(month.avgNW.7$residLin2)^.5 #0.9772636
WenatcheeYearRMSELin[4,5]<-mean(month.avgNW.6$residLin2)^.5 #1.160099
WenatcheeYearRMSELin[3,5]<-mean(month.avgNW.5$residLin2)^.5 #1.351713
WenatcheeYearRMSELin[2,5]<-mean(month.avgNW.4$residLin2)^.5 #1.401978
WenatcheeYearRMSELin[1,5]<-mean(month.avgNW.3$residLin2)^.5 #1.079134
monthly.modNW <-lm(AvgMonthTemp~predGAM, month.avgNW)
summary(monthly.modNW) #1.05, 0.9526

win.graph(width =5, height = 4.5)
par(cex=.9, ps=10)
plot(AvgMonthTemp~predGAM, month.avgNW, ylab = "Tw (°C)", xlab = "predicted Tw (°C)")
abline(0,1, col ="red")

#site and data counts 
library(plyr)
count<-count(month.avgNW, "year")
count<-count(month.avgNW.CH, "year")

with(data.NW.day.COM, tapply(PERMA_FID.x,year, FUN = function(x) length(unique(x))))
with(data.NW.day.CHCOM, tapply(PERMA_FID.x,year, FUN = function(x) length(unique(x))))

length(unique(data.NW.day.COM$PERMA_FID.x))
length(unique(data.NW.day.CHCOM$PERMA_FID.x))


#by month
monthly.mod1NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==1))
monthly.mod2NW<-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==2))
monthly.mod3NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==3))
monthly.mod4NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==4))
monthly.mod5NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==5))
monthly.mod6NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==6))
monthly.mod7NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==7))
monthly.mod8NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==8))
monthly.mod9NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==9))
monthly.mod10NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==10))
monthly.mod11NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==11))
monthly.mod12NW <-lm(AvgMonthTemp~predGAM, data= subset(month.avgNW, month==12))
#RSE     R2 #not updated
summary(monthly.mod1NW) #0.7613, 0.4453
summary(monthly.mod2NW) #0.5715, 0.5301
summary(monthly.mod3NW) #0.4845, 0.7738
summary(monthly.mod4NW) #0.6232, 0.7856
summary(monthly.mod5NW) #0.8372, 0.7979
summary(monthly.mod6NW) #1.109, 0.8205
summary(monthly.mod7NW) #1.319, 0.7477
summary(monthly.mod8NW) #1.298, 0.6993
summary(monthly.mod9NW) #0.8819, 0.7939
summary(monthly.mod10NW)#0.7868, 0.8006
summary(monthly.mod11NW)#1.203, 0.6925
summary(monthly.mod12NW)#1.119,0.5059
month.avgNW.m12 <- subset(month.avgNW, month == 12)
month.avgNW.m11 <- subset(month.avgNW, month == 11)
month.avgNW.m10 <- subset(month.avgNW, month == 10)
month.avgNW.m9 <- subset(month.avgNW, month == 9)
month.avgNW.m8 <- subset(month.avgNW, month == 8)
month.avgNW.m7 <- subset(month.avgNW, month == 7)
month.avgNW.m6 <- subset(month.avgNW, month == 6)
month.avgNW.m5 <- subset(month.avgNW, month == 5)
month.avgNW.m4 <- subset(month.avgNW, month == 4)
month.avgNW.m3 <- subset(month.avgNW, month == 3)
month.avgNW.m2 <- subset(month.avgNW, month == 2)
month.avgNW.m1 <- subset(month.avgNW, month == 1)





#### * Monthly predictions Chiwawa ####

data.NW.day.CHCOM$year_month_site <- paste(data.NW.day.CHCOM$year,"_",data.NW.day.CHCOM$month, "_",data.NW.day.CHCOM$PERMA_FID.x,sep = "")
month.avgNW.CH <- as.data.frame.table(tapply(data.NW.day.CHCOM$AvgDailyTemp, data.NW.day.CHCOM$year_month_site,  mean))
colnames(month.avgNW.CH)<- c("year_month_site","AvgMonthTemp")
month.avgNW.CH$SiteName<- sub(".*_", "", month.avgNW.CH$year_month_site)
head(month.avgNW.CH)
monthNW.CH <- as.data.frame.table(tapply(data.NW.day.CHCOM$month, data.NW.day.CHCOM$year_month_site,  mean))
yearNW.CH <- as.data.frame.table(tapply(data.NW.day.CHCOM$year, data.NW.day.CHCOM$year_month_site,  mean))
month.avgNW.CH$year <- yearNW.CH$Freq
month.avgNW.CH$month <- monthNW.CH$Freq
month.avgNW.GAM.CH <- as.data.frame.table(tapply(data.NW.day.CHCOM$predictGAM, data.NW.day.CHCOM$year_month_site,  mean))
month.avgNW.LIN.CH <- as.data.frame.table(tapply(data.NW.day.CHCOM$predictLIN, data.NW.day.CHCOM$year_month_site,  mean))
month.avgNW.CH$predGAM <- month.avgNW.GAM.CH$Freq
month.avgNW.CH$predLIN<- month.avgNW.LIN.CH$Freq
month.avgNW.CH$resid <- month.avgNW.CH$predGAM - month.avgNW.CH$AvgMonthTemp
month.avgNW.CH$residLin <- month.avgNW.CH$predLIN - month.avgNW.CH$AvgMonthTemp
month.avgNW.CH$resid2 <- (month.avgNW.CH$resid)^2
month.avgNW.CH$residLIN2 <- (month.avgNW.CH$residLin)^2
mean(month.avgNW.CH$resid2)^.5 #0.6548706 
mean(month.avgNW.CH$residLIN2)^.5 #0.6158936 


month.avgNW.CH11 <- subset(month.avgNW.CH, year == 2011)
month.avgNW.CH10 <- subset(month.avgNW.CH, year == 2010)
month.avgNW.CH9 <- subset(month.avgNW.CH, year == 2009)
month.avgNW.CH8 <- subset(month.avgNW.CH, year == 2008)
month.avgNW.CH7 <- subset(month.avgNW.CH, year == 2007)
month.avgNW.CH6 <- subset(month.avgNW.CH, year == 2006)
month.avgNW.CH5 <- subset(month.avgNW.CH, year == 2005)
month.avgNW.CH4 <- subset(month.avgNW.CH, year == 2004)
month.avgNW.CH3 <- subset(month.avgNW.CH, year == 2003)
ChiwawaYearRMSE[10,5]<-mean(month.avgNW.CH$resid2)^.5 #0.6548706 RMSE!!!
ChiwawaYearRMSE[9,5]<- mean(month.avgNW.CH11$resid2)^.5 #1.031101
ChiwawaYearRMSE[8,5]<-mean(month.avgNW.CH10$resid2)^.5 #0.7212222
ChiwawaYearRMSE[7,5]<-mean(month.avgNW.CH9$resid2)^.5 #0.5545797
ChiwawaYearRMSE[6,5]<-mean(month.avgNW.CH8$resid2)^.5 #0.618978
ChiwawaYearRMSE[5,5]<-mean(month.avgNW.CH7$resid2)^.5 #0.3954536
ChiwawaYearRMSE[4,5]<-mean(month.avgNW.CH6$resid2)^.5 #0.5111774
ChiwawaYearRMSE[3,5]<-mean(month.avgNW.CH5$resid2)^.5 #0.6713916
ChiwawaYearRMSE[2,5]<-mean(month.avgNW.CH4$resid2)^.5 #0.8213561
ChiwawaYearRMSE[1,5]<-mean(month.avgNW.CH3$resid2)^.5 #0.7809275

ChiwawaYearRMSELin[10,5]<-mean(month.avgNW.CH$residLIN2)^.5 #0.6158936 RMSE!!!
ChiwawaYearRMSELin[9,5]<- mean(month.avgNW.CH11$residLIN2)^.5 #1.031101
ChiwawaYearRMSELin[8,5]<-mean(month.avgNW.CH10$residLIN2)^.5 #0.7212222
ChiwawaYearRMSELin[7,5]<-mean(month.avgNW.CH9$residLIN2)^.5 #0.5545797
ChiwawaYearRMSELin[6,5]<-mean(month.avgNW.CH8$residLIN2)^.5 #0.618978
ChiwawaYearRMSELin[5,5]<-mean(month.avgNW.CH7$residLIN2)^.5 #0.3954536
ChiwawaYearRMSELin[4,5]<-mean(month.avgNW.CH6$residLIN2)^.5 #0.5111774
ChiwawaYearRMSELin[3,5]<-mean(month.avgNW.CH5$residLIN2)^.5 #0.6713916
ChiwawaYearRMSELin[2,5]<-mean(month.avgNW.CH4$residLIN2)^.5 #0.8213561
ChiwawaYearRMSELin[1,5]<-mean(month.avgNW.CH3$residLIN2)^.5 #0.7809275





# RMSE by month table (for all) ####
#made for all rivers (Wen, Chiwawa, MFJD, Tucannon)
Monthly_Table <- data.frame(matrix(NA, nrow = 13, ncol = 17))
colnames(Monthly_Table)<- c("month","GAM RMSE","GAM NSC","linear RMSE","linear NSC","GAM RMSE","GAM NSC","linear RMSE","linear NSC","GAM RMSE","GAM NSC","linear RMSE","linear NSC","GAM RMSE","GAM NSC","linear RMSE","linear NSC")
Monthly_Table$month<-c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December","year")
Monthly_Table[13,2]<- mean(month.avgNW$resid2)^.5 #1.213611
Monthly_Table[12,2]<- mean(month.avgNW.m12$resid2)^.5 #1.213611
Monthly_Table[11,2]<-mean(month.avgNW.m11$resid2)^.5 #1.483114
Monthly_Table[10,2]<-mean(month.avgNW.m10$resid2)^.5 #0.9386101
Monthly_Table[9,2]<-mean(month.avgNW.m9$resid2)^.5 #0.877668
Monthly_Table[8,2]<-mean(month.avgNW.m8$resid2)^.5 #1.289687
Monthly_Table[7,2]<-mean(month.avgNW.m7$resid2)^.5 #1.213196
Monthly_Table[6,2]<-mean(month.avgNW.m6$resid2)^.5 #1.137688
Monthly_Table[5,2]<-mean(month.avgNW.m5$resid2)^.5 #0.9633347
Monthly_Table[4,2]<-mean(month.avgNW.m4$resid2)^.5 #0.787521
Monthly_Table[3,2]<-mean(month.avgNW.m3$resid2)^.5 #0.6247667
Monthly_Table[2,2]<-mean(month.avgNW.m2$resid2)^.5 #0.6203161
Monthly_Table[1,2]<-mean(month.avgNW.m1$resid2)^.5 #0.8083223

Monthly_Table[13,3]<- 1 - ((sum((month.avgNW$resid)^2))/(sum((month.avgNW$AvgMonthTemp-(mean(month.avgNW$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,3]<- 1 - ((sum((month.avgNW.m12$resid)^2))/(sum((month.avgNW.m12$AvgMonthTemp-(mean(month.avgNW.m12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,3]<- 1 - ((sum((month.avgNW.m11$resid)^2))/(sum((month.avgNW.m11$AvgMonthTemp-(mean(month.avgNW.m11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,3]<- 1 - ((sum((month.avgNW.m10$resid)^2))/(sum((month.avgNW.m10$AvgMonthTemp-(mean(month.avgNW.m10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,3]<- 1 - ((sum((month.avgNW.m9$resid)^2))/(sum((month.avgNW.m9$AvgMonthTemp-(mean(month.avgNW.m9$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,3]<- 1 - ((sum((month.avgNW.m8$resid)^2))/(sum((month.avgNW.m8$AvgMonthTemp-(mean(month.avgNW.m8$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,3]<- 1 - ((sum((month.avgNW.m7$resid)^2))/(sum((month.avgNW.m7$AvgMonthTemp-(mean(month.avgNW.m7$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,3]<- 1 - ((sum((month.avgNW.m6$resid)^2))/(sum((month.avgNW.m6$AvgMonthTemp-(mean(month.avgNW.m6$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,3]<- 1 - ((sum((month.avgNW.m5$resid)^2))/(sum((month.avgNW.m5$AvgMonthTemp-(mean(month.avgNW.m5$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,3]<- 1 - ((sum((month.avgNW.m4$resid)^2))/(sum((month.avgNW.m4$AvgMonthTemp-(mean(month.avgNW.m4$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,3]<- 1 - ((sum((month.avgNW.m3$resid)^2))/(sum((month.avgNW.m3$AvgMonthTemp-(mean(month.avgNW.m3$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,3]<- 1 - ((sum((month.avgNW.m2$resid)^2))/(sum((month.avgNW.m2$AvgMonthTemp-(mean(month.avgNW.m2$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,3]<- 1 - ((sum((month.avgNW.m1$resid)^2))/(sum((month.avgNW.m1$AvgMonthTemp-(mean(month.avgNW.m1$AvgMonthTemp)))^2))) #0.402458

Monthly_Table[13,4]<- mean(month.avgNW$residLin2)^.5 #1.213611
Monthly_Table[12,4]<- mean(month.avgNW.m12$residLin2)^.5 #1.213611
Monthly_Table[11,4]<-mean(month.avgNW.m11$residLin2)^.5 #1.483114
Monthly_Table[10,4]<-mean(month.avgNW.m10$residLin2)^.5 #0.9386101
Monthly_Table[9,4]<-mean(month.avgNW.m9$residLin2)^.5 #0.877668
Monthly_Table[8,4]<-mean(month.avgNW.m8$residLin2)^.5 #1.289687
Monthly_Table[7,4]<-mean(month.avgNW.m7$residLin2)^.5 #1.213196
Monthly_Table[6,4]<-mean(month.avgNW.m6$residLin2)^.5 #1.137688
Monthly_Table[5,4]<-mean(month.avgNW.m5$residLin2)^.5 #0.9633347
Monthly_Table[4,4]<-mean(month.avgNW.m4$residLin2)^.5 #0.787521
Monthly_Table[3,4]<-mean(month.avgNW.m3$residLin2)^.5 #0.6247667
Monthly_Table[2,4]<-mean(month.avgNW.m2$residLin2)^.5 #0.6203161
Monthly_Table[1,4]<-mean(month.avgNW.m1$residLin2)^.5 #0.8083223

Monthly_Table[13,5]<- 1 - ((sum((month.avgNW$residLin)^2))/(sum((month.avgNW$AvgMonthTemp-(mean(month.avgNW$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,5]<- 1 - ((sum((month.avgNW.m12$residLin)^2))/(sum((month.avgNW.m12$AvgMonthTemp-(mean(month.avgNW.m12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,5]<- 1 - ((sum((month.avgNW.m11$residLin)^2))/(sum((month.avgNW.m11$AvgMonthTemp-(mean(month.avgNW.m11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,5]<- 1 - ((sum((month.avgNW.m10$residLin)^2))/(sum((month.avgNW.m10$AvgMonthTemp-(mean(month.avgNW.m10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,5]<- 1 - ((sum((month.avgNW.m9$residLin)^2))/(sum((month.avgNW.m9$AvgMonthTemp-(mean(month.avgNW.m9$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,5]<- 1 - ((sum((month.avgNW.m8$residLin)^2))/(sum((month.avgNW.m8$AvgMonthTemp-(mean(month.avgNW.m8$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,5]<- 1 - ((sum((month.avgNW.m7$residLin)^2))/(sum((month.avgNW.m7$AvgMonthTemp-(mean(month.avgNW.m7$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,5]<- 1 - ((sum((month.avgNW.m6$residLin)^2))/(sum((month.avgNW.m6$AvgMonthTemp-(mean(month.avgNW.m6$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,5]<- 1 - ((sum((month.avgNW.m5$residLin)^2))/(sum((month.avgNW.m5$AvgMonthTemp-(mean(month.avgNW.m5$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,5]<- 1 - ((sum((month.avgNW.m4$residLin)^2))/(sum((month.avgNW.m4$AvgMonthTemp-(mean(month.avgNW.m4$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,5]<- 1 - ((sum((month.avgNW.m3$residLin)^2))/(sum((month.avgNW.m3$AvgMonthTemp-(mean(month.avgNW.m3$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,5]<- 1 - ((sum((month.avgNW.m2$residLin)^2))/(sum((month.avgNW.m2$AvgMonthTemp-(mean(month.avgNW.m2$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,5]<- 1 - ((sum((month.avgNW.m1$residLin)^2))/(sum((month.avgNW.m1$AvgMonthTemp-(mean(month.avgNW.m1$AvgMonthTemp)))^2))) #0.402458



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





#BY MONTH  Chiwawa
month.avgNW.CHm12 <- subset(month.avgNW.CH, month == 12)
month.avgNW.CHm11 <- subset(month.avgNW.CH, month == 11)
month.avgNW.CHm10 <- subset(month.avgNW.CH, month == 10)
month.avgNW.CHm9 <- subset(month.avgNW.CH, month == 9)
month.avgNW.CHm8 <- subset(month.avgNW.CH, month == 8)
month.avgNW.CHm7 <- subset(month.avgNW.CH, month == 7)
month.avgNW.CHm6 <- subset(month.avgNW.CH, month == 6)
month.avgNW.CHm5 <- subset(month.avgNW.CH, month == 5)
month.avgNW.CHm4 <- subset(month.avgNW.CH, month == 4)
month.avgNW.CHm3 <- subset(month.avgNW.CH, month == 3)
month.avgNW.CHm2 <- subset(month.avgNW.CH, month == 2)
month.avgNW.CHm1 <- subset(month.avgNW.CH, month == 1)



#RMSE by month
Monthly_Table[13,6]<-mean(month.avgNW.CH$resid2)^.5 #0.7809275
Monthly_Table[12,6]<-mean(month.avgNW.CHm12$resid2)^.5 #0.7809275
Monthly_Table[11,6]<-mean(month.avgNW.CHm11$resid2)^.5 #0.8213561
Monthly_Table[10,6]<-mean(month.avgNW.CHm10$resid2)^.5 #0.6713916
Monthly_Table[9,6]<-mean(month.avgNW.CHm9$resid2)^.5 #0.5491844
Monthly_Table[8,6]<-mean(month.avgNW.CHm8$resid2)^.5 #0.8648771
Monthly_Table[7,6]<-mean(month.avgNW.CHm7$resid2)^.5 #0.726188
Monthly_Table[6,6]<-mean(month.avgNW.CHm6$resid2)^.5 #0.726188
Monthly_Table[5,6]<-mean(month.avgNW.CHm5$resid2)^.5 #0.3954536
Monthly_Table[4,6]<-mean(month.avgNW.CHm4$resid2)^.5 #0.5111774
Monthly_Table[3,6]<-mean(month.avgNW.CHm3$resid2)^.5 #0.4699066
Monthly_Table[2,6]<-mean(month.avgNW.CHm2$resid2)^.5 #0.3152228
Monthly_Table[1,6]<-mean(month.avgNW.CHm1$resid2)^.5 #0.2999073

Monthly_Table[13,7]<- 1 - ((sum((month.avgNW.CH$resid)^2))/(sum((month.avgNW.CH$AvgMonthTemp-(mean(month.avgNW.CH$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,7]<- 1 - ((sum((month.avgNW.CHm12$resid)^2))/(sum((month.avgNW.CHm12$AvgMonthTemp-(mean(month.avgNW.CHm12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,7]<- 1 - ((sum((month.avgNW.CHm11$resid)^2))/(sum((month.avgNW.CHm11$AvgMonthTemp-(mean(month.avgNW.CHm11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,7]<- 1 - ((sum((month.avgNW.CHm10$resid)^2))/(sum((month.avgNW.CHm10$AvgMonthTemp-(mean(month.avgNW.CHm10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,7]<- 1 - ((sum((month.avgNW.CHm9$resid)^2))/(sum((month.avgNW.CHm9$AvgMonthTemp-(mean(month.avgNW.CHm9$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,7]<- 1 - ((sum((month.avgNW.CHm8$resid)^2))/(sum((month.avgNW.CHm8$AvgMonthTemp-(mean(month.avgNW.CHm8$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,7]<- 1 - ((sum((month.avgNW.CHm7$resid)^2))/(sum((month.avgNW.CHm7$AvgMonthTemp-(mean(month.avgNW.CHm7$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,7]<- 1 - ((sum((month.avgNW.CHm6$resid)^2))/(sum((month.avgNW.CHm6$AvgMonthTemp-(mean(month.avgNW.CHm6$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,7]<- 1 - ((sum((month.avgNW.CHm5$resid)^2))/(sum((month.avgNW.CHm5$AvgMonthTemp-(mean(month.avgNW.CHm5$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,7]<- 1 - ((sum((month.avgNW.CHm4$resid)^2))/(sum((month.avgNW.CHm4$AvgMonthTemp-(mean(month.avgNW.CHm4$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,7]<- 1 - ((sum((month.avgNW.CHm3$resid)^2))/(sum((month.avgNW.CHm3$AvgMonthTemp-(mean(month.avgNW.CHm3$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,7]<- 1 - ((sum((month.avgNW.CHm2$resid)^2))/(sum((month.avgNW.CHm2$AvgMonthTemp-(mean(month.avgNW.CHm2$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,7]<- 1 - ((sum((month.avgNW.CHm1$resid)^2))/(sum((month.avgNW.CHm1$AvgMonthTemp-(mean(month.avgNW.CHm1$AvgMonthTemp)))^2))) #0.402458

Monthly_Table[13,8]<-mean(month.avgNW.CH$residLIN2)^.5 #0.7809275
Monthly_Table[12,8]<-mean(month.avgNW.CHm12$residLIN2)^.5 #0.7809275
Monthly_Table[11,8]<-mean(month.avgNW.CHm11$residLIN2)^.5 #0.8213561
Monthly_Table[10,8]<-mean(month.avgNW.CHm10$residLIN2)^.5 #0.6713916
Monthly_Table[9,8]<-mean(month.avgNW.CHm9$residLIN2)^.5 #0.5491844
Monthly_Table[8,8]<-mean(month.avgNW.CHm8$residLIN2)^.5 #0.8648771
Monthly_Table[7,8]<-mean(month.avgNW.CHm7$residLIN2)^.5 #0.726188
Monthly_Table[6,8]<-mean(month.avgNW.CHm6$residLIN2)^.5 #0.726188
Monthly_Table[5,8]<-mean(month.avgNW.CHm5$residLIN2)^.5 #0.3954536
Monthly_Table[4,8]<-mean(month.avgNW.CHm4$residLIN2)^.5 #0.5111774
Monthly_Table[3,8]<-mean(month.avgNW.CHm3$residLIN2)^.5 #0.4699066
Monthly_Table[2,8]<-mean(month.avgNW.CHm2$residLIN2)^.5 #0.3152228
Monthly_Table[1,8]<-mean(month.avgNW.CHm1$residLIN2)^.5 #0.2999073

Monthly_Table[13,9]<- 1 - ((sum((month.avgNW.CH$residLin)^2))/(sum((month.avgNW.CH$AvgMonthTemp-(mean(month.avgNW.CH$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,9]<- 1 - ((sum((month.avgNW.CHm12$residLin)^2))/(sum((month.avgNW.CHm12$AvgMonthTemp-(mean(month.avgNW.CHm12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,9]<- 1 - ((sum((month.avgNW.CHm11$residLin)^2))/(sum((month.avgNW.CHm11$AvgMonthTemp-(mean(month.avgNW.CHm11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,9]<- 1 - ((sum((month.avgNW.CHm10$residLin)^2))/(sum((month.avgNW.CHm10$AvgMonthTemp-(mean(month.avgNW.CHm10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,9]<- 1 - ((sum((month.avgNW.CHm9$residLin)^2))/(sum((month.avgNW.CHm9$AvgMonthTemp-(mean(month.avgNW.CHm9$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,9]<- 1 - ((sum((month.avgNW.CHm8$residLin)^2))/(sum((month.avgNW.CHm8$AvgMonthTemp-(mean(month.avgNW.CHm8$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,9]<- 1 - ((sum((month.avgNW.CHm7$residLin)^2))/(sum((month.avgNW.CHm7$AvgMonthTemp-(mean(month.avgNW.CHm7$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,9]<- 1 - ((sum((month.avgNW.CHm6$residLin)^2))/(sum((month.avgNW.CHm6$AvgMonthTemp-(mean(month.avgNW.CHm6$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,9]<- 1 - ((sum((month.avgNW.CHm5$residLin)^2))/(sum((month.avgNW.CHm5$AvgMonthTemp-(mean(month.avgNW.CHm5$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,9]<- 1 - ((sum((month.avgNW.CHm4$residLin)^2))/(sum((month.avgNW.CHm4$AvgMonthTemp-(mean(month.avgNW.CHm4$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,9]<- 1 - ((sum((month.avgNW.CHm3$residLin)^2))/(sum((month.avgNW.CHm3$AvgMonthTemp-(mean(month.avgNW.CHm3$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,9]<- 1 - ((sum((month.avgNW.CHm2$residLin)^2))/(sum((month.avgNW.CHm2$AvgMonthTemp-(mean(month.avgNW.CHm2$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,9]<- 1 - ((sum((month.avgNW.CHm1$residLin)^2))/(sum((month.avgNW.CHm1$AvgMonthTemp-(mean(month.avgNW.CHm1$AvgMonthTemp)))^2))) #0.402458




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


win.graph(width = 12, height = 8)
par(mfrow= c(3,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==1), ylim =c(0,22), xlim = c (0,22), main = "January")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==2), ylim =c(0,22), xlim = c (0,22), main = "February")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==3), ylim =c(0,22), xlim = c (0,22), main = "March")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==4), ylim =c(0,22), xlim = c (0,22), main = "April")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==5), ylim =c(0,22), xlim = c (0,22), main = "May")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==6), ylim =c(0,22), xlim = c (0,22), main = "June")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==7), ylim =c(0,22), xlim = c (0,22), main = "July")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==8), ylim =c(0,22), xlim = c (0,22), main = "August")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==9), ylim =c(0,22), xlim = c (0,22), main = "September")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==10), ylim =c(0,22), xlim = c (0,22), main = "October")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==11), ylim =c(0,22), xlim = c (0,22),main = "November")
abline(0,1, col = "red")
plot(AvgMonthTemp~predGAM,data= subset(month.avgNW, month==12),ylim =c(0,22), xlim = c(0,22), main = "December")
abline(0,1, col = "red")











#### ............... ####
#### TUCANNON MODELS ####

#### * Load data ####
setwd(WD)
Tuc_NW <-read.csv("Tuc_NWdata_day.csv")
Tuc_Champ <-read.csv("Tuc_Champ_day.csv")




#### * Smoother to split spring/fall ####

#predicting peak temperature with GAM for seasonal model splitting
library(mgcv)
Tuc_Champ$predSmooth <- predict(gam(AvgDailyTemp~s(JulianDate, k=10), data = Tuc_Champ))
Tuc_NW$predSmooth <- predict(gam(DailyMean~s(Julian, k=10), data = Tuc_NW))

Tuc_Champ$residSmooth <- resid(gam(AvgDailyTemp~s(JulianDate, k=10), data = Tuc_Champ))
Tuc_NW$residSmooth <- resid(gam(DailyMean~s(Julian, k=10), data = Tuc_NW))

plot(Tuc_Champ$JulianDate,Tuc_Champ$AvgDailyTemp, pch = 16, ylab = "Julian date", xlab = "stream temperature")
points(Tuc_Champ$JulianDate,Tuc_Champ$predSmooth, pch = 16, col = "red")

# * Data cleaning ####
weird.CH <- as.data.frame.table(tapply(Tuc_Champ$residSmooth,Tuc_Champ$SiteName, sd))
weird.CH <- weird.CH[order(-weird.CH$Freq),]
weird.CH  #major error
plot(AvgDailyTemp~JulianDate,data = subset(Tuc_Champ, SiteName == "CBW05583-310143"), pch = 16, ylab = "Julian date", xlab = "stream temperature")
points(Tuc_Champ$JulianDate,Tuc_Champ$predSmooth, pch = 16, col = "red")#uggle site, not sure what is going on but will remove
badsitesCH <- c("CBW05583-310143") # this site appears to be two sites, removing
Tuc_Champ <- subset(Tuc_Champ, !(SiteName %in% badsitesCH))



#### * Model fitting and metrics ####

#reducing to only mainstem sites
Tuc_Champ_mainstem <- subset(Tuc_Champ, GNIS_NAME == "Tucannon River")
Tuc_NW_mainstem <- subset(Tuc_NW, GNIS_NAME == "Tucannon River")

library(plyr)
count<-count(Tuc_Champ_mainstem, "Month")
count<-count(Tuc_NW_mainstem, "month")
length(unique(Tuc_Champ_mainstem$SiteName))
length(unique(Tuc_NW_mainstem$PERMA_FID.x))

Tuc_Champ_mainstem$predSmooth <- predict(gam(AvgDailyTemp~s(JulianDate, k=10), data = Tuc_Champ_mainstem))
smoothCH <- as.data.frame.table(tapply(Tuc_Champ_mainstem$predSmooth,Tuc_Champ_mainstem$JulianDate, mean))
#day 206 is max temp 

Tuc_NW_mainstem$predSmooth <- predict(gam(DailyMean~s(Julian, k=10), data = Tuc_NW_mainstem))
plot(Tuc_NW_mainstem$Julian,Tuc_NW_mainstem$DailyMean, pch = 16, ylab = "Julian date", xlab = "stream temperature")
points(Tuc_NW_mainstem$Julian,Tuc_NW_mainstem$predSmooth, pch = 16, col = "blue")


#splitting by max temp day of CHAMP data
Tuc_Champsp <- subset(Tuc_Champ_mainstem, JulianDate < 207)
Tuc_Champfl <- subset(Tuc_Champ_mainstem, JulianDate > 206)
Tuc_NWsp <- subset(Tuc_NW_mainstem, Julian< 207)
Tuc_NWfl <- subset(Tuc_NW_mainstem, Julian > 206)


######GAM models
TucGsp <- (gam(AvgDailyTemp ~ s(TAVG5, k = 4) +s(Tchange5,k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG5, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVG5, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(Echange2, by = TAVG5, k = 3), data = Tuc_Champsp))
TucGfl <- (gam(AvgDailyTemp ~ s(TAVG3, k = 4) +s(Tchange3,k = 3)+s(SNWD.lin,by = TAVG3, k = 3) +s(ae,by = SNWD.lin, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG3, k = 3)+ s(dailymeanCMS, by = TAVG3, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(ae, by = JulianDate, k = 5) + s(Echange2, by = TAVG3, k = 3), data = Tuc_Champfl))
summary(TucGsp)#0.97   
summary(TucGfl)#0.957 

######Linear Modesl 
TucLsp <- (lm(AvgDailyTemp ~ TAVG5 +Tchange5+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+catch_area*TAVG5+ae*JulianDate+dailymeanCMS*TAVG5+JulianDate*TAVG5 + Echange2*TAVG5, data = Tuc_Champsp))
TucLfl <- (lm(AvgDailyTemp ~ TAVG3 + Tchange3 + SNWD.lin*TAVG3+SNWD.lin*ae +catch_area*ae+catch_area*TAVG3+dailymeanCMS*TAVG3+JulianDate*TAVG5+ae*JulianDate+Echange2*TAVG3, data = Tuc_Champfl))
summary(TucLsp) #0.9096, 0.9666   
summary(TucLfl) #1.126, 0.9516  


####* Goodness of fit 

####
Tuc_Champsp$predictCHAMP  <- predict(TucGsp)
Tuc_Champfl$predictCHAMP  <- predict(TucGfl)
Tuc_Champsp$residCHAMP  <- resid(TucGsp)
Tuc_Champfl$residCHAMP  <- resid(TucGfl)
Tuc_Champsp$resid2  <- (Tuc_Champsp$residCHAMP)^2
Tuc_Champfl$resid2  <- (Tuc_Champfl$residCHAMP)^2
mean(Tuc_Champsp$resid2)^.5 #0.8629833
mean(Tuc_Champfl$resid2)^.5 #1.057236

Tuc_Champsp$predictLin <- predict(TucLsp)
Tuc_Champfl$predictLin  <- predict(TucLfl)
Tuc_Champsp$residLin  <- resid(TucLsp)
Tuc_Champfl$residLin  <- resid(TucLfl)
Tuc_Champsp$residLin2  <- (Tuc_Champsp$residLin)^2
Tuc_Champfl$residLin2  <- (Tuc_Champfl$residLin)^2
mean(Tuc_Champsp$residLin2)^.5 #0.9093941
mean(Tuc_Champfl$residLin2)^.5 #1.125708




#### * Relationship explorations ####
#GAM visreg
visreg(TucGsp, "TAVG5")
visreg2d(TucGsp, "TAVG5", "Tchange5", plot.type="persp",nn=12)  
visreg2d(TucGsp, "A1Snow", "JulianDate", plot.type="persp",nn=12)  
visreg2d(TucGsp, "JulianDate", "TAVG5", plot.type="persp",nn=12)  
visreg2d(TucGsp, "dailymeanCMS", "TAVG5", plot.type="persp",nn=12) 
visreg2d(TucGsp, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(TucGsp, "catch_area", "dailymeanCMS", plot.type="persp",nn=12) 
visreg2d(TucGsp, "catch_area", "TAVG5", plot.type="persp",nn=12) 
visreg2d(TucGsp, "ae", "TAVG5", plot.type="persp",nn=12)
visreg2d(TucGsp, "ae", "JulianDate", plot.type="persp",nn=12)  
visreg2d(TucGsp, "Echange2", "TAVG5", plot.type="persp",nn=12)  
visreg2d(TucGsp, "JulianDate", "ae", plot.type="persp",nn=12)

visreg(TucGfl, "TAVG3",nn=12)
visreg2d(TucGfl, "Tchange3", "TAVG3", plot.type="persp",nn=12)  
visreg2d(TucGfl, "SNWD.lin", "TAVG3", plot.type="persp",nn=12)   
visreg2d(TucGfl, "JulianDate", "TAVG3", plot.type="persp",nn=12)
visreg2d(TucGfl, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(TucGfl, "catch_area", "TAVG3", plot.type="persp",nn=12) 
visreg2d(TucGfl, "dailymeanCMS", "TAVG3", plot.type="persp",nn=12) 
visreg2d(TucGfl, "ae", "TAVG3", plot.type="persp",nn=12) 
visreg2d(TucGfl, "SNWD.lin", "ae", plot.type="persp",nn=12)   
visreg2d(TucGfl, "JulianDate", "ae", plot.type="persp",nn=12)  
visreg2d(TucGfl, "Echange2", "TAVG3", plot.type="persp",nn=12) 
visreg2d(TucGfl, "JulianDate", "ae", plot.type="persp",nn=12)

#linear visreg
visreg(TucLsp, "TAVG5")
visreg2d(TucLsp, "TAVG5", "Tchange5", plot.type="persp",nn=12)  
visreg2d(TucLsp, "A1Snow", "JulianDate", plot.type="persp",nn=12)  
visreg2d(TucLsp, "JulianDate", "TAVG5", plot.type="persp",nn=12)  
visreg2d(TucLsp, "dailymeanCMS", "TAVG5", plot.type="persp",nn=12)  
visreg2d(TucLsp, "catch_area", "TAVG5", plot.type="persp",nn=12)  
visreg2d(TucLsp, "catch_area", "ae", plot.type="persp",nn=12) 
visreg2d(TucLsp, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  
visreg2d(TucLsp, "ae", "JulianDate", plot.type="persp",nn=12)  
visreg2d(TucLsp, "JulianDate", "TAVG5", plot.type="persp",nn=12)  
visreg2d(TucLsp, "Echange2", "TAVG5", plot.type="persp",nn=12)  

visreg(TucLfl, "TAVG3")
visreg2d(TucLfl, "Tchange3", "TAVG3", plot.type="persp",nn=12) 
visreg2d(TucLfl, "SNWD.lin", "TAVG3", plot.type="persp",nn=12)  
visreg2d(TucLfl, "JulianDate", "TAVG3", plot.type="persp",nn=12)  
visreg2d(TucLfl, "dailymeanCMS", "TAVG3", plot.type="persp",nn=12)  
visreg2d(TucLfl, "catch_area", "ae", plot.type="persp",nn=12) 
visreg2d(TucLfl, "catch_area", "TAVG3", plot.type="persp",nn=12) 
visreg2d(TucLfl, "JulianDate", "ae", plot.type="persp",nn=12)  
visreg2d(TucLfl, "Echange2", "TAVG3", plot.type="persp",nn=12)  
###




######### * LOOCV #########################################
library(mgcv)

###
####LOOCV spring Tucannon GAM  
resid_list <- matrix(ncol = 2, nrow = 0)
Tuc_Champsp$SiteName <- factor(Tuc_Champsp$SiteName)
Sites_springTUC <- unique(Tuc_Champsp$SiteName)
for(i in Sites_springTUC) {
  Tuc_Champsp_Ni <- subset(Tuc_Champsp, SiteName != i)
  Tuc_Champsp_i  <- subset(Tuc_Champsp, SiteName == i)
  mdl <- gam(AvgDailyTemp ~ s(TAVG5, k = 4) +s(Tchange5)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG5, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVG5, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(Echange2, by = TAVG5, k = 3), data = Tuc_Champsp_Ni) # leave i'th site out and refit model
  Tuc_Champsp_i$y_pred <- predict(mdl, newdata = Tuc_Champsp_i) #predict site data without site info
  Tuc_Champsp_i$y_pred[Tuc_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(Tuc_Champsp_i$AvgDailyTemp  - Tuc_Champsp_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_TucSp_GAM  <- (mean(resid_list$resid^2))^.5
tapply(abs(resid_list$resid),resid_list$site,mean)

####LOOCV fall Tucannon GAM  
resid_list <- vector(mode="numeric", length=0)
Tuc_Champfl$SiteName <- factor(Tuc_Champfl$SiteName)
Sites_fallTUC <- unique(Tuc_Champfl$SiteName)
for(i in Sites_fallTUC) {
  Tuc_Champfl_Ni <- subset(Tuc_Champfl, SiteName!=i)
  Tuc_Champfl_i <- subset(Tuc_Champfl, SiteName == i)
  mdl <- gam(AvgDailyTemp ~ s(TAVG3, k = 4) +s(Tchange3)+s(SNWD.lin,by = TAVG3, k = 3) +s(ae,by = SNWD.lin, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = TAVG3, k = 3)+ s(dailymeanCMS, by = TAVG3, k = 3) + s(JulianDate, by = TAVG5, k = 5)+ s(ae, by = JulianDate, k = 5) + s(Echange2, by = TAVG3, k = 3), data = Tuc_Champfl_Ni) # leave i'th site out and refit model
  Tuc_Champfl_i$y_pred <- predict(mdl, newdata = Tuc_Champfl_i) #predict site data without site info
  Tuc_Champfl_i$y_pred[Tuc_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(Tuc_Champfl_i$AvgDailyTemp  - Tuc_Champfl_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_TucFl_GAM  <- (mean(resid_list$resid^2))^.5
tapply(abs(resid_list$resid),resid_list$site,mean)


####LOOCV spring Tucannon Linear  
resid_list <- vector(mode="numeric", length=0)
Tuc_Champsp$SiteName <- factor(Tuc_Champsp$SiteName)
Sites_springTUC <- unique(Tuc_Champsp$SiteName)
for(i in Sites_springTUC) {
  Tuc_Champsp_Ni <- subset(Tuc_Champsp, SiteName!=i)
  Tuc_Champsp_i <- subset(Tuc_Champsp, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVG5 +Tchange5+A1Snow*JulianDate+catch_area*ae+catch_area*dailymeanCMS+catch_area*TAVG5+ae*JulianDate+dailymeanCMS*TAVG5+JulianDate*TAVG5 + Echange2*TAVG5, data = Tuc_Champsp_Ni) # leave i'th site out and refit model
  Tuc_Champsp_i$y_pred <- predict(mdl, newdata = Tuc_Champsp_i) #predict site data without site info
  Tuc_Champsp_i$y_pred[Tuc_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(Tuc_Champsp_i$AvgDailyTemp  - Tuc_Champsp_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_TucSp_linear  <- (mean(resid_list$resid^2))^.5
tapply(abs(resid_list$resid),resid_list$site,mean)

####LOOCV fall Tucannon Linear  
resid_list <- vector(mode="numeric", length=0)
Tuc_Champfl$SiteName <- factor(Tuc_Champfl$SiteName)
Sites_fallTUC <- unique(Tuc_Champfl$SiteName)
for(i in Sites_fallTUC) {
  Tuc_Champfl_Ni <- subset(Tuc_Champfl, SiteName!=i)
  Tuc_Champfl_i <- subset(Tuc_Champfl, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVG3 + Tchange3 + SNWD.lin*TAVG3+SNWD.lin*ae +catch_area*ae+catch_area*TAVG3+dailymeanCMS*TAVG3+JulianDate*TAVG5+ae*JulianDate+Echange2*TAVG3, data = Tuc_Champfl_Ni) # leave i'th site out and refit model
  Tuc_Champfl_i$y_pred <- predict(mdl, newdata = Tuc_Champfl_i) #predict site data without site info
  Tuc_Champfl_i$y_pred[Tuc_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(Tuc_Champfl_i$AvgDailyTemp  - Tuc_Champfl_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_TucFl_linear  <- (mean(resid_list$resid^2))^.5
tapply(abs(resid_list$resid),resid_list$site,mean)

RMSPE_TucSp_GAM #0.8734912
RMSPE_TucSp_linear #0.9195558
RMSPE_TucFl_GAM #1.063154
RMSPE_TucFl_linear #1.13507





############### * alidation dataset predictions (Norwest) #########################
Tuc_NWsp$JulianDate <- Tuc_NWsp$Julian
Tuc_NWfl$JulianDate <- Tuc_NWfl$Julian
Tuc_NWsp$AvgDailyTemp <- Tuc_NWsp$DailyMean
Tuc_NWfl$AvgDailyTemp <- Tuc_NWfl$DailyMean


Tuc_NWsp$predictCHAMP<- predict(TucGsp, newdata = Tuc_NWsp)
Tuc_NWfl$predictCHAMP<- predict(TucGfl, newdata = Tuc_NWfl)
Tuc_NWsp$predictLin<- predict(TucLsp, newdata = Tuc_NWsp)
Tuc_NWfl$predictLin<- predict(TucLfl, newdata = Tuc_NWfl)

Tuc_NWfl$predictCHAMP[Tuc_NWfl$predictCHAMP < 0] <- 0
Tuc_NWsp$predictCHAMP[Tuc_NWsp$predictCHAMP < 0] <- 0
Tuc_NWsp$residCHAMP<-(Tuc_NWsp$predictCHAMP-Tuc_NWsp$AvgDailyTemp)
Tuc_NWfl$residCHAMP<-(Tuc_NWfl$predictCHAMP-Tuc_NWfl$AvgDailyTemp)
summary(lm(Tuc_NWsp$AvgDailyTemp~Tuc_NWsp$predictCHAMP))#0.8622, 0.9726
summary(lm(Tuc_NWfl$AvgDailyTemp~Tuc_NWfl$predictCHAMP))#0.9512, 0.9582
Tuc_NWfl$predictLin[Tuc_NWfl$predictLin < 0] <- 0
Tuc_NWsp$predictLin[Tuc_NWsp$predictLin < 0] <- 0
Tuc_NWsp$residLin<-(Tuc_NWsp$predictLin-Tuc_NWsp$AvgDailyTemp)
Tuc_NWfl$residLin<-(Tuc_NWfl$predictLin-Tuc_NWfl$AvgDailyTemp)
summary(lm(Tuc_NWsp$AvgDailyTemp~Tuc_NWsp$predictLin))#0.9077, 0.9697
summary(lm(Tuc_NWfl$AvgDailyTemp~Tuc_NWfl$predictLin))#0.9528, 0.958

Tuc_NWmainstem_COM <- rbind(Tuc_NWsp, Tuc_NWfl)
summary(lm(Tuc_NWmainstem_COM$AvgDailyTemp~Tuc_NWmainstem_COM$predictCHAMP))#0.9855, 0.9604
Tuc_NWfl$resid2<-(Tuc_NWfl$residCHAMP)^2
Tuc_NWfl$residFull2<-(Tuc_NWfl$residFull)^2
Tuc_NWfl$residLin2<-(Tuc_NWfl$residLin)^2
Tuc_NWsp$resid2<-(Tuc_NWsp$residCHAMP)^2
Tuc_NWsp$residLin2<-(Tuc_NWsp$residLin)^2
Tuc_NWmainstem_COM$resid2<-(Tuc_NWmainstem_COM$residCHAMP)^2
Tuc_NWmainstem_COM$residLin2<-(Tuc_NWmainstem_COM$residLin)^2
mean(Tuc_NWmainstem_COM$resid2)^.5 #0.9376285
mean(Tuc_NWmainstem_COM$residLin2)^.5 #0.9575504

plot(AvgDailyTemp~predictCHAMP, pch =".", data= Tuc_NWmainstem_COM)
abline(0,1, col = "red")
plot(residCHAMP~Julian, pch =".", data= Tuc_NWmainstem_COM)
abline(h=0, col = "red")

#### * RMSE by year ####
Tuc_NWsp11 <- subset(Tuc_NWsp, year == 2011)
Tuc_NWsp10 <- subset(Tuc_NWsp, year == 2010)
Tuc_NWsp9 <- subset(Tuc_NWsp, year == 2009)
Tuc_NWsp8 <- subset(Tuc_NWsp, year == 2008)
Tuc_NWsp7 <- subset(Tuc_NWsp, year == 2007)
Tuc_NWsp6 <- subset(Tuc_NWsp, year == 2006)
Tuc_NWsp5 <- subset(Tuc_NWsp, year == 2005)
Tuc_NWsp4 <- subset(Tuc_NWsp, year == 2004)
Tuc_NWsp3 <- subset(Tuc_NWsp, year == 2003)
Tuc_NWsp2 <- subset(Tuc_NWsp, year == 2002)
Tuc_NWsp1 <- subset(Tuc_NWsp, year == 2001)

Tuc_NWfl11 <- subset(Tuc_NWfl, year == 2011)
Tuc_NWfl10 <- subset(Tuc_NWfl, year == 2010)
Tuc_NWfl9 <- subset(Tuc_NWfl, year == 2009)
Tuc_NWfl8 <- subset(Tuc_NWfl, year == 2008)
Tuc_NWfl7 <- subset(Tuc_NWfl, year == 2007)
Tuc_NWfl6 <- subset(Tuc_NWfl, year == 2006)
Tuc_NWfl5 <- subset(Tuc_NWfl, year == 2005)
Tuc_NWfl4 <- subset(Tuc_NWfl, year == 2004)
Tuc_NWfl3 <- subset(Tuc_NWfl, year == 2003)
Tuc_NWfl2 <- subset(Tuc_NWfl, year == 2002)
Tuc_NWfl1 <- subset(Tuc_NWfl, year == 2001)

Tuc_NWmainstem_COM11 <- subset(Tuc_NWmainstem_COM, year == 2011)
Tuc_NWmainstem_COM10 <- subset(Tuc_NWmainstem_COM, year == 2010)
Tuc_NWmainstem_COM9 <- subset(Tuc_NWmainstem_COM, year == 2009)
Tuc_NWmainstem_COM8 <- subset(Tuc_NWmainstem_COM, year == 2008)
Tuc_NWmainstem_COM7 <- subset(Tuc_NWmainstem_COM, year == 2007)
Tuc_NWmainstem_COM6 <- subset(Tuc_NWmainstem_COM, year == 2006)
Tuc_NWmainstem_COM5 <- subset(Tuc_NWmainstem_COM, year == 2005)
Tuc_NWmainstem_COM4 <- subset(Tuc_NWmainstem_COM, year == 2004)
Tuc_NWmainstem_COM3 <- subset(Tuc_NWmainstem_COM, year == 2003)
Tuc_NWmainstem_COM2 <- subset(Tuc_NWmainstem_COM, year == 2002)
Tuc_NWmainstem_COM1 <- subset(Tuc_NWmainstem_COM, year == 2001)

#creating RMSE 
#GAMS
Tuc_RMSE_GAM <- data.frame(matrix(NA, nrow = 12, ncol = 5))
colnames(Tuc_RMSE_GAM)<- c("year","springGAM","fallGAM","CombinedGAM","MonthGAM")
Tuc_RMSE_GAM$year<-c(2001:2011,"All")
Tuc_RMSE_GAM[12,2]<-mean(Tuc_NWsp$resid2)^.5 #1.554385
Tuc_RMSE_GAM[11,2]<-mean(Tuc_NWsp11$resid2)^.5 #1.554385
Tuc_RMSE_GAM[10,2]<-mean(Tuc_NWsp10$resid2)^.5 #1.554385
Tuc_RMSE_GAM[9,2]<-mean(Tuc_NWsp9$resid2)^.5 #1.554385
Tuc_RMSE_GAM[8,2]<-mean(Tuc_NWsp8$resid2)^.5 #1.554385
Tuc_RMSE_GAM[7,2]<-mean(Tuc_NWsp7$resid2)^.5 #1.554385
Tuc_RMSE_GAM[6,2]<-mean(Tuc_NWsp6$resid2)^.5 #1.554385
Tuc_RMSE_GAM[5,2]<-mean(Tuc_NWsp5$resid2)^.5 #1.554385
Tuc_RMSE_GAM[4,2]<-mean(Tuc_NWsp4$resid2)^.5 #1.554385
Tuc_RMSE_GAM[3,2]<-mean(Tuc_NWsp3$resid2)^.5 #1.554385
Tuc_RMSE_GAM[2,2]<-mean(Tuc_NWsp2$resid2)^.5 #1.554385
Tuc_RMSE_GAM[1,2]<-mean(Tuc_NWsp1$resid2)^.5 #1.554385

Tuc_RMSE_GAM[12,3]<-mean(Tuc_NWfl$resid2)^.5 #1.554385
Tuc_RMSE_GAM[11,3]<-mean(Tuc_NWfl11$resid2)^.5 #1.554385
Tuc_RMSE_GAM[10,3]<-mean(Tuc_NWfl10$resid2)^.5 #1.554385
Tuc_RMSE_GAM[9,3]<-mean(Tuc_NWfl9$resid2)^.5 #1.554385
Tuc_RMSE_GAM[8,3]<-mean(Tuc_NWfl8$resid2)^.5 #1.554385
Tuc_RMSE_GAM[7,3]<-mean(Tuc_NWfl7$resid2)^.5 #1.554385
Tuc_RMSE_GAM[6,3]<-mean(Tuc_NWfl6$resid2)^.5 #1.554385
Tuc_RMSE_GAM[5,3]<-mean(Tuc_NWfl5$resid2)^.5 #1.554385
Tuc_RMSE_GAM[4,3]<-mean(Tuc_NWfl4$resid2)^.5 #1.554385
Tuc_RMSE_GAM[3,3]<-mean(Tuc_NWfl3$resid2)^.5 #1.554385
Tuc_RMSE_GAM[2,3]<-mean(Tuc_NWfl2$resid2)^.5 #1.554385
Tuc_RMSE_GAM[1,3]<-mean(Tuc_NWfl1$resid2)^.5 #1.554385

Tuc_RMSE_GAM[12,4]<-mean(Tuc_NWmainstem_COM$resid2)^.5 #1.554385
Tuc_RMSE_GAM[11,4]<-mean(Tuc_NWmainstem_COM11$resid2)^.5 #1.554385
Tuc_RMSE_GAM[10,4]<-mean(Tuc_NWmainstem_COM10$resid2)^.5 #1.554385
Tuc_RMSE_GAM[9,4]<-mean(Tuc_NWmainstem_COM9$resid2)^.5 #1.554385
Tuc_RMSE_GAM[8,4]<-mean(Tuc_NWmainstem_COM8$resid2)^.5 #1.554385
Tuc_RMSE_GAM[7,4]<-mean(Tuc_NWmainstem_COM7$resid2)^.5 #1.554385
Tuc_RMSE_GAM[6,4]<-mean(Tuc_NWmainstem_COM6$resid2)^.5 #1.554385
Tuc_RMSE_GAM[5,4]<-mean(Tuc_NWmainstem_COM5$resid2)^.5 #1.554385
Tuc_RMSE_GAM[4,4]<-mean(Tuc_NWmainstem_COM4$resid2)^.5 #1.554385
Tuc_RMSE_GAM[3,4]<-mean(Tuc_NWmainstem_COM3$resid2)^.5 #1.554385
Tuc_RMSE_GAM[2,4]<-mean(Tuc_NWmainstem_COM2$resid2)^.5 #1.554385
Tuc_RMSE_GAM[1,4]<-mean(Tuc_NWmainstem_COM1$resid2)^.5 #1.554385

#Linear
Tuc_RMSE_Lin <- data.frame(matrix(NA, nrow = 12, ncol = 5))
colnames(Tuc_RMSE_Lin)<- c("year","springLin","fallLin","CombinedLin","MonthLin")
Tuc_RMSE_Lin$year<-c(2001:2011,"All")
Tuc_RMSE_Lin[12,2]<-mean(Tuc_NWsp$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[11,2]<-mean(Tuc_NWsp11$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[10,2]<-mean(Tuc_NWsp10$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[9,2]<-mean(Tuc_NWsp9$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[8,2]<-mean(Tuc_NWsp8$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[7,2]<-mean(Tuc_NWsp7$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[6,2]<-mean(Tuc_NWsp6$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[5,2]<-mean(Tuc_NWsp5$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[4,2]<-mean(Tuc_NWsp4$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[3,2]<-mean(Tuc_NWsp3$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[2,2]<-mean(Tuc_NWsp2$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[1,2]<-mean(Tuc_NWsp1$residLin2)^.5 #1.554385

Tuc_RMSE_Lin[12,3]<-mean(Tuc_NWfl$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[11,3]<-mean(Tuc_NWfl11$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[10,3]<-mean(Tuc_NWfl10$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[9,3]<-mean(Tuc_NWfl9$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[8,3]<-mean(Tuc_NWfl8$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[7,3]<-mean(Tuc_NWfl7$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[6,3]<-mean(Tuc_NWfl6$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[5,3]<-mean(Tuc_NWfl5$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[4,3]<-mean(Tuc_NWfl4$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[3,3]<-mean(Tuc_NWfl3$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[2,3]<-mean(Tuc_NWfl2$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[1,3]<-mean(Tuc_NWfl1$residLin2)^.5 #1.554385

Tuc_RMSE_Lin[12,4]<-mean(Tuc_NWmainstem_COM$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[11,4]<-mean(Tuc_NWmainstem_COM11$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[10,4]<-mean(Tuc_NWmainstem_COM10$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[9,4]<-mean(Tuc_NWmainstem_COM9$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[8,4]<-mean(Tuc_NWmainstem_COM8$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[7,4]<-mean(Tuc_NWmainstem_COM7$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[6,4]<-mean(Tuc_NWmainstem_COM6$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[5,4]<-mean(Tuc_NWmainstem_COM5$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[4,4]<-mean(Tuc_NWmainstem_COM4$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[3,4]<-mean(Tuc_NWmainstem_COM3$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[2,4]<-mean(Tuc_NWmainstem_COM2$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[1,4]<-mean(Tuc_NWmainstem_COM1$residLin2)^.5 #1.554385


library(lattice)
win.graph(width =6, height = 7)
par(cex=.9, ps=10)
xyplot(AvgDailyTemp~predictCHAMP|factor(year), data = Tuc_NWmainstem_COM, pch = ".", col = "black",
       layout=c(3,4),panel=function(...){panel.xyplot(...)
         panel.abline(0,1, col = "red")
       },
       ylab = "Tw (°C)", xlab = "predicted Tw (°C)")



# * RMSE by month #### 
#PREDICTING MONTHLY AVERAGES
Tuc_NWmainstem_COM$year_month_site <- paste(Tuc_NWmainstem_COM$year,"_",Tuc_NWmainstem_COM$month, "_",Tuc_NWmainstem_COM$PERMA_FID.x,sep = "")
month.avgTUC <- as.data.frame.table(tapply(Tuc_NWmainstem_COM$AvgDailyTemp, Tuc_NWmainstem_COM$year_month_site,  mean))
colnames(month.avgTUC)<- c("year_month_site","AvgMonthTemp")
month.avgTUC$SiteName<- sub(".*_", "", month.avgTUC$year_month_site)
head(month.avgTUC)
PERMA_FID.xNW <- as.data.frame.table(tapply(Tuc_NWmainstem_COM$PERMA_FID.x, Tuc_NWmainstem_COM$year_month_site,  mean))
monthNW <- as.data.frame.table(tapply(Tuc_NWmainstem_COM$month, Tuc_NWmainstem_COM$year_month_site,  mean))
yearNW <- as.data.frame.table(tapply(Tuc_NWmainstem_COM$year, Tuc_NWmainstem_COM$year_month_site,  mean))
month.avgTUC$PERMA_FID <- PERMA_FID.xNW$Freq
month.avgTUC$year <- yearNW$Freq
month.avgTUC$month <- monthNW$Freq
month.avgNW.CHAMP <- as.data.frame.table(tapply(Tuc_NWmainstem_COM$predictCHAMP, Tuc_NWmainstem_COM$year_month_site,  mean))
month.avgTUC$predCHAMP <- month.avgNW.CHAMP$Freq
month.avgNW.Lin<- as.data.frame.table(tapply(Tuc_NWmainstem_COM$predictLin, Tuc_NWmainstem_COM$year_month_site,  mean))
month.avgTUC$predLin <- month.avgNW.Lin$Freq
head(month.avgTUC)
month.avgTUC.mod <-lm(AvgMonthTemp~predCHAMP, month.avgTUC)
month.avgTUC.modLin <-lm(AvgMonthTemp~predLin, month.avgTUC)
summary(month.avgTUC.mod) #0.644, 0.9816
summary(month.avgTUC.modLin) #0.6483, 0.9813
month.avgTUC$resid <-(month.avgTUC$AvgMonthTemp)-(month.avgTUC$predCHAMP)
month.avgTUC$resid2<-(month.avgTUC$resid)^2
month.avgTUC$residLin <-(month.avgTUC$AvgMonthTemp)-(month.avgTUC$predLin)
month.avgTUC$residLin2<-(month.avgTUC$residLin)^2
mean(month.avgTUC$resid2)^.5 #0.6850205
mean(month.avgTUC$residLin2)^.5 #0.6972084

month.avgTUC11 <- subset(month.avgTUC, year == 2011)
month.avgTUC10 <- subset(month.avgTUC, year == 2010)
month.avgTUC9 <- subset(month.avgTUC, year == 2009)
month.avgTUC8 <- subset(month.avgTUC, year == 2008)
month.avgTUC7 <- subset(month.avgTUC, year == 2007)
month.avgTUC6 <- subset(month.avgTUC, year == 2006)
month.avgTUC5 <- subset(month.avgTUC, year == 2005)
month.avgTUC4 <- subset(month.avgTUC, year == 2004)
month.avgTUC3 <- subset(month.avgTUC, year == 2003)
month.avgTUC2 <- subset(month.avgTUC, year == 2002)
month.avgTUC1 <- subset(month.avgTUC, year == 2001)

Tuc_RMSE_GAM[12,5]<-mean(month.avgTUC$resid2)^.5 #1.554385
Tuc_RMSE_GAM[11,5]<-mean(month.avgTUC11$resid2)^.5 #1.554385
Tuc_RMSE_GAM[10,5]<-mean(month.avgTUC10$resid2)^.5 #1.554385
Tuc_RMSE_GAM[9,5]<-mean(month.avgTUC9$resid2)^.5 #1.554385
Tuc_RMSE_GAM[8,5]<-mean(month.avgTUC8$resid2)^.5 #1.554385
Tuc_RMSE_GAM[7,5]<-mean(month.avgTUC7$resid2)^.5 #1.554385
Tuc_RMSE_GAM[6,5]<-mean(month.avgTUC6$resid2)^.5 #1.554385
Tuc_RMSE_GAM[5,5]<-mean(month.avgTUC5$resid2)^.5 #1.554385
Tuc_RMSE_GAM[4,5]<-mean(month.avgTUC4$resid2)^.5 #1.554385
Tuc_RMSE_GAM[3,5]<-mean(month.avgTUC3$resid2)^.5 #1.554385
Tuc_RMSE_GAM[2,5]<-mean(month.avgTUC2$resid2)^.5 #1.554385
Tuc_RMSE_GAM[1,5]<-mean(month.avgTUC1$resid2)^.5 #1.554385

Tuc_RMSE_Lin[12,5]<-mean(month.avgTUC$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[11,5]<-mean(month.avgTUC11$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[10,5]<-mean(month.avgTUC10$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[9,5]<-mean(month.avgTUC9$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[8,5]<-mean(month.avgTUC8$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[7,5]<-mean(month.avgTUC7$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[6,5]<-mean(month.avgTUC6$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[5,5]<-mean(month.avgTUC5$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[4,5]<-mean(month.avgTUC4$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[3,5]<-mean(month.avgTUC3$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[2,5]<-mean(month.avgTUC2$residLin2)^.5 #1.554385
Tuc_RMSE_Lin[1,5]<-mean(month.avgTUC1$residLin2)^.5 #1.554385

plot(AvgMonthTemp~predCHAMP, month.avgTUC, pch= 16)
abline(0,1, col ="red")
plot(AvgMonthTemp~predLin, month.avgTUC, pch= 16)
abline(0,1, col ="red")

#RSE     R2
mean(month.avgTUC$resid2)^.5 #0.684958
mean(month.avgTUC$residLin2)^.5 #0.6972084

#
month.avgTUC_01 <- subset(month.avgTUC, month == 1)
month.avgTUC_02 <- subset(month.avgTUC, month == 2)
month.avgTUC_03 <- subset(month.avgTUC, month == 3)
month.avgTUC_04 <- subset(month.avgTUC, month == 4)
month.avgTUC_05 <- subset(month.avgTUC, month == 5)
month.avgTUC_06 <- subset(month.avgTUC, month == 6)
month.avgTUC_07 <- subset(month.avgTUC, month == 7)
month.avgTUC_08 <- subset(month.avgTUC, month == 8)
month.avgTUC_09 <- subset(month.avgTUC, month == 9)
month.avgTUC_10 <- subset(month.avgTUC, month == 10)
month.avgTUC_11 <- subset(month.avgTUC, month == 11)
month.avgTUC_12 <- subset(month.avgTUC, month == 12)

#### * Finish RMSE by monthXmonth table ####
#RMSE AND NSC for Tucannon predictions by month
Monthly_Table[1,14]<-mean(month.avgTUC_01$resid2)^.5 #1.000373
Monthly_Table[2,14]<-mean(month.avgTUC_02$resid2)^.5 #1.000373
Monthly_Table[3,14]<-mean(month.avgTUC_03$resid2)^.5 #1.000373
Monthly_Table[4,14]<-mean(month.avgTUC_04$resid2)^.5 #1.000373
Monthly_Table[5,14]<-mean(month.avgTUC_05$resid2)^.5 #1.000373
Monthly_Table[6,14]<-mean(month.avgTUC_06$resid2)^.5 #1.000373
Monthly_Table[7,14]<-mean(month.avgTUC_07$resid2)^.5 #1.000373
Monthly_Table[8,14]<-mean(month.avgTUC_08$resid2)^.5 #1.000373
Monthly_Table[9,14]<-mean(month.avgTUC_09$resid2)^.5 #1.000373
Monthly_Table[10,14]<-mean(month.avgTUC_10$resid2)^.5 #1.000373
Monthly_Table[11,14]<-mean(month.avgTUC_11$resid2)^.5 #1.000373
Monthly_Table[12,14]<-mean(month.avgTUC_12$resid2)^.5 #1.000373
Monthly_Table[13,14]<-mean(month.avgTUC$resid2)^.5 #1.000373

Monthly_Table[13,15]<- 1 - ((sum((month.avgTUC$resid)^2))/(sum((month.avgTUC$AvgMonthTemp-(mean(month.avgTUC$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,15]<- 1 - ((sum((month.avgTUC_12$resid)^2))/(sum((month.avgTUC_12$AvgMonthTemp-(mean(month.avgTUC_12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,15]<- 1 - ((sum((month.avgTUC_11$resid)^2))/(sum((month.avgTUC_11$AvgMonthTemp-(mean(month.avgTUC_11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,15]<- 1 - ((sum((month.avgTUC_10$resid)^2))/(sum((month.avgTUC_10$AvgMonthTemp-(mean(month.avgTUC_10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,15]<- 1 - ((sum((month.avgTUC_09$resid)^2))/(sum((month.avgTUC_09$AvgMonthTemp-(mean(month.avgTUC_09$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,15]<- 1 - ((sum((month.avgTUC_08$resid)^2))/(sum((month.avgTUC_08$AvgMonthTemp-(mean(month.avgTUC_08$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,15]<- 1 - ((sum((month.avgTUC_07$resid)^2))/(sum((month.avgTUC_07$AvgMonthTemp-(mean(month.avgTUC_07$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,15]<- 1 - ((sum((month.avgTUC_06$resid)^2))/(sum((month.avgTUC_06$AvgMonthTemp-(mean(month.avgTUC_06$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,15]<- 1 - ((sum((month.avgTUC_05$resid)^2))/(sum((month.avgTUC_05$AvgMonthTemp-(mean(month.avgTUC_05$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,15]<- 1 - ((sum((month.avgTUC_04$resid)^2))/(sum((month.avgTUC_04$AvgMonthTemp-(mean(month.avgTUC_04$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,15]<- 1 - ((sum((month.avgTUC_03$resid)^2))/(sum((month.avgTUC_03$AvgMonthTemp-(mean(month.avgTUC_03$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,15]<- 1 - ((sum((month.avgTUC_02$resid)^2))/(sum((month.avgTUC_02$AvgMonthTemp-(mean(month.avgTUC_02$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,15]<- 1 - ((sum((month.avgTUC_01$resid)^2))/(sum((month.avgTUC_01$AvgMonthTemp-(mean(month.avgTUC_01$AvgMonthTemp)))^2))) #0.402458

Monthly_Table[1,16]<-mean(month.avgTUC_01$residLin2)^.5 #1.000373
Monthly_Table[2,16]<-mean(month.avgTUC_02$residLin2)^.5 #1.000373
Monthly_Table[3,16]<-mean(month.avgTUC_03$residLin2)^.5 #1.000373
Monthly_Table[4,16]<-mean(month.avgTUC_04$residLin2)^.5 #1.000373
Monthly_Table[5,16]<-mean(month.avgTUC_05$residLin2)^.5 #1.000373
Monthly_Table[6,16]<-mean(month.avgTUC_06$residLin2)^.5 #1.000373
Monthly_Table[7,16]<-mean(month.avgTUC_07$residLin2)^.5 #1.000373
Monthly_Table[8,16]<-mean(month.avgTUC_08$residLin2)^.5 #1.000373
Monthly_Table[9,16]<-mean(month.avgTUC_09$residLin2)^.5 #1.000373
Monthly_Table[10,16]<-mean(month.avgTUC_10$residLin2)^.5 #1.000373
Monthly_Table[11,16]<-mean(month.avgTUC_11$residLin2)^.5 #1.000373
Monthly_Table[12,16]<-mean(month.avgTUC_12$residLin2)^.5 #1.000373
Monthly_Table[13,16]<-mean(month.avgTUC$residLin2)^.5 #1.000373

Monthly_Table[13,17]<- 1 - ((sum((month.avgTUC$residLin)^2))/(sum((month.avgTUC$AvgMonthTemp-(mean(month.avgTUC$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,17]<- 1 - ((sum((month.avgTUC_12$residLin)^2))/(sum((month.avgTUC_12$AvgMonthTemp-(mean(month.avgTUC_12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,17]<- 1 - ((sum((month.avgTUC_11$residLin)^2))/(sum((month.avgTUC_11$AvgMonthTemp-(mean(month.avgTUC_11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,17]<- 1 - ((sum((month.avgTUC_10$residLin)^2))/(sum((month.avgTUC_10$AvgMonthTemp-(mean(month.avgTUC_10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,17]<- 1 - ((sum((month.avgTUC_09$residLin)^2))/(sum((month.avgTUC_09$AvgMonthTemp-(mean(month.avgTUC_09$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,17]<- 1 - ((sum((month.avgTUC_08$residLin)^2))/(sum((month.avgTUC_08$AvgMonthTemp-(mean(month.avgTUC_08$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,17]<- 1 - ((sum((month.avgTUC_07$residLin)^2))/(sum((month.avgTUC_07$AvgMonthTemp-(mean(month.avgTUC_07$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,17]<- 1 - ((sum((month.avgTUC_06$residLin)^2))/(sum((month.avgTUC_06$AvgMonthTemp-(mean(month.avgTUC_06$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,17]<- 1 - ((sum((month.avgTUC_05$residLin)^2))/(sum((month.avgTUC_05$AvgMonthTemp-(mean(month.avgTUC_05$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,17]<- 1 - ((sum((month.avgTUC_04$residLin)^2))/(sum((month.avgTUC_04$AvgMonthTemp-(mean(month.avgTUC_04$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,17]<- 1 - ((sum((month.avgTUC_03$residLin)^2))/(sum((month.avgTUC_03$AvgMonthTemp-(mean(month.avgTUC_03$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,17]<- 1 - ((sum((month.avgTUC_02$residLin)^2))/(sum((month.avgTUC_02$AvgMonthTemp-(mean(month.avgTUC_02$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,17]<- 1 - ((sum((month.avgTUC_01$residLin)^2))/(sum((month.avgTUC_01$AvgMonthTemp-(mean(month.avgTUC_01$AvgMonthTemp)))^2))) #0.402458



with(month.avgTUC, tapply(month, SiteName, FUN = function(x) length(unique(x))))
with(month.avgTUC, tapply(year, SiteName, FUN = function(x) length(unique(x))))
with(month.avgTUC, tapply(AvgMonthTemp, SiteName, FUN = function(x) length(unique(x))))

library(dplyr)
monthsites <- month.avgTUC %>% count(SiteName,sort = TRUE)
max(month.avgTUC$resid)
min(month.avgTUC$resid)
mean(month.avgTUC$resid)

#### * Some graphs ####
win.graph(width = 12, height = 8)
par(mfrow= c(3,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==1 & SiteName == "21094"), ylim =c (1,6), xlim = c(1,6), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==1 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==1 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==1 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==2 & SiteName == "21094"), ylim =c (2,7), xlim = c(2,7), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==2 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==2 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==2 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==3 & SiteName == "21094"), ylim =c (2,9), xlim = c(2,9), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==3 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==3 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==3 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==4 & SiteName == "21094"), ylim =c (4,10), xlim = c(4,10), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==4 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==4 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==4 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==5 & SiteName == "21094"), ylim =c (4,12), xlim = c(4,12), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==5 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==5 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==5 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==6 & SiteName == "21094"), ylim =c (6,15), xlim = c(6,15), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==6 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==6 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==6 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==7 & SiteName == "21094"), ylim =c (9,18), xlim = c(9,18), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==7 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==7 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==7 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==8 & SiteName == "21094"), ylim =c (10,19), xlim = c(10,19), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==8 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==8 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==8 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==9 & SiteName == "21094"), ylim =c (8,15), xlim = c(8,15), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==9 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==9 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==9 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==10 & SiteName == "21094"), ylim =c (5,12), xlim = c(5,12), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==10 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==10 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==10 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==11 & SiteName == "21094"), ylim =c (3,8), xlim = c(3,8), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==11 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==11 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==11 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==12 & SiteName == "21094"),ylim =c (1,6), xlim = c(1,6), col = "red", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==12 & SiteName == "10177"), col = "blue", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==12 & SiteName == "10184"), col = "green", pch = 16)
points(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==12 & SiteName == "169"), col = "purple", pch = 16)
abline(0,1)

plot(AvgMonthTemp~predCHAMP,data= month.avgTUC, col = "green", pch = 16)
abline(0,1)



win.graph(width = 9, height = 12)
par(mfrow= c(4,3), mar=c(2,2,1,1), cex=.8, ps=10)
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2001), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2003")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2002), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2003")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2003), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2003")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2004), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2004")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2005), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2005")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2006), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2006")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2007), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2007")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2008), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2008")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2009), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2009")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2010), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2010")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictCHAMP,data= subset(Tuc_NWmainstem_COM, year==2011), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2011")
abline(0,1, col = "red", pch=".")

win.graph(width = 12, height = 8)
par(mfrow= c(3,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==1), ylim =c(0,22), xlim = c (0,22), main = "January")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==2), ylim =c(0,22), xlim = c (0,22), main = "February")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==3), ylim =c(0,22), xlim = c (0,22), main = "March")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==4), ylim =c(0,22), xlim = c (0,22), main = "April")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==5), ylim =c(0,22), xlim = c (0,22), main = "May")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==6), ylim =c(0,22), xlim = c (0,22), main = "June")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==7), ylim =c(0,22), xlim = c (0,22), main = "July")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==8), ylim =c(0,22), xlim = c (0,22), main = "August")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==9), ylim =c(0,22), xlim = c (0,22), main = "September")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==10), ylim =c(0,22), xlim = c (0,22), main = "October")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==11), ylim =c(0,22), xlim = c (0,22),main = "November")
abline(0,1, col = "red")
plot(AvgMonthTemp~predCHAMP,data= subset(month.avgTUC, month==12),ylim =c(0,22), xlim = c(0,22), main = "December")
abline(0,1, col = "red")




#### ............... ####
#### M. F. JOHN DAY MODEL ########################################################################################

####### * Loading data ####
setwd("C:/Users/deraj/Documents/South Fork/Temperature models/Manuscript_clean/")
library(mgcv)
data.NW <- read.csv("MFJD_data.dayNW.csv")
data.CH <- read.csv("MFJD_data.dayCH.csv")

data.CH <- data.CH[order(data.CH$JulianDate),]
data.NW <- data.NW[order(data.NW$Julian),]
colnames(data.CH)


#### * Data cleaning ####
###Remove sites with clear spring water influence and anomolous data from CHAMP data...
BadSitesCH<-c("CBW05583-298738","CBW05583-282354","CBW05583-418546")
plot(AvgDailyTemp~JulianDate, data =subset(data.CH, (SiteName %in% BadSitesCH)), col="red", pch=".")
points(AvgDailyTemp~JulianDate, data =subset(data.CH, !(SiteName %in% BadSitesCH)), col="blue", pch=".")

data.CH.sub <- subset(data.CH, !(SiteName %in% BadSitesCH))

#Spring sites NorWest DATA
BadSitesNW <-c("19566","19565","1979","7341","19599","19631","17998","19613", "7322")
plot(DailyMean~Julian, data =subset(data.NW, (PERMA_FID.x %in% BadSitesNW)), col="red", pch=".")
points(DailyMean~Julian, data =subset(data.NW, !(PERMA_FID.x %in% BadSitesNW)), col="blue", pch=".")
#Remove downstream sites outside  of predicted area
DownstreamSites <-c("2214","4202","4204","4227","4229","5290","5293","7339", "19564","21152")
DownstreamSites <-c("1976","2211","2257","5285","5287","2214","4202","4204","4227","4229","5290","5293","7339", "19564","21152")

data.sub.NW <- subset(data.NW, !(PERMA_FID.x %in% BadSitesNW))
data.sub.NW <- subset(data.sub.NW, !(PERMA_FID.x %in% DownstreamSites))

#### * Split fall and spring datasets
#smoother for each dataset identifying day of max temperature for spring/fall division
data.CH.sub$predSmooth <- predict(gam(AvgDailyTemp~s(JulianDate, k=10), data = data.CH.sub))
data.sub.NW$predSmooth <- predict(gam(DailyMean~s(Julian, k=10), data = data.sub.NW))
plot(data.CH.sub$JulianDate,data.CH.sub$predSmooth, pch = 16, col = "red", ylab = "Julian date", xlab = "stream temperature")
points(data.sub.NW$Julian,data.sub.NW$predSmooth, pch = 16, col = "blue")

max.temp.CH <- as.data.frame.table(tapply(data.CH.sub$predSmooth,data.CH.sub$JulianDate, mean))
max.temp.NW <- as.data.frame.table(tapply(data.sub.NW$predSmooth,data.sub.NW$Julian, mean))
max(max.temp.CH$Freq)#day 198 max of CHAMP data
max(max.temp.NW$Freq)#day 209 max of NW data

library(plyr)

#unique site
length(unique(data.CH.sub$SiteName)) #of sites champ
length(unique(data.sub.NW$PERMA_FID.x)) #of sites NW
ChampSitesUsedMFJD<- as.data.frame(unique(data.CH.sub$SiteName))
write.csv(ChampSitesUsedMFJD, "ChampSitesUsedMFJD.csv")

getwd()
data.sub.NW.sp<- subset(data.sub.NW, Julian< 198)
data.sub.NW.fl<- subset(data.sub.NW, Julian > 199)
data.CH.sub.sp<- subset(data.CH.sub, JulianDate< 198)
data.CH.sub.fl<- subset(data.CH.sub, JulianDate > 199)
data.sub.NW.sp$JulianDate <- data.sub.NW.sp$Julian
data.sub.NW.fl$JulianDate <- data.sub.NW.fl$Julian
data.sub.NW.sp$AvgDailyTemp <- data.sub.NW.sp$DailyMean
data.sub.NW.fl$AvgDailyTemp <- data.sub.NW.fl$DailyMean






#### * Fit models and metrics ####
# GAM MODELS
MFJDgsCH <- (gam(AvgDailyTemp ~ s(TAVGn5d, k = 4) +s(Tchange5, k = 3)+s(SNWD,by = TAVGn5d, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(GRADIENT, by = TAVGn5d, k = 3)+ s(BFI, by = TAVGn5d, k = 3)+ s(dailymeanCMS, by = TAVGn5d, k = 3) + s(JulianDate, by = TAVGn5d, k = 5)+ s(Echange2, by = TAVGn5d, k = 3)+ s(TreeDens_1, by = TAVGn5d, k = 3), data = data.CH.sub.sp))#+s(TreeDens_1, by = TAVGn5d, k = 3)
MFJDgfCH <- (gam(AvgDailyTemp ~ s(TAVGn3d, k = 4) +s(Tchange3, k = 3)+s(SNWD,by = TAVGn3d, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(catch_area, by = TAVGn3d, k = 3)+s(ae, by = SNWD, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3d, k = 3)+ s(Echange2, by = TAVGn3d, k = 3)+ s(TreeDens_1, by = TAVGn3d, k = 3)+s(GRADIENT, by = TAVGn3d, k = 3), data = data.CH.sub.fl))#+ s(TreeDens_1, by = TAVGn3d, k = 3)
#linear
MFJDgsCHLin <- (lm(AvgDailyTemp ~ TAVGn5d+Tchange5+SNWD*TAVGn5d+A1Snow*JulianDate+catch_area*dailymeanCMS+ae*JulianDate+GRADIENT*TAVGn5d+BFI*TAVGn5d+dailymeanCMS*TAVGn5d+JulianDate*TAVGn5d+Echange2*TAVGn5d, data = data.CH.sub.sp))
MFJDgfCHLin <- (lm(AvgDailyTemp ~TAVGn3d+Tchange3+SNWD*TAVGn3d+dailymeanCMS*TAVGn3d+catch_area*TAVGn3d+ae*SNWD+ae*JulianDate+Echange2*TAVGn3d+TreeDens_1*TAVGn3d+GRADIENT*TAVGn3d, data = data.CH.sub.fl))

summary(MFJDgsCH)#0.957,1.2458
summary(MFJDgfCH)#0.936,2.1187
summary(MFJDgsCHLin)#0.945,1.268
summary(MFJDgfCHLin)#0.9298,1.522

data.CH.sub.sp$predictCHAMP  <- predict(MFJDgsCH)
data.CH.sub.fl$predictCHAMP  <- predict(MFJDgfCH)
data.CH.sub.sp$residCHAMP  <- resid(MFJDgsCH)
data.CH.sub.fl$residCHAMP  <- resid(MFJDgfCH)
data.CH.sub.sp$residCHAMP2  <- (data.CH.sub.sp$predictCHAMP-data.CH.sub.sp$AvgDailyTemp)
data.CH.sub.fl$residCHAMP2  <- (data.CH.sub.fl$predictCHAMP-data.CH.sub.fl$AvgDailyTemp)

data.CH.sub.sp$resid<-resid(MFJDgsCH)
data.CH.sub.sp$residLin<-resid(MFJDgsCHLin)
data.CH.sub.sp$resid2<-(data.CH.sub.sp$resid)^2
data.CH.sub.sp$residLin2<-(data.CH.sub.sp$residLin)^2
mean(data.CH.sub.sp$resid2)^.5 #1.114085 #RMSE
mean(data.CH.sub.sp$residLin2)^.5 #1.267009 #RMSE

data.CH.sub.fl$resid<-resid(MFJDgfCH)
data.CH.sub.fl$residLin<-resid(MFJDgfCHLin)
data.CH.sub.fl$resid2<-(data.CH.sub.fl$resid)^2
data.CH.sub.fl$residLin2<-(data.CH.sub.fl$residLin)^2
(mean(data.CH.sub.fl$resid2))^.5 #1.451663 #RMSE
(mean(data.CH.sub.fl$residLin2))^.5 #1.521216 #RMSE



#### * Relationship explorations ####
library(visreg)
#Checking relationships
visreg(MFJDgsCH, "TAVGn5d",nn=12)
visreg2d(MFJDgsCH, "TAVGn5d", "Tchange5", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "SNWD", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "A1Snow", "JulianDate", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "JulianDate", "TAVGn5d", plot.type="persp",nn=12) 
visreg2d(MFJDgsCH, "dailymeanCMS", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "ae", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "JulianDate", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "Echange2", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "TreeDens_1", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "GRADIENT", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCH, "BFI", "TAVGn5d", plot.type="persp",nn=12)  

visreg(MFJDgfCH, "TAVGn3d",nn=12)
visreg2d(MFJDgfCH, "TAVGn3d", "Tchange3", plot.type="persp",nn=12) 
visreg2d(MFJDgfCH, "SNWD", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "JulianDate", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "catch_area", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "ae", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "JulianDate", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "Echange2", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "SNWD", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "TreeDens_1", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "GRADIENT", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCH, "treedensit", "TAVGn3d", plot.type="persp",nn=12)  

#Checking relationships Linear
visreg(MFJDgsCHLin, "TAVGn5d")
visreg2d(MFJDgsCHLin, "TAVGn5d", "Tchange5", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "SNWD", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "A1Snow", "JulianDate", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "JulianDate", "TAVGn5d", plot.type="persp",nn=12) 
visreg2d(MFJDgsCHLin, "dailymeanCMS", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "catch_area", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "ae", "TAVGn5d", plot.type="persp",nn=12) 
visreg2d(MFJDgsCHLin, "JulianDate", "ae", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "Echange2", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "GRADIENT", "TAVGn5d", plot.type="persp",nn=12)  
visreg2d(MFJDgsCHLin, "BFI", "TAVGn5d", plot.type="persp",nn=12) 

visreg(MFJDgfCHLin, "TAVGn3d",nn=12)
visreg2d(MFJDgfCHLin, "TAVGn3d", "Tchange3", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "SNWD", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "JulianDate", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "catch_area", "TAVGn3d", plot.type="persp",nn=12) 
visreg2d(MFJDgfCHLin, "catch_area", "dailymeanCMS", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "ae", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "JulianDate", "ae", plot.type="persp",nn=12) 
visreg2d(MFJDgfCHLin, "ae", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "Echange2", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "TreeDens_1", "TAVGn3d", plot.type="persp",nn=12)  
visreg2d(MFJDgfCHLin, "GRADIENT", "TAVGn3d", plot.type="persp",nn=12)  


######### * LOOCV selected #########################################
####LOOCV spring MFJD GAM  
resid_list <- matrix(ncol = 2, nrow = 0)
data.CH.sub.sp$SiteName <- factor(data.CH.sub.sp$SiteName)
Sites_springMFJD <- unique(data.CH.sub.sp$SiteName)
for(i in Sites_springMFJD) {
  MFJD_Champsp_Ni <- subset(data.CH.sub.sp, SiteName!=i)
  MFJD_Champsp_i <- subset(data.CH.sub.sp, SiteName == i)
  mdl <- gam(AvgDailyTemp ~ s(TAVGn5d, k = 4) +s(Tchange5, k = 3)+s(SNWD,by = TAVGn5d, k = 3)+s(A1Snow,by = JulianDate, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(ae, by = JulianDate, k = 3)+s(GRADIENT, by = TAVGn5d, k = 3)+ s(BFI, by = TAVGn5d, k = 3)+ s(dailymeanCMS, by = TAVGn5d, k = 3) + s(JulianDate, by = TAVGn5d, k = 5)+ s(Echange2, by = TAVGn5d, k = 3)+ s(TreeDens_1, by = TAVGn5d, k = 3), data = MFJD_Champsp_Ni) # leave i'th site out and refit model
  MFJD_Champsp_i$y_pred <- predict(mdl, newdata = MFJD_Champsp_i) #predict site data without site info
  MFJD_Champsp_i$y_pred[MFJD_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(MFJD_Champsp_i$AvgDailyTemp  - MFJD_Champsp_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_MFJDSp_GAM  <- (mean(resid_list$resid^2))^.5 #1.300196
tapply(abs(resid_list$resid),resid_list$site,mean)

####LOOCV fall MFJD GAM  
resid_list <- vector(mode="numeric", length=0)
data.CH.sub.fl$SiteName <- factor(data.CH.sub.fl$SiteName)
Sites_fallMFJD <- unique(data.CH.sub.fl$SiteName)
for(i in Sites_fallMFJD) {
  MFJD_Champfl_Ni <- subset(data.CH.sub.fl, SiteName!=i)
  MFJD_Champfl_i <- subset(data.CH.sub.fl, SiteName == i)
  mdl <- gam(AvgDailyTemp ~ s(TAVGn3d, k = 4) +s(Tchange3, k = 3)+s(SNWD,by = TAVGn3d, k = 3)+s(catch_area, by = ae, k = 3)+s(catch_area, by = dailymeanCMS, k = 3)+s(catch_area, by = TAVGn3d, k = 3)+s(ae, by = SNWD, k = 3)+s(ae, by = JulianDate, k = 3)+ s(dailymeanCMS, by = TAVGn3d, k = 3)+ s(Echange2, by = TAVGn3d, k = 3)+ s(TreeDens_1, by = TAVGn3d, k = 3)+s(GRADIENT, by = TAVGn3d, k = 3), data = MFJD_Champfl_Ni)# leave i'th site out and refit model
  MFJD_Champfl_i$y_pred <- predict(mdl, newdata = MFJD_Champfl_i) #predict site data without site info
  MFJD_Champfl_i$y_pred[MFJD_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(MFJD_Champfl_i$AvgDailyTemp  - MFJD_Champfl_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_MFJDFl_GAM  <- (mean(resid_list$resid^2))^.5 #1.560673
tapply(abs(resid_list$resid),resid_list$site,mean)


####LOOCV spring MFJDLinear  
resid_list <- vector(mode="numeric", length=0)
data.CH.sub.sp$SiteName <- factor(data.CH.sub.sp$SiteName)
Sites_springMFJD <- unique(data.CH.sub.sp$SiteName)
for(i in Sites_springMFJD) {
  MFJD_Champsp_Ni <- subset(data.CH.sub.sp, SiteName!=i)
  MFJD_Champsp_i <- subset(data.CH.sub.sp, SiteName == i)
  mdl <- lm(AvgDailyTemp ~ TAVGn5d+Tchange5+SNWD*TAVGn5d+A1Snow*JulianDate+catch_area*dailymeanCMS+ae*JulianDate+GRADIENT*TAVGn5d+BFI*TAVGn5d+dailymeanCMS*TAVGn5d+JulianDate*TAVGn5d+Echange2*TAVGn5d, data = MFJD_Champsp_Ni) # leave i'th site out and refit model
  MFJD_Champsp_i$y_pred <- predict(mdl, newdata = MFJD_Champsp_i) #predict site data without site info
  MFJD_Champsp_i$y_pred[MFJD_Champsp_i$y_pred < 0] <- 0
  resid<- as.data.frame(MFJD_Champsp_i$AvgDailyTemp  - MFJD_Champsp_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_MFJDSp_linear  <- (mean(resid_list$resid^2))^.5 #1.443828
tapply(abs(resid_list$resid),resid_list$site,mean)

####LOOCV fall MFJDannon Linear  
resid_list <- vector(mode="numeric", length=0)
data.CH.sub.fl$SiteName <- factor(data.CH.sub.fl$SiteName)
Sites_fallMFJD <- unique(data.CH.sub.fl$SiteName)
for(i in Sites_fallMFJD) {
  MFJD_Champfl_Ni <- subset(data.CH.sub.fl, SiteName!=i)
  MFJD_Champfl_i <- subset(data.CH.sub.fl, SiteName == i)
  mdl <- lm(AvgDailyTemp ~TAVGn3d+Tchange3+SNWD*TAVGn3d+dailymeanCMS*TAVGn3d+catch_area*TAVGn3d+ae*SNWD+ae*JulianDate+Echange2*TAVGn3d+TreeDens_1*TAVGn3d+GRADIENT*TAVGn3d, data = MFJD_Champfl_Ni) # leave i'th site out and refit model
  MFJD_Champfl_i$y_pred <- predict(mdl, newdata = MFJD_Champfl_i) #predict site data without site info
  MFJD_Champfl_i$y_pred[MFJD_Champfl_i$y_pred < 0] <- 0
  resid<- as.data.frame(MFJD_Champfl_i$AvgDailyTemp  - MFJD_Champfl_i$y_pred) #resid
  resid$site <- i
  resid_list<- rbind(resid_list, resid)
}
colnames(resid_list)<-c("resid","site")
RMSPE_MFJDFl_linear  <- (mean(resid_list$resid^2))^.5 #1.603869
tapply(abs(resid_list$resid),resid_list$site,mean)


RMSPE_MFJDSp_GAM
RMSPE_MFJDSp_linear
RMSPE_MFJDFl_GAM
RMSPE_MFJDFl_linear




#### * Validation dataset predictions (Norwest) #### 
data.sub.NW.sp$predictGAM<- predict(MFJDgsCH, newdata = data.sub.NW.sp)
data.sub.NW.fl$predictGAM<- predict(MFJDgfCH, newdata = data.sub.NW.fl)
data.sub.NW.sp$predictLIN<- predict(MFJDgsCHLin, newdata = data.sub.NW.sp)
data.sub.NW.fl$predictLIN<- predict(MFJDgfCHLin, newdata = data.sub.NW.fl)

data.sub.NW.fl$predictGAM[data.sub.NW.fl$predictGAM < 0] <- 0
data.sub.NW.sp$predictGAM[data.sub.NW.sp$predictGAM < 0] <- 0
data.sub.NW.fl$predictLIN[data.sub.NW.fl$predictLIN < 0] <- 0
data.sub.NW.sp$predictLIN[data.sub.NW.sp$predictLIN < 0] <- 0

data.sub.NW.sp$residGAM<-(data.sub.NW.sp$predictGAM-data.sub.NW.sp$AvgDailyTemp)
data.sub.NW.fl$residGAM<-(data.sub.NW.fl$predictGAM-data.sub.NW.fl$AvgDailyTemp)
data.sub.NW.sp$residLin<-(data.sub.NW.sp$predictLIN-data.sub.NW.sp$AvgDailyTemp)
data.sub.NW.fl$residLin<-(data.sub.NW.fl$predictLIN-data.sub.NW.fl$AvgDailyTemp)

data.sub.NW.sp$residGAM2 <-(data.sub.NW.sp$residGAM)^2
data.sub.NW.fl$residGAM2 <-(data.sub.NW.fl$residGAM)^2
data.sub.NW.sp$residLin2 <-(data.sub.NW.sp$residLin)^2
data.sub.NW.fl$residLin2 <-(data.sub.NW.fl$residLin)^2

mean(data.sub.NW.sp$residGAM2)^.5 #1.51732
mean(data.sub.NW.fl$residGAM2)^.5 #1.582491
mean(data.sub.NW.sp$residLin2)^.5 #1.811227
mean(data.sub.NW.fl$residLin2)^.5 #1.796701

data.sub.NW.COM <- rbind(data.sub.NW.sp, data.sub.NW.fl)

mean(data.sub.NW.COM$residGAM2)^.5 #1.562475
mean(data.sub.NW.COM$residLin2)^.5 #1.80124

plot(AvgDailyTemp~predictGAM, pch =".", data = data.sub.NW.COM)
abline(0,1, col = "red")
plot(residGAM~Julian, pch =".", data= data.sub.NW.COM)
abline(h=0, col = "red")
plot(AvgDailyTemp~predictLIN, pch =".", data= data.sub.NW.COM)
abline(0,1, col = "red")
plot(residLin~Julian, pch =".", data= data.sub.NW.COM)
abline(h=0, col = "red")

#### * RMSE by year for each model ####
data.sub.NW.sp11 <- subset(data.sub.NW.sp, year == 2011)
data.sub.NW.sp10 <- subset(data.sub.NW.sp, year == 2010)
data.sub.NW.sp9 <- subset(data.sub.NW.sp, year == 2009)
data.sub.NW.sp8 <- subset(data.sub.NW.sp, year == 2008)
data.sub.NW.sp7 <- subset(data.sub.NW.sp, year == 2007)
data.sub.NW.sp6 <- subset(data.sub.NW.sp, year == 2006)
data.sub.NW.sp5 <- subset(data.sub.NW.sp, year == 2005)
data.sub.NW.sp4 <- subset(data.sub.NW.sp, year == 2004)
data.sub.NW.sp3 <- subset(data.sub.NW.sp, year == 2003)
data.sub.NW.sp2 <- subset(data.sub.NW.sp, year == 2002)
data.sub.NW.sp1 <- subset(data.sub.NW.sp, year == 2001)
data.sub.NW.sp0 <- subset(data.sub.NW.sp, year == 2000)
data.sub.NW.sp99 <- subset(data.sub.NW.sp, year == 1999)
data.sub.NW.sp98 <- subset(data.sub.NW.sp, year == 1998)
data.sub.NW.sp97 <- subset(data.sub.NW.sp, year == 1997)
data.sub.NW.fl11 <- subset(data.sub.NW.fl, year == 2011)
data.sub.NW.fl10 <- subset(data.sub.NW.fl, year == 2010)
data.sub.NW.fl9 <- subset(data.sub.NW.fl, year == 2009)
data.sub.NW.fl8 <- subset(data.sub.NW.fl, year == 2008)
data.sub.NW.fl7 <- subset(data.sub.NW.fl, year == 2007)
data.sub.NW.fl6 <- subset(data.sub.NW.fl, year == 2006)
data.sub.NW.fl5 <- subset(data.sub.NW.fl, year == 2005)
data.sub.NW.fl4 <- subset(data.sub.NW.fl, year == 2004)
data.sub.NW.fl3 <- subset(data.sub.NW.fl, year == 2003)
data.sub.NW.fl2 <- subset(data.sub.NW.fl, year == 2002)
data.sub.NW.fl1 <- subset(data.sub.NW.fl, year == 2001)
data.sub.NW.fl0 <- subset(data.sub.NW.fl, year == 2000)
data.sub.NW.fl99 <- subset(data.sub.NW.fl, year == 1999)
data.sub.NW.fl98 <- subset(data.sub.NW.fl, year == 1998)
data.sub.NW.fl97 <- subset(data.sub.NW.fl, year == 1997)
data.sub.NW.COM11 <- subset(data.sub.NW.COM, year == 2011)
data.sub.NW.COM10 <- subset(data.sub.NW.COM, year == 2010)
data.sub.NW.COM9 <- subset(data.sub.NW.COM, year == 2009)
data.sub.NW.COM8 <- subset(data.sub.NW.COM, year == 2008)
data.sub.NW.COM7 <- subset(data.sub.NW.COM, year == 2007)
data.sub.NW.COM6 <- subset(data.sub.NW.COM, year == 2006)
data.sub.NW.COM5 <- subset(data.sub.NW.COM, year == 2005)
data.sub.NW.COM4 <- subset(data.sub.NW.COM, year == 2004)
data.sub.NW.COM3 <- subset(data.sub.NW.COM, year == 2003)
data.sub.NW.COM2 <- subset(data.sub.NW.COM, year == 2002)
data.sub.NW.COM1 <- subset(data.sub.NW.COM, year == 2001)
data.sub.NW.COM0 <- subset(data.sub.NW.COM, year == 2000)
data.sub.NW.COM99 <- subset(data.sub.NW.COM, year == 1999)
data.sub.NW.COM98 <- subset(data.sub.NW.COM, year == 1998)
data.sub.NW.COM97 <- subset(data.sub.NW.COM, year == 1997)


MFJD_RMSE_GAM <- data.frame(matrix(NA, nrow = 16, ncol = 5))
colnames(MFJD_RMSE_GAM)<- c("year","springGAM","fallGAM","CombinedGAM","MonthGAM")
MFJD_RMSE_GAM$year<-c(1997:2011,"All")
MFJD_RMSE_GAM[16,2]<-mean(data.sub.NW.sp$residGAM2)^.5 #1.554385
MFJD_RMSE_GAM[15,2]<-mean(data.sub.NW.sp11$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[14,2]<-mean(data.sub.NW.sp10$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[13,2]<-mean(data.sub.NW.sp9$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[12,2]<-mean(data.sub.NW.sp8$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[11,2]<-mean(data.sub.NW.sp7$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[10,2]<-mean(data.sub.NW.sp6$residGAM2)^.5 #0.788395
MFJD_RMSE_GAM[9,2]<-mean(data.sub.NW.sp5$residGAM2)^.5 #0.7452477
MFJD_RMSE_GAM[8,2]<-mean(data.sub.NW.sp4$residGAM2)^.5 #0.5636819
MFJD_RMSE_GAM[7,2]<-mean(data.sub.NW.sp3$residGAM2)^.5 #0.5912394
MFJD_RMSE_GAM[6,2]<-mean(data.sub.NW.sp2$residGAM2)^.5 #0.6820043
MFJD_RMSE_GAM[5,2]<-mean(data.sub.NW.sp1$residGAM2)^.5 #1.011454
MFJD_RMSE_GAM[4,2]<-mean(data.sub.NW.sp0$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[3,2]<-mean(data.sub.NW.sp99$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[2,2]<-mean(data.sub.NW.sp98$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[1,2]<-mean(data.sub.NW.sp97$residGAM2)^.5 #1.000373

MFJD_RMSE_GAM[16,3]<-mean(data.sub.NW.fl$residGAM2)^.5 
MFJD_RMSE_GAM[15,3]<-mean(data.sub.NW.fl11$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[14,3]<-mean(data.sub.NW.fl10$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[13,3]<-mean(data.sub.NW.fl9$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[12,3]<-mean(data.sub.NW.fl8$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[11,3]<-mean(data.sub.NW.fl7$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[10,3]<-mean(data.sub.NW.fl6$residGAM2)^.5 #0.788395
MFJD_RMSE_GAM[9,3]<-mean(data.sub.NW.fl5$residGAM2)^.5 #0.7452477
MFJD_RMSE_GAM[8,3]<-mean(data.sub.NW.fl4$residGAM2)^.5 #0.5636819
MFJD_RMSE_GAM[7,3]<-mean(data.sub.NW.fl3$residGAM2)^.5 #0.5912394
MFJD_RMSE_GAM[6,3]<-mean(data.sub.NW.fl2$residGAM2)^.5 #0.6820043
MFJD_RMSE_GAM[5,3]<-mean(data.sub.NW.fl1$residGAM2)^.5 #1.011454
MFJD_RMSE_GAM[4,3]<-mean(data.sub.NW.fl0$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[3,3]<-mean(data.sub.NW.fl99$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[2,3]<-mean(data.sub.NW.fl98$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[1,3]<-mean(data.sub.NW.fl97$residGAM2)^.5 #1.000373

MFJD_RMSE_GAM[16,4]<-mean(data.sub.NW.COM$residGAM2)^.5 #1.590835
MFJD_RMSE_GAM[15,4]<-mean(data.sub.NW.COM11$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[14,4]<-mean(data.sub.NW.COM10$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[13,4]<-mean(data.sub.NW.COM9$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[12,4]<-mean(data.sub.NW.COM8$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[11,4]<-mean(data.sub.NW.COM7$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[10,4]<-mean(data.sub.NW.COM6$residGAM2)^.5 #0.788395
MFJD_RMSE_GAM[9,4]<-mean(data.sub.NW.COM5$residGAM2)^.5 #0.7452477
MFJD_RMSE_GAM[8,4]<-mean(data.sub.NW.COM4$residGAM2)^.5 #0.5636819
MFJD_RMSE_GAM[7,4]<-mean(data.sub.NW.COM3$residGAM2)^.5 #0.5912394
MFJD_RMSE_GAM[6,4]<-mean(data.sub.NW.COM2$residGAM2)^.5 #0.6820043
MFJD_RMSE_GAM[5,4]<-mean(data.sub.NW.COM1$residGAM2)^.5 #1.011454
MFJD_RMSE_GAM[4,4]<-mean(data.sub.NW.COM0$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[3,4]<-mean(data.sub.NW.COM99$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[2,4]<-mean(data.sub.NW.COM98$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[1,4]<-mean(data.sub.NW.COM97$residGAM2)^.5 #1.000373

MFJD_RMSE_Lin <- data.frame(matrix(NA, nrow = 16, ncol = 5))
colnames(MFJD_RMSE_Lin)<- c("year","springLin","fallLin","CombinedLin","MonthLin")
MFJD_RMSE_Lin$year<-c(1997:2011,"All")
MFJD_RMSE_Lin[16,2]<-mean(data.sub.NW.sp$residLin2)^.5 #2.281095
MFJD_RMSE_Lin[15,2]<-mean(data.sub.NW.sp11$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[14,2]<-mean(data.sub.NW.sp10$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[13,2]<-mean(data.sub.NW.sp9$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[12,2]<-mean(data.sub.NW.sp8$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[11,2]<-mean(data.sub.NW.sp7$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[10,2]<-mean(data.sub.NW.sp6$residLin2)^.5 #0.788395
MFJD_RMSE_Lin[9,2]<-mean(data.sub.NW.sp5$residLin2)^.5 #0.7452477
MFJD_RMSE_Lin[8,2]<-mean(data.sub.NW.sp4$residLin2)^.5 #0.5636819
MFJD_RMSE_Lin[7,2]<-mean(data.sub.NW.sp3$residLin2)^.5 #0.5912394
MFJD_RMSE_Lin[6,2]<-mean(data.sub.NW.sp2$residLin2)^.5 #0.6820043
MFJD_RMSE_Lin[5,2]<-mean(data.sub.NW.sp1$residLin2)^.5 #1.011454
MFJD_RMSE_Lin[4,2]<-mean(data.sub.NW.sp0$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[3,2]<-mean(data.sub.NW.sp99$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[2,2]<-mean(data.sub.NW.sp98$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[1,2]<-mean(data.sub.NW.sp97$residLin2)^.5 #1.000373

MFJD_RMSE_Lin[16,3]<-mean(data.sub.NW.fl$residLin2)^.5 #2.060444
MFJD_RMSE_Lin[15,3]<-mean(data.sub.NW.fl11$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[14,3]<-mean(data.sub.NW.fl10$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[13,3]<-mean(data.sub.NW.fl9$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[12,3]<-mean(data.sub.NW.fl8$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[11,3]<-mean(data.sub.NW.fl7$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[10,3]<-mean(data.sub.NW.fl6$residLin2)^.5 #0.788395
MFJD_RMSE_Lin[9,3]<-mean(data.sub.NW.fl5$residLin2)^.5 #0.7452477
MFJD_RMSE_Lin[8,3]<-mean(data.sub.NW.fl4$residLin2)^.5 #0.5636819
MFJD_RMSE_Lin[7,3]<-mean(data.sub.NW.fl3$residLin2)^.5 #0.5912394
MFJD_RMSE_Lin[6,3]<-mean(data.sub.NW.fl2$residLin2)^.5 #0.6820043
MFJD_RMSE_Lin[5,3]<-mean(data.sub.NW.fl1$residLin2)^.5 #1.011454
MFJD_RMSE_Lin[4,3]<-mean(data.sub.NW.fl0$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[3,3]<-mean(data.sub.NW.fl99$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[2,3]<-mean(data.sub.NW.fl98$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[1,3]<-mean(data.sub.NW.fl97$residLin2)^.5 #1.000373

MFJD_RMSE_Lin[16,4]<-mean(data.sub.NW.COM$residLin2)^.5 #1.919044
MFJD_RMSE_Lin[15,4]<-mean(data.sub.NW.COM11$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[14,4]<-mean(data.sub.NW.COM10$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[13,4]<-mean(data.sub.NW.COM9$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[12,4]<-mean(data.sub.NW.COM8$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[11,4]<-mean(data.sub.NW.COM7$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[10,4]<-mean(data.sub.NW.COM6$residLin2)^.5 #0.788395
MFJD_RMSE_Lin[9,4]<-mean(data.sub.NW.COM5$residLin2)^.5 #0.7452477
MFJD_RMSE_Lin[8,4]<-mean(data.sub.NW.COM4$residLin2)^.5 #0.5636819
MFJD_RMSE_Lin[7,4]<-mean(data.sub.NW.COM3$residLin2)^.5 #0.5912394
MFJD_RMSE_Lin[6,4]<-mean(data.sub.NW.COM2$residLin2)^.5 #0.6820043
MFJD_RMSE_Lin[5,4]<-mean(data.sub.NW.COM1$residLin2)^.5 #1.011454
MFJD_RMSE_Lin[4,4]<-mean(data.sub.NW.COM0$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[3,4]<-mean(data.sub.NW.COM99$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[2,4]<-mean(data.sub.NW.COM98$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[1,4]<-mean(data.sub.NW.COM97$residLin2)^.5 #1.000373


#### * RMSE by month ####

data.sub.NW.COM_01 <- subset(data.sub.NW.COM, month == 1)
data.sub.NW.COM_02 <- subset(data.sub.NW.COM, month == 2)
data.sub.NW.COM_03 <- subset(data.sub.NW.COM, month == 3)
data.sub.NW.COM_04 <- subset(data.sub.NW.COM, month == 4)
data.sub.NW.COM_05 <- subset(data.sub.NW.COM, month == 5)
data.sub.NW.COM_06 <- subset(data.sub.NW.COM, month == 6)
data.sub.NW.COM_07 <- subset(data.sub.NW.COM, month == 7)
data.sub.NW.COM_08 <- subset(data.sub.NW.COM, month == 8)
data.sub.NW.COM_09 <- subset(data.sub.NW.COM, month == 9)
data.sub.NW.COM_10 <- subset(data.sub.NW.COM, month == 10)
data.sub.NW.COM_11 <- subset(data.sub.NW.COM, month == 11)
data.sub.NW.COM_12 <- subset(data.sub.NW.COM, month == 12)

# combined y year MFJD
win.graph(width = 12, height = 12)
par(mfrow= c(4,4), mar=c(2,2,1,1), cex=.8, ps=10)
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==1997), pch=".", ylim =c(0,22), xlim = c (0,22), main ="1997")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==1998), pch=".", ylim =c(0,22), xlim = c (0,22), main ="1998")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==1999), pch=".", ylim =c(0,22), xlim = c (0,22), main ="1999")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2000), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2000")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2001), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2001")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2002), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2002")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2003), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2003")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2004), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2004")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2005), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2005")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2006), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2006")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2007), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2007")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2008), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2008")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2009), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2009")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2010), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2010")
abline(0,1, col = "red", pch=".")
plot(DailyMean~predictGAM,data= subset(data.sub.NW.COM, year==2011), pch=".", ylim =c(0,22), xlim = c (0,22), main ="2011")
abline(0,1, col = "red", pch=".")

#PREDICTING MONTHLY AVERAGES
data.sub.NW.COM$year_month_site <- paste(data.sub.NW.COM$year,"_",data.sub.NW.COM$month, "_",data.sub.NW.COM$PERMA_FID.x,sep = "")
month.avgNWMFJD <- as.data.frame.table(tapply(data.sub.NW.COM$AvgDailyTemp, data.sub.NW.COM$year_month_site,  mean))
colnames(month.avgNWMFJD)<- c("year_month_site","AvgMonthTemp")
month.avgNWMFJD$SiteName<- sub(".*_", "", month.avgNWMFJD$year_month_site)
head(month.avgNWMFJD)
PERMA_FID.xNW <- as.data.frame.table(tapply(data.sub.NW.COM$PERMA_FID.x, data.sub.NW.COM$year_month_site,  mean))
monthNW <- as.data.frame.table(tapply(data.sub.NW.COM$month, data.sub.NW.COM$year_month_site,  mean))
yearNW <- as.data.frame.table(tapply(data.sub.NW.COM$year, data.sub.NW.COM$year_month_site,  mean))
month.avgNWMFJD$PERMA_FID <- PERMA_FID.xNW$Freq
month.avgNWMFJD$year <- yearNW$Freq
month.avgNWMFJD$month <- monthNW$Freq
month.avgNW.CHAMP <- as.data.frame.table(tapply(data.sub.NW.COM$predictGAM, data.sub.NW.COM$year_month_site,  mean))
month.avgNW.Lin <- as.data.frame.table(tapply(data.sub.NW.COM$predictLIN, data.sub.NW.COM$year_month_site,  mean))
month.avgNWMFJD$predGAM <- month.avgNW.CHAMP$Freq
month.avgNWMFJD$predLin <- month.avgNW.Lin$Freq
month.avgNWMFJD$residGAM <-(month.avgNWMFJD$AvgMonthTemp)-(month.avgNWMFJD$predGAM)
month.avgNWMFJD$residGAM2 <-(month.avgNWMFJD$resid)^2
month.avgNWMFJD$residLin <-(month.avgNWMFJD$AvgMonthTemp)-(month.avgNWMFJD$predLin)
month.avgNWMFJD$residLin2 <-(month.avgNWMFJD$residLin)^2
mean(month.avgNWMFJD$residGAM2)^.5 #1.267332
mean(month.avgNWMFJD$residLin2)^.5 #1.552789


plot(AvgMonthTemp~predGAM, month.avgNWMFJD)
abline(0,1, col ="red")
plot(AvgMonthTemp~predLin, month.avgNWMFJD)
abline(0,1, col ="red")

month.avgNWMFJD <- month.avgNWMFJD[order(-month.avgNWMFJD$resid),]
head(month.avgNWMFJD)

month.avgNWMFJD11 <- subset(month.avgNWMFJD, year == 2011)
month.avgNWMFJD10 <- subset(month.avgNWMFJD, year == 2010)
month.avgNWMFJD09 <- subset(month.avgNWMFJD, year == 2009)
month.avgNWMFJD08 <- subset(month.avgNWMFJD, year == 2008)
month.avgNWMFJD07 <- subset(month.avgNWMFJD, year == 2007)
month.avgNWMFJD06 <- subset(month.avgNWMFJD, year == 2006)
month.avgNWMFJD05 <- subset(month.avgNWMFJD, year == 2005)
month.avgNWMFJD04 <- subset(month.avgNWMFJD, year == 2004)
month.avgNWMFJD03 <- subset(month.avgNWMFJD, year == 2003)
month.avgNWMFJD02 <- subset(month.avgNWMFJD, year == 2002)
month.avgNWMFJD01 <- subset(month.avgNWMFJD, year == 2001)
month.avgNWMFJD00 <- subset(month.avgNWMFJD, year == 2000)
month.avgNWMFJD99 <- subset(month.avgNWMFJD, year == 1999)
month.avgNWMFJD98 <- subset(month.avgNWMFJD, year == 1998)
month.avgNWMFJD97 <- subset(month.avgNWMFJD, year == 1997)

MFJD_RMSE_GAM[16,5]<-mean(month.avgNWMFJD$residGAM2)^.5 #1.2883
MFJD_RMSE_GAM[15,5]<-mean(month.avgNWMFJD11$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[14,5]<-mean(month.avgNWMFJD10$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[13,5]<-mean(month.avgNWMFJD09$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[12,5]<-mean(month.avgNWMFJD08$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[11,5]<-mean(month.avgNWMFJD07$residGAM2)^.5 #0.8842574
MFJD_RMSE_GAM[10,5]<-mean(month.avgNWMFJD06$residGAM2)^.5 #0.788395
MFJD_RMSE_GAM[9,5]<-mean(month.avgNWMFJD05$residGAM2)^.5 #0.7452477
MFJD_RMSE_GAM[8,5]<-mean(month.avgNWMFJD04$residGAM2)^.5 #0.5636819
MFJD_RMSE_GAM[7,5]<-mean(month.avgNWMFJD03$residGAM2)^.5 #0.5912394
MFJD_RMSE_GAM[6,5]<-mean(month.avgNWMFJD02$residGAM2)^.5 #0.6820043
MFJD_RMSE_GAM[5,5]<-mean(month.avgNWMFJD01$residGAM2)^.5 #1.011454
MFJD_RMSE_GAM[4,5]<-mean(month.avgNWMFJD00$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[3,5]<-mean(month.avgNWMFJD99$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[2,5]<-mean(month.avgNWMFJD98$residGAM2)^.5 #1.000373
MFJD_RMSE_GAM[1,5]<-mean(month.avgNWMFJD97$residGAM2)^.5 #1.000373

MFJD_RMSE_Lin[16,5]<-mean(month.avgNWMFJD$residLin2)^.5 #1.67982
MFJD_RMSE_Lin[15,5]<-mean(month.avgNWMFJD11$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[14,5]<-mean(month.avgNWMFJD10$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[13,5]<-mean(month.avgNWMFJD09$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[12,5]<-mean(month.avgNWMFJD08$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[11,5]<-mean(month.avgNWMFJD07$residLin2)^.5 #0.8842574
MFJD_RMSE_Lin[10,5]<-mean(month.avgNWMFJD06$residLin2)^.5 #0.788395
MFJD_RMSE_Lin[9,5]<-mean(month.avgNWMFJD05$residLin2)^.5 #0.7452477
MFJD_RMSE_Lin[8,5]<-mean(month.avgNWMFJD04$residLin2)^.5 #0.5636819
MFJD_RMSE_Lin[7,5]<-mean(month.avgNWMFJD03$residLin2)^.5 #0.5912394
MFJD_RMSE_Lin[6,5]<-mean(month.avgNWMFJD02$residLin2)^.5 #0.6820043
MFJD_RMSE_Lin[5,5]<-mean(month.avgNWMFJD01$residLin2)^.5 #1.011454
MFJD_RMSE_Lin[4,5]<-mean(month.avgNWMFJD00$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[3,5]<-mean(month.avgNWMFJD99$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[2,5]<-mean(month.avgNWMFJD98$residLin2)^.5 #1.000373
MFJD_RMSE_Lin[1,5]<-mean(month.avgNWMFJD97$residLin2)^.5 #1.000373







#### * Finish RMSE by monthXmonth table ####
month.avgNWMFJD_01 <- subset(month.avgNWMFJD, month == 1)
month.avgNWMFJD_02 <- subset(month.avgNWMFJD, month == 2)
month.avgNWMFJD_03 <- subset(month.avgNWMFJD, month == 3)
month.avgNWMFJD_04 <- subset(month.avgNWMFJD, month == 4)
month.avgNWMFJD_05 <- subset(month.avgNWMFJD, month == 5)
month.avgNWMFJD_06 <- subset(month.avgNWMFJD, month == 6)
month.avgNWMFJD_07 <- subset(month.avgNWMFJD, month == 7)
month.avgNWMFJD_08 <- subset(month.avgNWMFJD, month == 8)
month.avgNWMFJD_09 <- subset(month.avgNWMFJD, month == 9)
month.avgNWMFJD_10 <- subset(month.avgNWMFJD, month == 10)
month.avgNWMFJD_11 <- subset(month.avgNWMFJD, month == 11)
month.avgNWMFJD_12 <- subset(month.avgNWMFJD, month == 12)

Monthly_Table[1,10]<-mean(month.avgNWMFJD_01$residGAM2)^.5 #1.000373
Monthly_Table[2,10]<-mean(month.avgNWMFJD_02$residGAM2)^.5 #1.000373
Monthly_Table[3,10]<-mean(month.avgNWMFJD_03$residGAM2)^.5 #1.000373
Monthly_Table[4,10]<-mean(month.avgNWMFJD_04$residGAM2)^.5 #1.000373
Monthly_Table[5,10]<-mean(month.avgNWMFJD_05$residGAM2)^.5 #1.000373
Monthly_Table[6,10]<-mean(month.avgNWMFJD_06$residGAM2)^.5 #1.000373
Monthly_Table[7,10]<-mean(month.avgNWMFJD_07$residGAM2)^.5 #1.000373
Monthly_Table[8,10]<-mean(month.avgNWMFJD_08$residGAM2)^.5 #1.000373
Monthly_Table[9,10]<-mean(month.avgNWMFJD_09$residGAM2)^.5 #1.000373
Monthly_Table[10,10]<-mean(month.avgNWMFJD_10$residGAM2)^.5 #1.000373
Monthly_Table[11,10]<-mean(month.avgNWMFJD_11$residGAM2)^.5 #1.000373
Monthly_Table[12,10]<-mean(month.avgNWMFJD_12$residGAM2)^.5 #1.000373
Monthly_Table[13,10]<-mean(month.avgNWMFJD$residGAM2)^.5 #1.000373

Monthly_Table[13,11]<- 1 - ((sum((month.avgNWMFJD$residGAM)^2))/(sum((month.avgNWMFJD$AvgMonthTemp-(mean(month.avgNWMFJD$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,11]<- 1 - ((sum((month.avgNWMFJD_12$residGAM)^2))/(sum((month.avgNWMFJD_12$AvgMonthTemp-(mean(month.avgNWMFJD_12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,11]<- 1 - ((sum((month.avgNWMFJD_11$residGAM)^2))/(sum((month.avgNWMFJD_11$AvgMonthTemp-(mean(month.avgNWMFJD_11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,11]<- 1 - ((sum((month.avgNWMFJD_10$residGAM)^2))/(sum((month.avgNWMFJD_10$AvgMonthTemp-(mean(month.avgNWMFJD_10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,11]<- 1 - ((sum((month.avgNWMFJD_09$residGAM)^2))/(sum((month.avgNWMFJD_09$AvgMonthTemp-(mean(month.avgNWMFJD_09$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,11]<- 1 - ((sum((month.avgNWMFJD_08$residGAM)^2))/(sum((month.avgNWMFJD_08$AvgMonthTemp-(mean(month.avgNWMFJD_08$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,11]<- 1 - ((sum((month.avgNWMFJD_07$residGAM)^2))/(sum((month.avgNWMFJD_07$AvgMonthTemp-(mean(month.avgNWMFJD_07$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,11]<- 1 - ((sum((month.avgNWMFJD_06$residGAM)^2))/(sum((month.avgNWMFJD_06$AvgMonthTemp-(mean(month.avgNWMFJD_06$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,11]<- 1 - ((sum((month.avgNWMFJD_05$residGAM)^2))/(sum((month.avgNWMFJD_05$AvgMonthTemp-(mean(month.avgNWMFJD_05$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,11]<- 1 - ((sum((month.avgNWMFJD_04$residGAM)^2))/(sum((month.avgNWMFJD_04$AvgMonthTemp-(mean(month.avgNWMFJD_04$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,11]<- 1 - ((sum((month.avgNWMFJD_03$residGAM)^2))/(sum((month.avgNWMFJD_03$AvgMonthTemp-(mean(month.avgNWMFJD_03$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,11]<- 1 - ((sum((month.avgNWMFJD_02$residGAM)^2))/(sum((month.avgNWMFJD_02$AvgMonthTemp-(mean(month.avgNWMFJD_02$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,11]<- 1 - ((sum((month.avgNWMFJD_01$residGAM)^2))/(sum((month.avgNWMFJD_01$AvgMonthTemp-(mean(month.avgNWMFJD_01$AvgMonthTemp)))^2))) #0.402458

Monthly_Table[1,12]<-mean(month.avgNWMFJD_01$residLin2)^.5 #1.000373
Monthly_Table[2,12]<-mean(month.avgNWMFJD_02$residLin2)^.5 #1.000373
Monthly_Table[3,12]<-mean(month.avgNWMFJD_03$residLin2)^.5 #1.000373
Monthly_Table[4,12]<-mean(month.avgNWMFJD_04$residLin2)^.5 #1.000373
Monthly_Table[5,12]<-mean(month.avgNWMFJD_05$residLin2)^.5 #1.000373
Monthly_Table[6,12]<-mean(month.avgNWMFJD_06$residLin2)^.5 #1.000373
Monthly_Table[7,12]<-mean(month.avgNWMFJD_07$residLin2)^.5 #1.000373
Monthly_Table[8,12]<-mean(month.avgNWMFJD_08$residLin2)^.5 #1.000373
Monthly_Table[9,12]<-mean(month.avgNWMFJD_09$residLin2)^.5 #1.000373
Monthly_Table[10,12]<-mean(month.avgNWMFJD_10$residLin2)^.5 #1.000373
Monthly_Table[11,12]<-mean(month.avgNWMFJD_11$residLin2)^.5 #1.000373
Monthly_Table[12,12]<-mean(month.avgNWMFJD_12$residLin2)^.5 #1.000373
Monthly_Table[13,12]<-mean(month.avgNWMFJD$residLin2)^.5 #1.000373

Monthly_Table[13,13]<- 1 - ((sum((month.avgNWMFJD$residLin)^2))/(sum((month.avgNWMFJD$AvgMonthTemp-(mean(month.avgNWMFJD$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[12,13]<- 1 - ((sum((month.avgNWMFJD_12$residLin)^2))/(sum((month.avgNWMFJD_12$AvgMonthTemp-(mean(month.avgNWMFJD_12$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[11,13]<- 1 - ((sum((month.avgNWMFJD_11$residLin)^2))/(sum((month.avgNWMFJD_11$AvgMonthTemp-(mean(month.avgNWMFJD_11$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[10,13]<- 1 - ((sum((month.avgNWMFJD_10$residLin)^2))/(sum((month.avgNWMFJD_10$AvgMonthTemp-(mean(month.avgNWMFJD_10$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[09,13]<- 1 - ((sum((month.avgNWMFJD_09$residLin)^2))/(sum((month.avgNWMFJD_09$AvgMonthTemp-(mean(month.avgNWMFJD_09$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[08,13]<- 1 - ((sum((month.avgNWMFJD_08$residLin)^2))/(sum((month.avgNWMFJD_08$AvgMonthTemp-(mean(month.avgNWMFJD_08$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[07,13]<- 1 - ((sum((month.avgNWMFJD_07$residLin)^2))/(sum((month.avgNWMFJD_07$AvgMonthTemp-(mean(month.avgNWMFJD_07$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[06,13]<- 1 - ((sum((month.avgNWMFJD_06$residLin)^2))/(sum((month.avgNWMFJD_06$AvgMonthTemp-(mean(month.avgNWMFJD_06$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[05,13]<- 1 - ((sum((month.avgNWMFJD_05$residLin)^2))/(sum((month.avgNWMFJD_05$AvgMonthTemp-(mean(month.avgNWMFJD_05$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[04,13]<- 1 - ((sum((month.avgNWMFJD_04$residLin)^2))/(sum((month.avgNWMFJD_04$AvgMonthTemp-(mean(month.avgNWMFJD_04$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[03,13]<- 1 - ((sum((month.avgNWMFJD_03$residLin)^2))/(sum((month.avgNWMFJD_03$AvgMonthTemp-(mean(month.avgNWMFJD_03$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[02,13]<- 1 - ((sum((month.avgNWMFJD_02$residLin)^2))/(sum((month.avgNWMFJD_02$AvgMonthTemp-(mean(month.avgNWMFJD_02$AvgMonthTemp)))^2))) #0.402458
Monthly_Table[01,13]<- 1 - ((sum((month.avgNWMFJD_01$residLin)^2))/(sum((month.avgNWMFJD_01$AvgMonthTemp-(mean(month.avgNWMFJD_01$AvgMonthTemp)))^2))) #0.402458


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

