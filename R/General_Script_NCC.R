#####################################################################
#####################################################################
### Data Coral Size Population - Little by little we're gonna far ###
#####################################################################
#####################################################################

rm(list=ls()) ; setwd("data/")
library('ggplot2') ; library('readxl') ; library('cowplot') ; library('tidyverse') ; library('rstan') ; 
library('bayesplot') ; library('parallel') ; library('purrr') ; library('brms') ; library('abind') ; library('export')
options(mc.cores = parallel::detectCores())
## Loading datasets
load("Calcification_Alizarin_Metabo.RData")
load("SAR_Photogramm.RData")
load("CC_Dataset.RData")
load("Data_Coral_SnS_2005.RData")
load("Data_Coral_SnS_2008_2009_2010.RData")
load("Data_Coral_SnS_2011_2012.RData")
load("Data_Coral_SnS_2013_2014_2015.RData")
load("Data_Coral_SnS_2016.RData")
## Dataset built
load("Data_SNS_Complete_100iter.RData")
load("data_SNS_complete.RData")
## Models built
load("Models/model_iso_Calcif.RData")
load("Models/model_alo_Calcif.RData")

#################################################################
# PART 1 - RANDOM DEFINITION OF THE SIZE DISTRIBUTION IN MOOREA #
#################################################################

### Photogrammetry
SAR_Photogramm$Mean_Area = SAR_Photogramm$Mean_Area*10000 # Sa (units) = cm-2
fit_Coral <- nls(data = SAR_Photogramm, formula = Mean_Area ~ a*Mean_Diam^b, start = list(a = 1, b = 1))
Predict_Area <- predict(fit_Coral, newdata = Data_Coral_SnS, allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)

# Tracing the coral cover for the 3 main species
Fig_2 = ggplot(data = CC_Dataset,aes(x = Year, y = Coral_Cover*100)) + geom_point(aes(col = "green3")) + 
  stat_smooth(method="lm", col = "green3", fill = "darkolivegreen2", se=T, alpha = 0.2, formula=y ~ poly(x, 2, raw=T)) +
  scale_color_manual(values=c("darkolivegreen2")) + scale_x_continuous(name="") + scale_y_continuous(name = "Coral Cover (%)")

### Random sampling ###
## 1st Dataset - Database 2005
# Spliting the dataset for each species
Species <- c("ACR","POC","POR")
Data_Coral_SnS_2005$Date <- rep("01.01.2005",dim(Data_Coral_SnS_2005)[1]) ; Data_Coral_SnS_2005$Bidimensionnal_Area = ((Data_Coral_SnS_2005$X+Data_Coral_SnS_2005$Y)/4)^2*pi
Data_Coral_2005_ACR <- subset(Data_Coral_SnS_2005, Species == "ACR") ; Data_Coral_2005_ACR$IDS <- seq(1,length(Data_Coral_2005_ACR$Bidimensionnal_Area),1)
Data_Coral_2005_POC <- subset(Data_Coral_SnS_2005, Species == "POC") ; Data_Coral_2005_POC$IDS <- seq(1,length(Data_Coral_2005_POC$Bidimensionnal_Area),1)
Data_Coral_2005_POR <- subset(Data_Coral_SnS_2005, Species == "POR") ; Data_Coral_2005_POR$IDS <- seq(1,length(Data_Coral_2005_POR$Bidimensionnal_Area),1)
# Sampling
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(Data_Coral_2005_ACR)[1],1) ; sampled_ACR <- Data_Coral_2005_ACR[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[2,2]*100000 ) break }
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(Data_Coral_2005_POC)[1],1) ; sampled_POC <- Data_Coral_2005_POC[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[2,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(Data_Coral_2005_POR)[1],1) ; sampled_POR <- Data_Coral_2005_POR[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[2,4]*100000 ) break } 
ACR_2005 = merge(x= data.frame(ACR_ids), y = Data_Coral_2005_ACR, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2005 = merge(x= data.frame(POC_ids), y = Data_Coral_2005_POC, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2005 = merge(x= data.frame(POR_ids), y = Data_Coral_2005_POR, by.x = "POR_ids", by.y = "IDS", all = F)
# Merging sampling 2005
Data_Coral_SnS_2005 <- rbind(ACR_2005[,-1],POC_2005[,-1],POR_2005[,-1])
# Predicting Area and database management
Predict_Area_2005 <- predict(fit_Coral, newdata = Data_Coral_SnS_2005, allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
Data_Coral_SnS_2005 <- data.frame(Data_Coral_SnS_2005,Mean_Area = Predict_Area_2005)
colnames(Data_Coral_SnS_2005) = c("Date","Species","Site", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area", "Surface_Area")

## 2nd Dataset - Database 2008-2010
# Spliting the dataset for each species
Data_Coral_SnS_2008_2009_2010$Bidimensionnal_Area = ((Data_Coral_SnS_2008_2009_2010$X+Data_Coral_SnS_2008_2009_2010$Y)/4)^2*pi
Data_Coral_2008 <- subset(Data_Coral_SnS_2008_2009_2010, Date == "01.01.2008")
SNS01  <- subset(Data_Coral_2008, Species == "ACR") ; SNS01$IDS <- seq(1,length(SNS01$Bidimensionnal_Area),1)
SNS02  <- subset(Data_Coral_2008, Species == "POC") ; SNS02$IDS <- seq(1,length(SNS02$Bidimensionnal_Area),1)
SNS03  <- subset(Data_Coral_2008, Species == "POR") ; SNS03$IDS <- seq(1,length(SNS03$Bidimensionnal_Area),1)
Data_Coral_2009 <- subset(Data_Coral_SnS_2008_2009_2010, Date == "01.01.2009")
SNS04  <- subset(Data_Coral_2009, Species == "ACR") ; SNS04$IDS <- seq(1,length(SNS04$Bidimensionnal_Area),1)
SNS05  <- subset(Data_Coral_2009, Species == "POC") ; SNS05$IDS <- seq(1,length(SNS05$Bidimensionnal_Area),1)
SNS06  <- subset(Data_Coral_2009, Species == "POR") ; SNS06$IDS <- seq(1,length(SNS06$Bidimensionnal_Area),1)
Data_Coral_2010 <- subset(Data_Coral_SnS_2008_2009_2010, Date == "01.01.2010")
SNS07  <- subset(Data_Coral_2010, Species == "ACR") ; SNS07$IDS <- seq(1,length(SNS07$Bidimensionnal_Area),1)
SNS08  <- subset(Data_Coral_2010, Species == "POC") ; SNS08$IDS <- seq(1,length(SNS08$Bidimensionnal_Area),1)
SNS09  <- subset(Data_Coral_2010, Species == "POR") ; SNS09$IDS <- seq(1,length(SNS09$Bidimensionnal_Area),1)
# Sampling 2008
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS01)[1],1) ; sampled_ACR <- SNS01[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[5,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS02)[1],1) ; sampled_POC <- SNS02[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[5,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS03)[1],1) ; sampled_POR <- SNS03[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[5,4]*100000 ) break } 
ACR_2008 = merge(x= data.frame(ACR_ids), y = SNS01, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2008 = merge(x= data.frame(POC_ids), y = SNS02, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2008 = merge(x= data.frame(POR_ids), y = SNS03, by.x = "POR_ids", by.y = "IDS", all = F)
# Sampling 2009
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS04)[1],1) ; sampled_ACR <- SNS04[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[6,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS05)[1],1) ; sampled_POC <- SNS05[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[6,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS06)[1],1) ; sampled_POR <- SNS06[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[6,4]*100000 ) break } 
ACR_2009 = merge(x= data.frame(ACR_ids), y = SNS04, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2009 = merge(x= data.frame(POC_ids), y = SNS05, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2009 = merge(x= data.frame(POR_ids), y = SNS06, by.x = "POR_ids", by.y = "IDS", all = F)
# Sampling 2010
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS07)[1],1) ; sampled_ACR <- SNS07[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[7,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS08)[1],1) ; sampled_POC <- SNS08[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[7,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS09)[1],1) ; sampled_POR <- SNS09[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[7,4]*100000 ) break } 
ACR_2010 = merge(x= data.frame(ACR_ids), y = SNS07, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2010 = merge(x= data.frame(POC_ids), y = SNS08, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2010 = merge(x= data.frame(POR_ids), y = SNS09, by.x = "POR_ids", by.y = "IDS", all = F)
# Merging sampling 2008, 2009 and 2010
Data_Coral_SnS_2008_2009_2010 <- rbind(ACR_2008[,-1],ACR_2009[,-1],ACR_2010[,-1],POC_2008[,-1],POC_2009[,-1],POC_2010[,-1],POR_2008[,-1],POR_2009[,-1],POR_2010[,-1])
# Predicting Area and database management
Predict_Area_2008_2009_2010 <- predict(fit_Coral, newdata = Data_Coral_SnS_2008_2009_2010, allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
Data_Coral_SnS_2008_2009_2010 <- data.frame(Data_Coral_SnS_2008_2009_2010, Mean_Area = Predict_Area_2008_2009_2010) 
colnames(Data_Coral_SnS_2008_2009_2010) = c("Date", "Species", "Site", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area", "Surface_Area")

## 3rd Dataset - Database 2011-2012
# Spliting the dataset for each species
Data_Coral_SnS_2011_2012$Bidimensionnal_Area = ((Data_Coral_SnS_2011_2012$X+Data_Coral_SnS_2011_2012$Y)/4)^2*pi
Data_Coral_2011 <- subset(Data_Coral_SnS_2011_2012, Date == "01.01.2011")
SNS01  <- subset(Data_Coral_2011, Species == "ACR") ; SNS01$IDS <- seq(1,length(SNS01$Bidimensionnal_Area),1)
SNS02  <- subset(Data_Coral_2011, Species == "POC") ; SNS02$IDS <- seq(1,length(SNS02$Bidimensionnal_Area),1)
SNS03  <- subset(Data_Coral_2011, Species == "POR") ; SNS03$IDS <- seq(1,length(SNS03$Bidimensionnal_Area),1)
Data_Coral_2012 <- subset(Data_Coral_SnS_2011_2012, Date == "01.01.2012")
SNS04  <- subset(Data_Coral_2012, Species == "ACR") ; SNS04$IDS <- seq(1,length(SNS04$Bidimensionnal_Area),1) 
SNS05  <- subset(Data_Coral_2012, Species == "POC") ; SNS05$IDS <- seq(1,length(SNS05$Bidimensionnal_Area),1) 
SNS06  <- subset(Data_Coral_2012, Species == "POR") ; SNS06$IDS <- seq(1,length(SNS06$Bidimensionnal_Area),1)
# Sampling 2011
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS01)[1],1) ; sampled_ACR <- SNS01[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[8,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS02)[1],1) ; sampled_POC <- SNS02[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[8,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS03)[1],1) ; sampled_POR <- SNS03[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[8,4]*100000 ) break } 
ACR_2011 = merge(x= data.frame(ACR_ids), y = SNS01, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2011 = merge(x= data.frame(POC_ids), y = SNS02, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2011 = merge(x= data.frame(POR_ids), y = SNS03, by.x = "POR_ids", by.y = "IDS", all = F)
# Sampling 2012
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS04)[1],1) ; sampled_ACR <- SNS04[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[9,2]*100000 ) break }
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS05)[1],1) ; sampled_POC <- SNS05[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[9,3]*100000 ) break }
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS06)[1],1) ; sampled_POR <- SNS06[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[9,4]*100000 ) break } 
ACR_2012 = merge(x= data.frame(ACR_ids), y = SNS04, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2012 = merge(x= data.frame(POC_ids), y = SNS05, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2012 = merge(x= data.frame(POR_ids), y = SNS06, by.x = "POR_ids", by.y = "IDS", all = F)
# Merging sampling 2011 and 2012
Data_Coral_SnS_2011_2012 <- rbind(ACR_2011[,-1],ACR_2012[,-1],POC_2011[,-1],POC_2012[,-1],POR_2011[,-1],POR_2012[,-1])
# Predicting Area and database management
Predict_Area_2011_2012 <- predict(fit_Coral, newdata = Data_Coral_SnS_2011_2012, allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
Data_Coral_SnS_2011_2012 <- data.frame(Data_Coral_SnS_2011_2012, Mean_Area = Predict_Area_2011_2012)
colnames(Data_Coral_SnS_2011_2012) = c("Date","Site","Species", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area", "Surface_Area")

## 4th Dataset - Database 2013-2015
# Spliting the dataset for each species
Data_Coral_SnS_2013_2014_2015$Bidimensionnal_Area = ((Data_Coral_SnS_2013_2014_2015$L2013+Data_Coral_SnS_2013_2014_2015$W2013)/4)^2*pi
Data_Coral_2013 <- subset(Data_Coral_SnS_2013_2014_2015, Date == "01.01.2013")
SNS01  <- subset(Data_Coral_2013, Species == "ACR") ; SNS01$IDS <- seq(1,length(SNS01$Bidimensionnal_Area),1)
SNS02  <- subset(Data_Coral_2013, Species == "POC") ; SNS02$IDS <- seq(1,length(SNS02$Bidimensionnal_Area),1) 
SNS03  <- subset(Data_Coral_2013, Species == "POR") ; SNS03$IDS <- seq(1,length(SNS03$Bidimensionnal_Area),1)
Data_Coral_2014 <- subset(Data_Coral_SnS_2013_2014_2015, Date == "01.01.2014")
SNS04  <- subset(Data_Coral_2014, Species == "ACR") ; SNS04$IDS <- seq(1,length(SNS04$Bidimensionnal_Area),1) 
SNS05  <- subset(Data_Coral_2014, Species == "POC") ; SNS05$IDS <- seq(1,length(SNS05$Bidimensionnal_Area),1) 
SNS06  <- subset(Data_Coral_2014, Species == "POR") ; SNS06$IDS <- seq(1,length(SNS06$Bidimensionnal_Area),1)
Data_Coral_2015 <- subset(Data_Coral_SnS_2013_2014_2015, Date == "01.01.2015")
SNS07  <- subset(Data_Coral_2015, Species == "ACR") ; SNS07$IDS <- seq(1,length(SNS07$Bidimensionnal_Area),1) 
SNS08  <- subset(Data_Coral_2015, Species == "POC") ; SNS08$IDS <- seq(1,length(SNS08$Bidimensionnal_Area),1) 
SNS09  <- subset(Data_Coral_2015, Species == "POR") ; SNS09$IDS <- seq(1,length(SNS09$Bidimensionnal_Area),1)
# Sampling 2013
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS01)[1],1) ; sampled_ACR <- SNS01[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[10,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS02)[1],1) ; sampled_POC <- SNS02[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[10,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS03)[1],1) ; sampled_POR <- SNS03[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[10,4]*100000 ) break } 
ACR_2013 = merge(x= data.frame(ACR_ids), y = SNS01, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2013 = merge(x= data.frame(POC_ids), y = SNS02, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2013 = merge(x= data.frame(POR_ids), y = SNS03, by.x = "POR_ids", by.y = "IDS", all = F)
# Sampling 2014
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS04)[1],1) ; sampled_ACR <- SNS04[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[11,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS05)[1],1) ; sampled_POC <- SNS05[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[11,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS06)[1],1) ; sampled_POR <- SNS06[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[11,4]*100000 ) break } 
ACR_2014 = merge(x= data.frame(ACR_ids), y = SNS04, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2014 = merge(x= data.frame(POC_ids), y = SNS05, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2014 = merge(x= data.frame(POR_ids), y = SNS06, by.x = "POR_ids", by.y = "IDS", all = F)
# Sampling 2015
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS07)[1],1) ; sampled_ACR <- SNS07[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[12,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS08)[1],1) ; sampled_POC <- SNS08[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[12,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS09)[1],1) ; sampled_POR <- SNS09[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[12,4]*100000 ) break } 
ACR_2015 = merge(x= data.frame(ACR_ids), y = SNS07, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2015 = merge(x= data.frame(POC_ids), y = SNS08, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2015 = merge(x= data.frame(POR_ids), y = SNS09, by.x = "POR_ids", by.y = "IDS", all = F)
# Merging sampling 2008, 2009 and 2010
Data_Coral_SnS_2013_2014_2015 <- rbind(ACR_2013[,-1],ACR_2014[,-1],ACR_2015[,-1],POC_2013[,-1],POC_2014[,-1],POC_2015[,-1],POR_2013[,-1],POR_2014[,-1],POR_2015[,-1])
# Predicting Area and database management
Predict_Area_2013_2014_2015 <- predict(fit_Coral, newdata = Data_Coral_SnS_2013_2014_2015, allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
Data_Coral_SnS_2013_2014_2015 <- data.frame(Data_Coral_SnS_2013_2014_2015, Mean_Area = Predict_Area_2013_2014_2015) 
colnames(Data_Coral_SnS_2013_2014_2015) = c("Date","Site","Species", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area", "Surface_Area")

## 5th Dataset - Database 2016
# Spliting the dataset for each species
Data_Coral_SnS_2016$Bidimensionnal_Area = ((Data_Coral_SnS_2016$X+Data_Coral_SnS_2016$Y)/4)^2*pi
Data_Coral_2016 <- subset(Data_Coral_SnS_2016, Date == "01.01.2016")
SNS01  <- subset(Data_Coral_2016, Species == "ACR") ; SNS01$IDS <- seq(1,length(SNS01$Bidimensionnal_Area),1)
SNS02  <- subset(Data_Coral_2016, Species == "POC") ; SNS02$IDS <- seq(1,length(SNS02$Bidimensionnal_Area),1) 
SNS03  <- subset(Data_Coral_2016, Species == "POR") ; SNS03$IDS <- seq(1,length(SNS03$Bidimensionnal_Area),1)
# Sampling
ACR_ids <- c() ; sampled_ACR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS01)[1],1) ; sampled_ACR <- SNS01[sampled_id,]$Bidimensionnal_Area+sampled_ACR
  ACR_ids <- c(ACR_ids, sampled_id) ; if( sum(sampled_ACR) > CC_Dataset[13,2]*100000 ) break } 
POC_ids <- c() ; sampled_POC <- 0; repeat {
  sampled_id <- sample(1:dim(SNS02)[1],1) ; sampled_POC <- SNS02[sampled_id,]$Bidimensionnal_Area+sampled_POC
  POC_ids <- c(POC_ids, sampled_id) ; if( sum(sampled_POC) > CC_Dataset[13,3]*100000 ) break } 
POR_ids <- c() ; sampled_POR <- 0; repeat {
  sampled_id <- sample(1:dim(SNS03)[1],1) ; sampled_POR <- SNS03[sampled_id,]$Bidimensionnal_Area+sampled_POR
  POR_ids <- c(POR_ids, sampled_id) ; if( sum(sampled_POR) > CC_Dataset[13,4]*100000 ) break } 
ACR_2016 = merge(x= data.frame(ACR_ids), y = SNS01, by.x = "ACR_ids", by.y = "IDS", all = F)
POC_2016 = merge(x= data.frame(POC_ids), y = SNS02, by.x = "POC_ids", by.y = "IDS", all = F)
POR_2016 = merge(x= data.frame(POR_ids), y = SNS03, by.x = "POR_ids", by.y = "IDS", all = F)
# Merging sampling
Data_Coral_SnS_2016 <- rbind(ACR_2016[,-1],POC_2016[,-1],POR_2016[,-1])
# Predicting Area and database management
Predict_Area_2016 <- predict(fit_Coral, newdata = Data_Coral_SnS_2016, allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
Data_Coral_SnS_2016 <- data.frame(Data_Coral_SnS_2016, Mean_Area = Predict_Area_2016) 
colnames(Data_Coral_SnS_2016) = c("Date","Site","Species", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area", "Surface_Area")

## Final table managing and merging
Table_Final <- rbind(Data_Coral_SnS_2005,Data_Coral_SnS_2008_2009_2010,Data_Coral_SnS_2011_2012,
                     Data_Coral_SnS_2013_2014_2015,Data_Coral_SnS_2016)
colnames(Table_Final) <- c("Year","Species","Site","X","Y","Z","Mean_Diam","Bidimensionnal_Area","Surface_Area")
# Saving table (to run ~100 times)
write.table(Table_Final, "Predictions/SNS_001.xls", dec = ",", sep="\t", row.names = F)

##########################################################################################################################
### > FOR MORE ACCURACY IT IS POSSIBLE TO RUN THE CURRENT SCRIPT 100 TIMES AND TO SAVE EACH DATA IN AN UNIQUE FOLDER < ###
####### > HOWEVER THE RANDOMISATION + THE FUTURE BAYESIAN FRAMEWORK GIVE US ALREADY AN ACURATE CONFIDENT INTERVAL < ######
####### > THIS PROCEDURE COULD BE THUS BE AVOIDED ACCORDING TO THIS STUDY. FOR BEING EVEN MORE ROBUST, WE DID IT. < ######
##########################################################################################################################

# Opening document from a same folder
 setwd("Predictions/") ; library(here)
# files_names <- list.files(here())
 nb_files <- length(files_names)
# data_names <- vector("list",length=nb_files)
 for (i in 1 : nb_files) {data_names[i] <- strsplit(files_names[i], split=".xls")}
 for (i in 1:nb_files) {assign(data_names[[i]], read.delim2(paste(here(files_names[i]))))}
# Save data in the same folder
 dataset_list <- vector("list",length=nb_files)
 for (i in 1:nb_files) {dataset_list[[i]] <- get(data_names[[i]])}
 size_iter = vector("list", length=nb_files)
 for (i in 1:nb_files) {size_iter[[i]] = dim(dataset_list[[i]])[1]}
 vector_Size = vector("list", length=nb_files)
 for (i in 1:nb_files) {vector_Size[[i]] = rep(i,size_iter[[i]])}
 Size_vector_Iter = vector_Size[[1]]
 for (i in 2:nb_files){Size_vector_Iter <- c(Size_vector_Iter, vector_Size[[i]])}
# Compiling everything 
 complete_data <- dataset_list[[1]]
 for (i in 2:nb_files){complete_data <- rbind(complete_data, dataset_list[[i]])}
# Final data
 complete_data = cbind(complete_data, Size_vector_Iter)
# Finalising the final dataset with 100 random iterations
 write.table(complete_data, "data/SNS_100iter.xls", dec = ",", sep="\t", row.names = F)

##############################################################
# PART 2 - ELABORATING A CORAL GROWTH MODEL FOR EACH SPECIES #
##############################################################

## Spliting the Growth dataset for the 3 main species
Met01  <- subset(Calcification_Alizarin_Metabo, Species == "Acropora_hyacinthus"  )
Met02  <- subset(Calcification_Alizarin_Metabo, Species == "Pocillopora_verrucosa")
Met03  <- subset(Calcification_Alizarin_Metabo, Species == "Porites_lutea"        )
Met <- list(Met01,Met02,Met03) ; Pred_meta <- list(NA,NA,NA) ; model_alo_Calcif = list(NA,NA,NA) ; model_iso_Calcif = list(NA,NA,NA)
# Bayesian calcification model (BCM)
for (i in 1:3) {
  model_alo_Calcif[[i]] <- brm(bf(Production_g_yr ~ a*Surface_Area^b+0, a ~ 1, b ~ 1, nl = TRUE), iter = 3000,
                               data = Met[[i]], family = gaussian(),
                               prior = c(prior(normal(10,10), nlpar = "a"),prior(normal(0.5, 0.5), nlpar = "b")),
                               control = list(adapt_delta = 0.999, max_treedepth = 30))
  model_iso_Calcif[[i]] <- brm(bf(Production_g_yr ~ a*Surface_Area+0, a ~ 1, nl = TRUE), iter = 3000,
                               data = Met[[i]], family = gaussian(),
                               prior = prior(normal(1,1), nlpar = "a"),
                               control = list(adapt_delta = 0.999, max_treedepth = 30))}
save(model_alo_Calcif, file = "model_alo_Calcif.RData") ; save(model_iso_Calcif, file = "model_iso_Calcif.RData")
pred_ACR <- fitted(model_alo_Calcif[[1]]) ; pred_POC <- fitted(model_alo_Calcif[[2]]) ; pred_POR <- fitted(model_alo_Calcif[[3]])
pred_ACR <- data.frame(model_alo_Calcif[[1]]$data, pred_ACR, Species = rep("Acropora_hyacinthus",dim(fitted(model_alo_Calcif[[1]]))[1]))
pred_POC <- data.frame(model_alo_Calcif[[2]]$data, pred_POC, Species = rep("Pocillopora_verrucosa",dim(fitted(model_alo_Calcif[[2]]))[1]))
pred_POR <- data.frame(model_alo_Calcif[[3]]$data, pred_POR, Species = rep("Porites_lutea",dim(fitted(model_alo_Calcif[[3]]))[1]))
pred = rbind(pred_ACR,pred_POC,pred_POR)
# Plotting the Growth result
Fig_3a = ggplot(Calcification_Alizarin_Metabo, aes(x = Surface_Area, y = Production_g_yr, col = Species)) + 
  geom_ribbon(data = pred, aes(x = Surface_Area, ymin = `Q2.5`, ymax = `Q97.5`, fill = Species), alpha = .5) + theme_classic() +
  geom_smooth(data = pred, aes(y = Estimate, fill = Species), method = 'nls', se=F, formula = y~a*x^b, start = c(a=1,b=-1)) + geom_point(aes(shape = Method), alpha = .5) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3")) + scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3")) +
  scale_y_continuous(name = expression("Calcification rate (kg.yr"^-1*")"), labels=c(0,1,2,3,4)) + theme(legend.text = element_text(face = "italic")) +
  scale_x_continuous(name = expression("Surface Area (cm"^2*")"))
# Bayesian reverse calcification model (BrCM)
model_alo_Calcif_area_normalized = list(NA,NA,NA) ; model_iso_Calcif_area_normalized = list(NA,NA,NA)
for (i in 1:3) {
  model_alo_Calcif_area_normalized[[i]] <- brm(bf(Production_g_yr/Surface_Area ~ a*Surface_Area^b+0, a ~ 1, b ~ 1, nl = TRUE), iter = 3000,
                               data = Met[[i]], family = gaussian(),
                               prior = c(prior(normal(10,10), nlpar = "a"),prior(normal(0.5, 0.5), nlpar = "b")),
                               control = list(adapt_delta = 0.999, max_treedepth = 30))
  model_iso_Calcif_area_normalized[[i]] <- brm(bf(Production_g_yr/Surface_Area ~ a*Surface_Area+0, a ~ 1, nl = TRUE), iter = 3000,
                               data = Met[[i]], family = gaussian(),
                               prior = prior(normal(1,1), nlpar = "a"),
                               control = list(adapt_delta = 0.999, max_treedepth = 30))}
pred_ACR <- fitted(model_alo_Calcif_area_normalized[[1]]) ; pred_POC <- fitted(model_alo_Calcif_area_normalized[[2]]) ; pred_POR <- fitted(model_alo_Calcif_area_normalized[[3]])
pred_ACR <- data.frame(model_alo_Calcif_area_normalized[[1]]$data, pred_ACR, Species = rep("Acropora_hyacinthus",dim(fitted(model_alo_Calcif_area_normalized[[1]]))[1]))
pred_POC <- data.frame(model_alo_Calcif_area_normalized[[2]]$data, pred_POC, Species = rep("Pocillopora_verrucosa",dim(fitted(model_alo_Calcif_area_normalized[[2]]))[1]))
pred_POR <- data.frame(model_alo_Calcif_area_normalized[[3]]$data, pred_POR, Species = rep("Porites_lutea",dim(fitted(model_alo_Calcif_area_normalized[[3]]))[1]))
pred = rbind(pred_ACR,pred_POC,pred_POR)
# Plotting the Productivity result
Fig_3b = ggplot(Calcification_Alizarin_Metabo, aes(x = Surface_Area, y = Production_g_yr/Surface_Area, col = Species)) + 
  geom_ribbon(data = pred, aes(x = Surface_Area, ymin = `Q2.5`, ymax = `Q97.5`, fill = Species), alpha = .5) + theme_classic() +
  geom_smooth(data = pred, aes(y = Estimate, fill = Species), method = 'nls', se=F, formula = y~a*x^b, start = c(a=1,b=-1)) + geom_point(aes(shape = Method), alpha = .5) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3")) + scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3")) +
  scale_y_continuous(name = expression("Area-normalized calcification rate (g.cm"^-2*"yr"^-1*")")) + theme(legend.text = element_text(face = "italic")) +
  scale_x_continuous(name = expression("Surface Area (cm"^2*")"))

###############################################################
# PART 3 - DEFINITION OF THE CARBONATE TRAJECTORIES IN MOOREA #
###############################################################

## Adding uncertainties in our predictions
# Opening document from a same folder
files_names <- list.files(here::here("data/Predictions"))
nb_files <- length(files_names) ; data_names <- vector("list",length=nb_files) ; dataset_list <- vector("list",length=nb_files)
for (i in 1:nb_files) {data_names[i] <- strsplit(files_names[i], split=".xls")}
for (i in 1:nb_files) {assign(data_names[[i]], read.delim2(paste(getwd(),"/Predictions/",files_names[i], sep ="")))}
for (i in 1:nb_files) {dataset_list[[i]] <- get(data_names[[i]])}
# Build 3 * 100 datasets according to species
ACR_SNS = vector(mode="list", length=nb_files) ; POC_SNS = vector(mode="list", length=nb_files) ; POR_SNS = vector(mode="list", length=nb_files)
for (i in 1:nb_files){ 
  ACR_SNS[[i]] <- dataset_list[[i]][dataset_list[[i]]$Species=="ACR",]
  POC_SNS[[i]] <- dataset_list[[i]][dataset_list[[i]]$Species=="POC",]
  POR_SNS[[i]] <- dataset_list[[i]][dataset_list[[i]]$Species=="POR",]}

## Definition of calcification production according to both models
## Predict Metabolism trajectories for Acropora
Pred_alo_calcif_ACR = vector(mode="list", length=nb_files) ; Pred_iso_calcif_ACR = vector(mode="list", length=nb_files) 
for (i in 1:nb_files){ 
  Pred_alo_calcif_ACR[[i]] <- predict(model_alo_Calcif[[1]], newdata = ACR_SNS[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
  Pred_iso_calcif_ACR[[i]] <- predict(model_iso_Calcif[[1]], newdata = ACR_SNS[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
}
Pred_alo_calcif_ACR = abind::abind(Pred_alo_calcif_ACR, along=1)
Pred_iso_calcif_ACR = abind::abind(Pred_iso_calcif_ACR, along=1)
# Merging the new Acropora table
size_iter = vector("list", length=nb_files) ; for (i in 1:nb_files) {size_iter[[i]] = dim(ACR_SNS[[i]])[1]}
vector_Size = vector("list", length=nb_files) ; for (i in 1:nb_files) {vector_Size[[i]] = rep(i,size_iter[[i]])}
Size_vector_Iter = vector_Size[[1]] ; for (i in 2:nb_files){Size_vector_Iter <- c(Size_vector_Iter, vector_Size[[i]])}
complete_data_ACR <- ACR_SNS[[1]] ; for (i in 2:nb_files) {complete_data_ACR <- rbind(complete_data_ACR, ACR_SNS[[i]])}
complete_data_ACR = cbind(complete_data_ACR, Pred_alo_calcif_ACR[,1], Pred_iso_calcif_ACR[,1], Size_vector_Iter)
colnames(complete_data_ACR) = c("Year", "Species", "Site", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area",
                                "Surface_Area", "Predict_Calcif_Alo", "Predict_Calcif_Iso", "nb_iter")
## Predict Metabolism trajectories for Pocillopora
Pred_alo_calcif_POC = vector(mode="list", length=nb_files) ; Pred_iso_calcif_POC = vector(mode="list", length=nb_files) 
for (i in 1:nb_files){ 
  Pred_alo_calcif_POC[[i]] <- predict(model_alo_Calcif[[2]], newdata = POC_SNS[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
  Pred_iso_calcif_POC[[i]] <- predict(model_iso_Calcif[[2]], newdata = POC_SNS[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
}
Pred_alo_calcif_POC = abind::abind(Pred_alo_calcif_POC, along=1)
Pred_iso_calcif_POC = abind::abind(Pred_iso_calcif_POC, along=1)
# Merging the new Pocillopora table
size_iter = vector("list", length=nb_files) ; for (i in 1:nb_files) {size_iter[[i]] = dim(POC_SNS[[i]])[1]}
vector_Size = vector("list", length=nb_files) ; for (i in 1:nb_files) {vector_Size[[i]] = rep(i,size_iter[[i]])}
Size_vector_Iter = vector_Size[[1]] ; for (i in 2:nb_files){Size_vector_Iter <- c(Size_vector_Iter, vector_Size[[i]])}
complete_data_POC <- POC_SNS[[1]] ; for (i in 2:nb_files) {complete_data_POC <- rbind(complete_data_POC, POC_SNS[[i]])}
complete_data_POC = cbind(complete_data_POC, Pred_alo_calcif_POC[,1], Pred_iso_calcif_POC[,1], Size_vector_Iter)
colnames(complete_data_POC) = c("Year", "Species", "Site", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area",
                                "Surface_Area", "Predict_Calcif_Alo", "Predict_Calcif_Iso", "nb_iter")
## Predict Metabolism trajectories for Porites 
Pred_alo_calcif_POR = vector(mode="list", length=nb_files) ; Pred_iso_calcif_POR = vector(mode="list", length=nb_files) 
for (i in 1:nb_files){ 
  Pred_alo_calcif_POR[[i]] <- predict(model_alo_Calcif[[3]], newdata = POR_SNS[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
  Pred_iso_calcif_POR[[i]] <- predict(model_iso_Calcif[[3]], newdata = POR_SNS[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5)
}
Pred_alo_calcif_POR = abind::abind(Pred_alo_calcif_POR, along=1)
Pred_iso_calcif_POR = abind::abind(Pred_iso_calcif_POR, along=1)
# Merging the new Porites table
size_iter = vector("list", length=nb_files) ; for (i in 1:nb_files) {size_iter[[i]] = dim(POR_SNS[[i]])[1]}
vector_Size = vector("list", length=nb_files) ; for (i in 1:nb_files) {vector_Size[[i]] = rep(i,size_iter[[i]])}
Size_vector_Iter = vector_Size[[1]] ; for (i in 2:nb_files){Size_vector_Iter <- c(Size_vector_Iter, vector_Size[[i]])}
complete_data_POR <- POR_SNS[[1]] ; for (i in 2:nb_files) {complete_data_POR <- rbind(complete_data_POR, POR_SNS[[i]])}
complete_data_POR = cbind(complete_data_POR, Pred_alo_calcif_POR[,1], Pred_iso_calcif_POR[,1], Size_vector_Iter)
colnames(complete_data_POR) = c("Year", "Species", "Site", "X", "Y", "Z", "Mean_Diam", "Bidimensionnal_Area",
                                "Surface_Area", "Predict_Calcif_Alo", "Predict_Calcif_Iso",  "nb_iter")

## Final Table
data_SNS = rbind(data.frame(complete_data_ACR), data.frame(complete_data_POC), data.frame(complete_data_POR))
data_SNS$Ind = rep(1, length(data_SNS$Year))
data_SNS$Year = format(as.Date(data_SNS$Year, format="%d.%m.%Y"),"%Y")
# Aggregating by year
Final_Table_Year <- aggregate(cbind(Surface_Area/100,Predict_Calcif_Iso/100,Predict_Calcif_Alo/100,
                                   Mean_Diam/100, Ind/100)~Year, data = data_SNS, FUN = sum)
colnames(Final_Table_Year) = c("Year","Surface_Area","CaCO3_iso","CaCO3_alo","Mean_Diam","Nb_Ind")
Final_Table_Year$Mean_Diam = Final_Table_Year$Mean_Diam/Final_Table_Year$Nb_Ind
Final_Table_Year$Cumsum_CaCO3_iso = cumsum(Final_Table_Year$CaCO3_iso) ; Final_Table_Year$Cumsum_CaCO3_alo = cumsum(Final_Table_Year$CaCO3_alo)
# Managing dataset for ggplot
Table_Plot_Year = data.frame(Year = rep(Final_Table_Year$Year,2), 
                             Production_CaCO3 = c(Final_Table_Year$CaCO3_iso, Final_Table_Year$CaCO3_alo), 
                             Surface_Area = rep(Final_Table_Year$Surface_Area,2), 
                             Metabolism = c(rep("Isometric",10),rep("Allometric",10)),
                             Cumsum_CaCO3 = c(Final_Table_Year$Cumsum_CaCO3_iso, Final_Table_Year$Cumsum_CaCO3_alo))
# Geom_error_bar - Production 
Final_Table_error <- aggregate(cbind(Surface_Area,Predict_Calcif_Iso,Predict_Calcif_Alo,
                                     Mean_Diam, Ind)~Year+nb_iter, data = data_SNS, FUN = sum)
colnames(Final_Table_error) = c("Year","nb_iter","Surface_Area","CaCO3_iso","CaCO3_alo","Mean_Diam","Nb_Ind")
Final_Table_error$csum_CaCO3_alo <- ave(Final_Table_error$CaCO3_alo, Final_Table_error$nb_iter, FUN=cumsum)
Final_Table_error$csum_CaCO3_iso <- ave(Final_Table_error$CaCO3_iso, Final_Table_error$nb_iter, FUN=cumsum)
mean_Pred_Tot = aggregate(data = Final_Table_error, cbind(csum_CaCO3_iso,csum_CaCO3_alo) ~ Year, FUN = mean)
sd_Pred_Tot   = aggregate(data = Final_Table_error, cbind(csum_CaCO3_iso,csum_CaCO3_alo) ~ Year, FUN = sd  )
table_errorbar = data.frame(Year = rep(Final_Table_Year$Year,2), 
                            sd_min_CaCO3 = c(mean_Pred_Tot[,2]-sd_Pred_Tot[,2], mean_Pred_Tot[,3]-sd_Pred_Tot[,3]),
                            Cumsum_CaCO3 = c(Final_Table_error$csum_CaCO3_iso,Final_Table_error$csum_CaCO3_alo),
                            sd_max_CaCO3 = c(mean_Pred_Tot[,2]+sd_Pred_Tot[,2], mean_Pred_Tot[,3]+sd_Pred_Tot[,3]),
                            Metabolism = c(rep("Isometric",10),rep("Allometric",10)))
# Polygon building for the confident interval - Production
Table_Plot_Year$Year = rep(c(2005,2008:2016),2) ; table_errorbar$Year = rep(c(2005,2008:2016),100)
Geom_Confint_Up = aggregate(sd_max_CaCO3~Year + Metabolism, data = table_errorbar, FUN = mean)
Geom_Confint_Dw = aggregate(sd_min_CaCO3~Year + Metabolism, data = table_errorbar, FUN = mean)
Geom_Confint <- data.frame(Pred = c(Geom_Confint_Up[,3]/1000,rev(Geom_Confint_Dw[,3]/1000)), 
                           Year = c(Geom_Confint_Dw$Year,rev(Geom_Confint_Dw$Year)),
                           Metabolism = c(Geom_Confint_Dw$Metabolism,rev(Geom_Confint_Dw$Metabolism)))
Geom_Confint$Metabolism[Geom_Confint$Metabolism == 1] <- "Allometric" ; Geom_Confint$Metabolism[Geom_Confint$Metabolism == 2] <- "Isometric"
# Figure Cumulative Carbonate production
(Fig_4A = ggplot(data = Table_Plot_Year, aes(x = Year, y = Cumsum_CaCO3/1000, col = Metabolism)) + geom_line(size = 1) +
    scale_color_manual(values=c("royalblue3", "orange")) + theme(legend.position='none') + scale_fill_manual(values=c("royalblue3", "orange")) +
    scale_x_continuous(name="") + scale_y_continuous(expression("Cumulative" ~ CaCO[3] ~ "production for a 10" * m^2 ~ "standard transect (kg." * yr^-1 * ")")) +
    geom_polygon(data=Geom_Confint, mapping=aes(x=Year, y=Pred, fill = Metabolism), alpha = 0.4))
# Geom_error_bar - Productivity
mean_Pred_Tot = aggregate(data = Final_Table_error, cbind(CaCO3_iso/Surface_Area,CaCO3_alo/Surface_Area) ~ Year, FUN = mean)
sd_Pred_Tot   = aggregate(data = Final_Table_error, cbind(CaCO3_iso/Surface_Area,CaCO3_alo/Surface_Area) ~ Year, FUN = sd  )
table_errorbar = data.frame(Year = rep(Final_Table_Year$Year,2), 
                            sd_min_CaCO3 = c(mean_Pred_Tot[,2]-sd_Pred_Tot[,2], mean_Pred_Tot[,3]-sd_Pred_Tot[,3]),
                            Productivity_CaCO3 = c(Final_Table_Year$CaCO3_iso/Final_Table_Year$Surface_Area,
                                                   Final_Table_Year$CaCO3_alo/Final_Table_Year$Surface_Area),
                            sd_max_CaCO3 = c(mean_Pred_Tot[,2]+sd_Pred_Tot[,2], mean_Pred_Tot[,3]+sd_Pred_Tot[,3]),
                            Metabolism = c(rep("Isometric",10),rep("Allometric",10)))
table_errorbar$Year = rep(c(2005,2008:2016),2)
# Polygon building for the confident interval - Productivity
Geom_Confint_Up = aggregate(sd_max_CaCO3~Year + Metabolism, data = table_errorbar, FUN = mean)
Geom_Confint_Dw = aggregate(sd_min_CaCO3~Year + Metabolism, data = table_errorbar, FUN = mean)
Geom_Confint <- data.frame(Pred = c(Geom_Confint_Up[,3]*10,rev(Geom_Confint_Dw[,3]*10)), # We add a x10 'cause we divided by the Surface area corresponding to 10m2
                           Year = c(Geom_Confint_Dw$Year,rev(Geom_Confint_Dw$Year)),
                           Metabolism = c(Geom_Confint_Dw$Metabolism,rev(Geom_Confint_Dw$Metabolism)))
Geom_Confint$Metabolism[Geom_Confint$Metabolism == 1] <- "Allometric" ; Geom_Confint$Metabolism[Geom_Confint$Metabolism == 2] <- "Isometric"
# Figure Carbonate Productivity
(Fig_4B = ggplot(data = table_errorbar, aes(x = Year, y = Productivity_CaCO3*10, col = Metabolism)) + geom_line(size = 1) +
    scale_color_manual(values=c("royalblue3", "orange")) + theme(legend.position='none') + scale_fill_manual(values=c("royalblue3", "orange")) +
    scale_x_continuous(name="") + scale_y_continuous(expression("Calcification Productivity (kg." * m^-2 * "." * yr^-1 *")")) +
    geom_polygon(data=Geom_Confint, mapping=aes(x=Year, y=Pred, fill = Metabolism), alpha = 0.4))

######################
# VALORISING RESULTS #
######################

export::graph2eps(Fig_2 , file = "Results/Figure_2.esp",  cairo = TRUE)
export::graph2eps(Fig_3a, file = "Results/Figure_3a.esp", cairo = TRUE)
export::graph2eps(Fig_3b, file = "Results/Figure_3b.esp", cairo = TRUE)
export::graph2eps(Fig_4a, file = "Results/Figure_3a.esp", cairo = TRUE)
export::graph2eps(Fig_4b, file = "/Results/Figure_3b.esp", cairo = TRUE)