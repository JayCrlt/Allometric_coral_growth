library(readxl) ; library(ggplot2)
R_100 <- read_excel("Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "RECRUIT_100%") ; R_100$Year = as.factor(R_100$Year)
R_75 <- read_excel("Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "RECRUIT_75%") ; R_75$Year = as.factor(R_75$Year)
R_50 <- read_excel("Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "RECRUIT_50%") ; R_50$Year = as.factor(R_50$Year)
R_25 <- read_excel("Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "RECRUIT_25%") ; R_25$Year = as.factor(R_25$Year)
Recruits <- read_excel("Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "Nb_Colonies")

# Scenario Recruitment normal
R_100_ACR = subset(R_100, subset = Species == "ACR") ; Acr_Recruits = round(Recruits$Nb_Colony[01:06],0) ; New_Pop_Acr_100 = c()
R_100_POC = subset(R_100, subset = Species == "POC") ; Poc_Recruits = round(Recruits$Nb_Colony[07:12],0) ; New_Pop_Poc_100 = c()
R_100_POR = subset(R_100, subset = Species == "POR") ; Por_Recruits = round(Recruits$Nb_Colony[13:18],0) ; New_Pop_Por_100 = c()
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_100_ACR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_100_ACR[R_100_ACR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Acr_100[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Acr_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_100_POC$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_100_POC[R_100_POC$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Poc_100[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Poc_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_100_POR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_100_POR[R_100_POR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Por_100[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Por_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}

# Scenario Recruitment x0.75
R_75_ACR = subset(R_75, subset = Species == "ACR") ; Acr_Recruits = round(Recruits$Nb_Colony[19:24],0) ; New_Pop_Acr_75 = c()
R_75_POC = subset(R_75, subset = Species == "POC") ; Poc_Recruits = round(Recruits$Nb_Colony[25:30],0) ; New_Pop_Poc_75 = c()
R_75_POR = subset(R_75, subset = Species == "POR") ; Por_Recruits = round(Recruits$Nb_Colony[31:36],0) ; New_Pop_Por_75 = c()
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_75_ACR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_75_ACR[R_75_ACR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Acr_75[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Acr_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_75_POC$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_75_POC[R_75_POC$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Poc_75[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Poc_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_75_POR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_75_POR[R_75_POR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Por_75[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Por_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}

# Scenario Recruitment x0.50
R_50_ACR = subset(R_50, subset = Species == "ACR") ; Acr_Recruits = round(Recruits$Nb_Colony[37:42],0) ; New_Pop_Acr_50 = c()
R_50_POC = subset(R_50, subset = Species == "POC") ; Poc_Recruits = round(Recruits$Nb_Colony[43:48],0) ; New_Pop_Poc_50 = c()
R_50_POR = subset(R_50, subset = Species == "POR") ; Por_Recruits = round(Recruits$Nb_Colony[49:54],0) ; New_Pop_Por_50 = c()
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_50_ACR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_50_ACR[R_50_ACR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Acr_50[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Acr_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_50_POC$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_50_POC[R_50_POC$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Poc_50[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Poc_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_50_POR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_50_POR[R_50_POR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Por_50[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Por_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}

# Scenario Recruitment x0.25
R_25_ACR = subset(R_25, subset = Species == "ACR") ; Acr_Recruits = round(Recruits$Nb_Colony[55:60],0) ; New_Pop_Acr_25 = c()
R_25_POC = subset(R_25, subset = Species == "POC") ; Poc_Recruits = round(Recruits$Nb_Colony[61:66],0) ; New_Pop_Poc_25 = c()
R_25_POR = subset(R_25, subset = Species == "POR") ; Por_Recruits = round(Recruits$Nb_Colony[67:72],0) ; New_Pop_Por_25 = c()
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_25_ACR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_25_ACR[R_25_ACR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Acr_25[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Acr_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_25_POC$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_25_POC[R_25_POC$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Poc_25[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Poc_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}
sub <- vector(mode="list", length = 6) ; IDs<-unique(R_25_POR$Year) ; for (i in 1:length(IDs)){ sub[[i]] <- R_25_POR[R_25_POR$Year==IDs[i],]}
for (i in 1:6) {New_Pop_Por_25[[i]] = replicate(100,sample(x=sub[[i]]$Surface_Area, Por_Recruits[[i]], replace=T, prob=sub[[i]]$Proba))}

# Load Calcification Model
load("/Data/Models/model_alo_Calcif.RData")

## Predict Metabolism trajectories for Acropora w/ scenario R=100
Pred_alo_calcif_ACR_100_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_100_2011 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_100_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_100_2013 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_100_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_100_2015 = vector(mode="list", length=100)
New_Pop_Acr_100_2010 = vector(mode = "list", length = 100) ; New_Pop_Acr_100_2011 = vector(mode = "list", length = 100)
New_Pop_Acr_100_2012 = vector(mode = "list", length = 100) ; New_Pop_Acr_100_2013 = vector(mode = "list", length = 100)
New_Pop_Acr_100_2014 = vector(mode = "list", length = 100) ; New_Pop_Acr_100_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Acr_100_2010 = vector(mode="list", length=100) ; Sum_Calcif_Acr_100_2011 = vector(mode="list", length=100)
Sum_Calcif_Acr_100_2012 = vector(mode="list", length=100) ; Sum_Calcif_Acr_100_2013 = vector(mode="list", length=100)
Sum_Calcif_Acr_100_2014 = vector(mode="list", length=100) ; Sum_Calcif_Acr_100_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Acr_100_2010[[i]] = data.frame(Surface_Area = New_Pop_Acr_100[[1]][,i])
  New_Pop_Acr_100_2011[[i]] = data.frame(Surface_Area = New_Pop_Acr_100[[2]][,i])
  New_Pop_Acr_100_2012[[i]] = data.frame(Surface_Area = New_Pop_Acr_100[[3]][,i])
  New_Pop_Acr_100_2013[[i]] = data.frame(Surface_Area = New_Pop_Acr_100[[4]][,i])
  New_Pop_Acr_100_2014[[i]] = data.frame(Surface_Area = New_Pop_Acr_100[[5]][,i])
  New_Pop_Acr_100_2015[[i]] = data.frame(Surface_Area = New_Pop_Acr_100[[6]][,i])
# Hindcast models
  Pred_alo_calcif_ACR_100_2010[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_100_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_100_2011[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_100_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_100_2012[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_100_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_100_2013[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_100_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_100_2014[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_100_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_100_2015[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_100_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
# Sum the 100 iterations
  Sum_Calcif_Acr_100_2010[[i]] = sum(Pred_alo_calcif_ACR_100_2010[[i]][,1])
  Sum_Calcif_Acr_100_2011[[i]] = sum(Pred_alo_calcif_ACR_100_2011[[i]][,1])
  Sum_Calcif_Acr_100_2012[[i]] = sum(Pred_alo_calcif_ACR_100_2012[[i]][,1])
  Sum_Calcif_Acr_100_2013[[i]] = sum(Pred_alo_calcif_ACR_100_2013[[i]][,1])
  Sum_Calcif_Acr_100_2014[[i]] = sum(Pred_alo_calcif_ACR_100_2014[[i]][,1])
  Sum_Calcif_Acr_100_2015[[i]] = sum(Pred_alo_calcif_ACR_100_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Acr_100_2010 = abind::abind(Sum_Calcif_Acr_100_2010, along=1)
Sum_Calcif_Acr_100_2011 = abind::abind(Sum_Calcif_Acr_100_2011, along=1)
Sum_Calcif_Acr_100_2012 = abind::abind(Sum_Calcif_Acr_100_2012, along=1)
Sum_Calcif_Acr_100_2013 = abind::abind(Sum_Calcif_Acr_100_2013, along=1)
Sum_Calcif_Acr_100_2014 = abind::abind(Sum_Calcif_Acr_100_2014, along=1)
Sum_Calcif_Acr_100_2015 = abind::abind(Sum_Calcif_Acr_100_2015, along=1)

## Predict Metabolism trajectories for Pocillopora w/ scenario R=100
Pred_alo_calcif_POC_100_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_100_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POC_100_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_100_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POC_100_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_100_2015 = vector(mode="list", length=100)
New_Pop_Poc_100_2010 = vector(mode = "list", length = 100) ; New_Pop_Poc_100_2011 = vector(mode = "list", length = 100)
New_Pop_Poc_100_2012 = vector(mode = "list", length = 100) ; New_Pop_Poc_100_2013 = vector(mode = "list", length = 100)
New_Pop_Poc_100_2014 = vector(mode = "list", length = 100) ; New_Pop_Poc_100_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Poc_100_2010 = vector(mode="list", length=100) ; Sum_Calcif_Poc_100_2011 = vector(mode="list", length=100)
Sum_Calcif_Poc_100_2012 = vector(mode="list", length=100) ; Sum_Calcif_Poc_100_2013 = vector(mode="list", length=100)
Sum_Calcif_Poc_100_2014 = vector(mode="list", length=100) ; Sum_Calcif_Poc_100_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Poc_100_2010[[i]] = data.frame(Surface_Area = New_Pop_Poc_100[[1]][,i])
  New_Pop_Poc_100_2011[[i]] = data.frame(Surface_Area = New_Pop_Poc_100[[2]][,i])
  New_Pop_Poc_100_2012[[i]] = data.frame(Surface_Area = New_Pop_Poc_100[[3]][,i])
  New_Pop_Poc_100_2013[[i]] = data.frame(Surface_Area = New_Pop_Poc_100[[4]][,i])
  New_Pop_Poc_100_2014[[i]] = data.frame(Surface_Area = New_Pop_Poc_100[[5]][,i])
  New_Pop_Poc_100_2015[[i]] = data.frame(Surface_Area = New_Pop_Poc_100[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POC_100_2010[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_100_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_100_2011[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_100_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_100_2012[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_100_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_100_2013[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_100_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_100_2014[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_100_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_100_2015[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_100_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Poc_100_2010[[i]] = sum(Pred_alo_calcif_POC_100_2010[[i]][,1])
  Sum_Calcif_Poc_100_2011[[i]] = sum(Pred_alo_calcif_POC_100_2011[[i]][,1])
  Sum_Calcif_Poc_100_2012[[i]] = sum(Pred_alo_calcif_POC_100_2012[[i]][,1])
  Sum_Calcif_Poc_100_2013[[i]] = sum(Pred_alo_calcif_POC_100_2013[[i]][,1])
  Sum_Calcif_Poc_100_2014[[i]] = sum(Pred_alo_calcif_POC_100_2014[[i]][,1])
  Sum_Calcif_Poc_100_2015[[i]] = sum(Pred_alo_calcif_POC_100_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Poc_100_2010 = abind::abind(Sum_Calcif_Poc_100_2010, along=1)
Sum_Calcif_Poc_100_2011 = abind::abind(Sum_Calcif_Poc_100_2011, along=1)
Sum_Calcif_Poc_100_2012 = abind::abind(Sum_Calcif_Poc_100_2012, along=1)
Sum_Calcif_Poc_100_2013 = abind::abind(Sum_Calcif_Poc_100_2013, along=1)
Sum_Calcif_Poc_100_2014 = abind::abind(Sum_Calcif_Poc_100_2014, along=1)
Sum_Calcif_Poc_100_2015 = abind::abind(Sum_Calcif_Poc_100_2015, along=1)

## Predict Metabolism trajectories for Porites w/ scenario R=100
Pred_alo_calcif_POR_100_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_100_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POR_100_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_100_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POR_100_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_100_2015 = vector(mode="list", length=100)
New_Pop_Por_100_2010 = vector(mode = "list", length = 100) ; New_Pop_Por_100_2011 = vector(mode = "list", length = 100)
New_Pop_Por_100_2012 = vector(mode = "list", length = 100) ; New_Pop_Por_100_2013 = vector(mode = "list", length = 100)
New_Pop_Por_100_2014 = vector(mode = "list", length = 100) ; New_Pop_Por_100_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Por_100_2010 = vector(mode="list", length=100) ; Sum_Calcif_Por_100_2011 = vector(mode="list", length=100)
Sum_Calcif_Por_100_2012 = vector(mode="list", length=100) ; Sum_Calcif_Por_100_2013 = vector(mode="list", length=100)
Sum_Calcif_Por_100_2014 = vector(mode="list", length=100) ; Sum_Calcif_Por_100_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Por_100_2010[[i]] = data.frame(Surface_Area = New_Pop_Por_100[[1]][,i])
  New_Pop_Por_100_2011[[i]] = data.frame(Surface_Area = New_Pop_Por_100[[2]][,i])
  New_Pop_Por_100_2012[[i]] = data.frame(Surface_Area = New_Pop_Por_100[[3]][,i])
  New_Pop_Por_100_2013[[i]] = data.frame(Surface_Area = New_Pop_Por_100[[4]][,i])
  New_Pop_Por_100_2014[[i]] = data.frame(Surface_Area = New_Pop_Por_100[[5]][,i])
  New_Pop_Por_100_2015[[i]] = data.frame(Surface_Area = New_Pop_Por_100[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POR_100_2010[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_100_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_100_2011[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_100_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_100_2012[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_100_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_100_2013[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_100_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_100_2014[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_100_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_100_2015[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_100_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Por_100_2010[[i]] = sum(Pred_alo_calcif_POR_100_2010[[i]][,1])
  Sum_Calcif_Por_100_2011[[i]] = sum(Pred_alo_calcif_POR_100_2011[[i]][,1])
  Sum_Calcif_Por_100_2012[[i]] = sum(Pred_alo_calcif_POR_100_2012[[i]][,1])
  Sum_Calcif_Por_100_2013[[i]] = sum(Pred_alo_calcif_POR_100_2013[[i]][,1])
  Sum_Calcif_Por_100_2014[[i]] = sum(Pred_alo_calcif_POR_100_2014[[i]][,1])
  Sum_Calcif_Por_100_2015[[i]] = sum(Pred_alo_calcif_POR_100_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Por_100_2010 = abind::abind(Sum_Calcif_Por_100_2010, along=1)
Sum_Calcif_Por_100_2011 = abind::abind(Sum_Calcif_Por_100_2011, along=1)
Sum_Calcif_Por_100_2012 = abind::abind(Sum_Calcif_Por_100_2012, along=1)
Sum_Calcif_Por_100_2013 = abind::abind(Sum_Calcif_Por_100_2013, along=1)
Sum_Calcif_Por_100_2014 = abind::abind(Sum_Calcif_Por_100_2014, along=1)
Sum_Calcif_Por_100_2015 = abind::abind(Sum_Calcif_Por_100_2015, along=1)

## Define the final table R=100
R_100_2010 = c() ; R_100_2011 = c() ; R_100_2012 = c() ; R_100_2013 = c() ; R_100_2014 = c() ; R_100_2015 = c()
for (i in 1:100){
  R_100_2010[[i]] = sum(Sum_Calcif_Acr_100_2010[i],Sum_Calcif_Poc_100_2010[i],Sum_Calcif_Por_100_2010[i])
  R_100_2011[[i]] = sum(Sum_Calcif_Acr_100_2011[i],Sum_Calcif_Poc_100_2011[i],Sum_Calcif_Por_100_2011[i])
  R_100_2012[[i]] = sum(Sum_Calcif_Acr_100_2012[i],Sum_Calcif_Poc_100_2012[i],Sum_Calcif_Por_100_2012[i])
  R_100_2013[[i]] = sum(Sum_Calcif_Acr_100_2013[i],Sum_Calcif_Poc_100_2013[i],Sum_Calcif_Por_100_2013[i])
  R_100_2014[[i]] = sum(Sum_Calcif_Acr_100_2014[i],Sum_Calcif_Poc_100_2014[i],Sum_Calcif_Por_100_2014[i])
  R_100_2015[[i]] = sum(Sum_Calcif_Acr_100_2015[i],Sum_Calcif_Poc_100_2015[i],Sum_Calcif_Por_100_2015[i])
}
# Final_Table R_100
R_100_Final = data.frame(Year = seq(2010,2015,1),
                         Calcif_Mean = c(mean(R_100_2010/10000),mean(R_100_2011/10000),mean(R_100_2012/10000),
                                         mean(R_100_2013/10000),mean(R_100_2014/10000),mean(R_100_2015/10000)), 
                         Calcif_sd = c(sd(R_100_2010/10000),sd(R_100_2011/10000),sd(R_100_2012/10000),
                                       sd(R_100_2013/10000),sd(R_100_2014/10000),sd(R_100_2015/10000)),
                         Nb_Colonies = c(sum(Recruits$Nb_Colony[01], Recruits$Nb_Colony[07], Recruits$Nb_Colony[13]),
                                         sum(Recruits$Nb_Colony[02], Recruits$Nb_Colony[08], Recruits$Nb_Colony[14]),
                                         sum(Recruits$Nb_Colony[03], Recruits$Nb_Colony[09], Recruits$Nb_Colony[15]),
                                         sum(Recruits$Nb_Colony[04], Recruits$Nb_Colony[10], Recruits$Nb_Colony[16]),
                                         sum(Recruits$Nb_Colony[05], Recruits$Nb_Colony[11], Recruits$Nb_Colony[17]),
                                         sum(Recruits$Nb_Colony[06], Recruits$Nb_Colony[12], Recruits$Nb_Colony[18])))
# Saving
write.table(R_100_Final, "~/Desktop/R_100_Final.xls", dec = ",", sep="\t", row.names = F)

## Predict Metabolism trajectories for Acropora w/ scenario R=100
Pred_alo_calcif_ACR_75_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_75_2011 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_75_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_75_2013 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_75_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_75_2015 = vector(mode="list", length=100)
New_Pop_Acr_75_2010 = vector(mode = "list", length = 100) ; New_Pop_Acr_75_2011 = vector(mode = "list", length = 100)
New_Pop_Acr_75_2012 = vector(mode = "list", length = 100) ; New_Pop_Acr_75_2013 = vector(mode = "list", length = 100)
New_Pop_Acr_75_2014 = vector(mode = "list", length = 100) ; New_Pop_Acr_75_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Acr_75_2010 = vector(mode="list", length=100) ; Sum_Calcif_Acr_75_2011 = vector(mode="list", length=100)
Sum_Calcif_Acr_75_2012 = vector(mode="list", length=100) ; Sum_Calcif_Acr_75_2013 = vector(mode="list", length=100)
Sum_Calcif_Acr_75_2014 = vector(mode="list", length=100) ; Sum_Calcif_Acr_75_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Acr_75_2010[[i]] = data.frame(Surface_Area = New_Pop_Acr_75[[1]][,i])
  New_Pop_Acr_75_2011[[i]] = data.frame(Surface_Area = New_Pop_Acr_75[[2]][,i])
  New_Pop_Acr_75_2012[[i]] = data.frame(Surface_Area = New_Pop_Acr_75[[3]][,i])
  New_Pop_Acr_75_2013[[i]] = data.frame(Surface_Area = New_Pop_Acr_75[[4]][,i])
  New_Pop_Acr_75_2014[[i]] = data.frame(Surface_Area = New_Pop_Acr_75[[5]][,i])
  New_Pop_Acr_75_2015[[i]] = data.frame(Surface_Area = New_Pop_Acr_75[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_ACR_75_2010[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_75_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_75_2011[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_75_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_75_2012[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_75_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_75_2013[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_75_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_75_2014[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_75_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_75_2015[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_75_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Acr_75_2010[[i]] = sum(Pred_alo_calcif_ACR_75_2010[[i]][,1])
  Sum_Calcif_Acr_75_2011[[i]] = sum(Pred_alo_calcif_ACR_75_2011[[i]][,1])
  Sum_Calcif_Acr_75_2012[[i]] = sum(Pred_alo_calcif_ACR_75_2012[[i]][,1])
  Sum_Calcif_Acr_75_2013[[i]] = sum(Pred_alo_calcif_ACR_75_2013[[i]][,1])
  Sum_Calcif_Acr_75_2014[[i]] = sum(Pred_alo_calcif_ACR_75_2014[[i]][,1])
  Sum_Calcif_Acr_75_2015[[i]] = sum(Pred_alo_calcif_ACR_75_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Acr_75_2010 = abind::abind(Sum_Calcif_Acr_75_2010, along=1)
Sum_Calcif_Acr_75_2011 = abind::abind(Sum_Calcif_Acr_75_2011, along=1)
Sum_Calcif_Acr_75_2012 = abind::abind(Sum_Calcif_Acr_75_2012, along=1)
Sum_Calcif_Acr_75_2013 = abind::abind(Sum_Calcif_Acr_75_2013, along=1)
Sum_Calcif_Acr_75_2014 = abind::abind(Sum_Calcif_Acr_75_2014, along=1)
Sum_Calcif_Acr_75_2015 = abind::abind(Sum_Calcif_Acr_75_2015, along=1)

## Predict Metabolism trajectories for Pocillopora w/ scenario R=100
Pred_alo_calcif_POC_75_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_75_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POC_75_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_75_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POC_75_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_75_2015 = vector(mode="list", length=100)
New_Pop_Poc_75_2010 = vector(mode = "list", length = 100) ; New_Pop_Poc_75_2011 = vector(mode = "list", length = 100)
New_Pop_Poc_75_2012 = vector(mode = "list", length = 100) ; New_Pop_Poc_75_2013 = vector(mode = "list", length = 100)
New_Pop_Poc_75_2014 = vector(mode = "list", length = 100) ; New_Pop_Poc_75_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Poc_75_2010 = vector(mode="list", length=100) ; Sum_Calcif_Poc_75_2011 = vector(mode="list", length=100)
Sum_Calcif_Poc_75_2012 = vector(mode="list", length=100) ; Sum_Calcif_Poc_75_2013 = vector(mode="list", length=100)
Sum_Calcif_Poc_75_2014 = vector(mode="list", length=100) ; Sum_Calcif_Poc_75_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Poc_75_2010[[i]] = data.frame(Surface_Area = New_Pop_Poc_75[[1]][,i])
  New_Pop_Poc_75_2011[[i]] = data.frame(Surface_Area = New_Pop_Poc_75[[2]][,i])
  New_Pop_Poc_75_2012[[i]] = data.frame(Surface_Area = New_Pop_Poc_75[[3]][,i])
  New_Pop_Poc_75_2013[[i]] = data.frame(Surface_Area = New_Pop_Poc_75[[4]][,i])
  New_Pop_Poc_75_2014[[i]] = data.frame(Surface_Area = New_Pop_Poc_75[[5]][,i])
  New_Pop_Poc_75_2015[[i]] = data.frame(Surface_Area = New_Pop_Poc_75[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POC_75_2010[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_75_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_75_2011[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_75_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_75_2012[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_75_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_75_2013[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_75_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_75_2014[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_75_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_75_2015[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_75_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Poc_75_2010[[i]] = sum(Pred_alo_calcif_POC_75_2010[[i]][,1])
  Sum_Calcif_Poc_75_2011[[i]] = sum(Pred_alo_calcif_POC_75_2011[[i]][,1])
  Sum_Calcif_Poc_75_2012[[i]] = sum(Pred_alo_calcif_POC_75_2012[[i]][,1])
  Sum_Calcif_Poc_75_2013[[i]] = sum(Pred_alo_calcif_POC_75_2013[[i]][,1])
  Sum_Calcif_Poc_75_2014[[i]] = sum(Pred_alo_calcif_POC_75_2014[[i]][,1])
  Sum_Calcif_Poc_75_2015[[i]] = sum(Pred_alo_calcif_POC_75_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Poc_75_2010 = abind::abind(Sum_Calcif_Poc_75_2010, along=1)
Sum_Calcif_Poc_75_2011 = abind::abind(Sum_Calcif_Poc_75_2011, along=1)
Sum_Calcif_Poc_75_2012 = abind::abind(Sum_Calcif_Poc_75_2012, along=1)
Sum_Calcif_Poc_75_2013 = abind::abind(Sum_Calcif_Poc_75_2013, along=1)
Sum_Calcif_Poc_75_2014 = abind::abind(Sum_Calcif_Poc_75_2014, along=1)
Sum_Calcif_Poc_75_2015 = abind::abind(Sum_Calcif_Poc_75_2015, along=1)

## Predict Metabolism trajectories for Porites w/ scenario R=100
Pred_alo_calcif_POR_75_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_75_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POR_75_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_75_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POR_75_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_75_2015 = vector(mode="list", length=100)
New_Pop_Por_75_2010 = vector(mode = "list", length = 100) ; New_Pop_Por_75_2011 = vector(mode = "list", length = 100)
New_Pop_Por_75_2012 = vector(mode = "list", length = 100) ; New_Pop_Por_75_2013 = vector(mode = "list", length = 100)
New_Pop_Por_75_2014 = vector(mode = "list", length = 100) ; New_Pop_Por_75_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Por_75_2010 = vector(mode="list", length=100) ; Sum_Calcif_Por_75_2011 = vector(mode="list", length=100)
Sum_Calcif_Por_75_2012 = vector(mode="list", length=100) ; Sum_Calcif_Por_75_2013 = vector(mode="list", length=100)
Sum_Calcif_Por_75_2014 = vector(mode="list", length=100) ; Sum_Calcif_Por_75_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Por_75_2010[[i]] = data.frame(Surface_Area = New_Pop_Por_75[[1]][,i])
  New_Pop_Por_75_2011[[i]] = data.frame(Surface_Area = New_Pop_Por_75[[2]][,i])
  New_Pop_Por_75_2012[[i]] = data.frame(Surface_Area = New_Pop_Por_75[[3]][,i])
  New_Pop_Por_75_2013[[i]] = data.frame(Surface_Area = New_Pop_Por_75[[4]][,i])
  New_Pop_Por_75_2014[[i]] = data.frame(Surface_Area = New_Pop_Por_75[[5]][,i])
  New_Pop_Por_75_2015[[i]] = data.frame(Surface_Area = New_Pop_Por_75[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POR_75_2010[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_75_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_75_2011[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_75_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_75_2012[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_75_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_75_2013[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_75_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_75_2014[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_75_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_75_2015[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_75_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Por_75_2010[[i]] = sum(Pred_alo_calcif_POR_75_2010[[i]][,1])
  Sum_Calcif_Por_75_2011[[i]] = sum(Pred_alo_calcif_POR_75_2011[[i]][,1])
  Sum_Calcif_Por_75_2012[[i]] = sum(Pred_alo_calcif_POR_75_2012[[i]][,1])
  Sum_Calcif_Por_75_2013[[i]] = sum(Pred_alo_calcif_POR_75_2013[[i]][,1])
  Sum_Calcif_Por_75_2014[[i]] = sum(Pred_alo_calcif_POR_75_2014[[i]][,1])
  Sum_Calcif_Por_75_2015[[i]] = sum(Pred_alo_calcif_POR_75_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Por_75_2010 = abind::abind(Sum_Calcif_Por_75_2010, along=1)
Sum_Calcif_Por_75_2011 = abind::abind(Sum_Calcif_Por_75_2011, along=1)
Sum_Calcif_Por_75_2012 = abind::abind(Sum_Calcif_Por_75_2012, along=1)
Sum_Calcif_Por_75_2013 = abind::abind(Sum_Calcif_Por_75_2013, along=1)
Sum_Calcif_Por_75_2014 = abind::abind(Sum_Calcif_Por_75_2014, along=1)
Sum_Calcif_Por_75_2015 = abind::abind(Sum_Calcif_Por_75_2015, along=1)

## Define the final table R=100
R_75_2010 = c() ; R_75_2011 = c() ; R_75_2012 = c() ; R_75_2013 = c() ; R_75_2014 = c() ; R_75_2015 = c()
for (i in 1:100){
  R_75_2010[[i]] = sum(Sum_Calcif_Acr_75_2010[i],Sum_Calcif_Poc_75_2010[i],Sum_Calcif_Por_75_2010[i])
  R_75_2011[[i]] = sum(Sum_Calcif_Acr_75_2011[i],Sum_Calcif_Poc_75_2011[i],Sum_Calcif_Por_75_2011[i])
  R_75_2012[[i]] = sum(Sum_Calcif_Acr_75_2012[i],Sum_Calcif_Poc_75_2012[i],Sum_Calcif_Por_75_2012[i])
  R_75_2013[[i]] = sum(Sum_Calcif_Acr_75_2013[i],Sum_Calcif_Poc_75_2013[i],Sum_Calcif_Por_75_2013[i])
  R_75_2014[[i]] = sum(Sum_Calcif_Acr_75_2014[i],Sum_Calcif_Poc_75_2014[i],Sum_Calcif_Por_75_2014[i])
  R_75_2015[[i]] = sum(Sum_Calcif_Acr_75_2015[i],Sum_Calcif_Poc_75_2015[i],Sum_Calcif_Por_75_2015[i])
}
# Final_Table R_75
R_75_Final = data.frame(Year = seq(2010,2015,1),
                        Calcif_Mean = c(mean(R_75_2010/10000),mean(R_75_2011/10000),mean(R_75_2012/10000),
                                        mean(R_75_2013/10000),mean(R_75_2014/10000),mean(R_75_2015/10000)), 
                        Calcif_sd = c(sd(R_75_2010/10000),sd(R_75_2011/10000),sd(R_75_2012/10000),
                                      sd(R_75_2013/10000),sd(R_75_2014/10000),sd(R_75_2015/10000)),
                        Nb_Colonies = c(sum(Recruits$Nb_Colony[01], Recruits$Nb_Colony[07], Recruits$Nb_Colony[13]),
                                        sum(Recruits$Nb_Colony[02], Recruits$Nb_Colony[08], Recruits$Nb_Colony[14]),
                                        sum(Recruits$Nb_Colony[03], Recruits$Nb_Colony[09], Recruits$Nb_Colony[15]),
                                        sum(Recruits$Nb_Colony[04], Recruits$Nb_Colony[10], Recruits$Nb_Colony[16]),
                                        sum(Recruits$Nb_Colony[05], Recruits$Nb_Colony[11], Recruits$Nb_Colony[17]),
                                        sum(Recruits$Nb_Colony[06], Recruits$Nb_Colony[12], Recruits$Nb_Colony[18])))
# Saving
write.table(R_75_Final, "~/Desktop/R_75_Final.xls", dec = ",", sep="\t", row.names = F)

## Predict Metabolism trajectories for Acropora w/ scenario R=100
Pred_alo_calcif_ACR_50_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_50_2011 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_50_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_50_2013 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_50_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_50_2015 = vector(mode="list", length=100)
New_Pop_Acr_50_2010 = vector(mode = "list", length = 100) ; New_Pop_Acr_50_2011 = vector(mode = "list", length = 100)
New_Pop_Acr_50_2012 = vector(mode = "list", length = 100) ; New_Pop_Acr_50_2013 = vector(mode = "list", length = 100)
New_Pop_Acr_50_2014 = vector(mode = "list", length = 100) ; New_Pop_Acr_50_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Acr_50_2010 = vector(mode="list", length=100) ; Sum_Calcif_Acr_50_2011 = vector(mode="list", length=100)
Sum_Calcif_Acr_50_2012 = vector(mode="list", length=100) ; Sum_Calcif_Acr_50_2013 = vector(mode="list", length=100)
Sum_Calcif_Acr_50_2014 = vector(mode="list", length=100) ; Sum_Calcif_Acr_50_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Acr_50_2010[[i]] = data.frame(Surface_Area = New_Pop_Acr_50[[1]][,i])
  New_Pop_Acr_50_2011[[i]] = data.frame(Surface_Area = New_Pop_Acr_50[[2]][,i])
  New_Pop_Acr_50_2012[[i]] = data.frame(Surface_Area = New_Pop_Acr_50[[3]][,i])
  New_Pop_Acr_50_2013[[i]] = data.frame(Surface_Area = New_Pop_Acr_50[[4]][,i])
  New_Pop_Acr_50_2014[[i]] = data.frame(Surface_Area = New_Pop_Acr_50[[5]][,i])
  New_Pop_Acr_50_2015[[i]] = data.frame(Surface_Area = New_Pop_Acr_50[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_ACR_50_2010[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_50_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_50_2011[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_50_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_50_2012[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_50_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_50_2013[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_50_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_50_2014[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_50_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_50_2015[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_50_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Acr_50_2010[[i]] = sum(Pred_alo_calcif_ACR_50_2010[[i]][,1])
  Sum_Calcif_Acr_50_2011[[i]] = sum(Pred_alo_calcif_ACR_50_2011[[i]][,1])
  Sum_Calcif_Acr_50_2012[[i]] = sum(Pred_alo_calcif_ACR_50_2012[[i]][,1])
  Sum_Calcif_Acr_50_2013[[i]] = sum(Pred_alo_calcif_ACR_50_2013[[i]][,1])
  Sum_Calcif_Acr_50_2014[[i]] = sum(Pred_alo_calcif_ACR_50_2014[[i]][,1])
  Sum_Calcif_Acr_50_2015[[i]] = sum(Pred_alo_calcif_ACR_50_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Acr_50_2010 = abind::abind(Sum_Calcif_Acr_50_2010, along=1)
Sum_Calcif_Acr_50_2011 = abind::abind(Sum_Calcif_Acr_50_2011, along=1)
Sum_Calcif_Acr_50_2012 = abind::abind(Sum_Calcif_Acr_50_2012, along=1)
Sum_Calcif_Acr_50_2013 = abind::abind(Sum_Calcif_Acr_50_2013, along=1)
Sum_Calcif_Acr_50_2014 = abind::abind(Sum_Calcif_Acr_50_2014, along=1)
Sum_Calcif_Acr_50_2015 = abind::abind(Sum_Calcif_Acr_50_2015, along=1)

## Predict Metabolism trajectories for Pocillopora w/ scenario R=100
Pred_alo_calcif_POC_50_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_50_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POC_50_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_50_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POC_50_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_50_2015 = vector(mode="list", length=100)
New_Pop_Poc_50_2010 = vector(mode = "list", length = 100) ; New_Pop_Poc_50_2011 = vector(mode = "list", length = 100)
New_Pop_Poc_50_2012 = vector(mode = "list", length = 100) ; New_Pop_Poc_50_2013 = vector(mode = "list", length = 100)
New_Pop_Poc_50_2014 = vector(mode = "list", length = 100) ; New_Pop_Poc_50_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Poc_50_2010 = vector(mode="list", length=100) ; Sum_Calcif_Poc_50_2011 = vector(mode="list", length=100)
Sum_Calcif_Poc_50_2012 = vector(mode="list", length=100) ; Sum_Calcif_Poc_50_2013 = vector(mode="list", length=100)
Sum_Calcif_Poc_50_2014 = vector(mode="list", length=100) ; Sum_Calcif_Poc_50_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Poc_50_2010[[i]] = data.frame(Surface_Area = New_Pop_Poc_50[[1]][,i])
  New_Pop_Poc_50_2011[[i]] = data.frame(Surface_Area = New_Pop_Poc_50[[2]][,i])
  New_Pop_Poc_50_2012[[i]] = data.frame(Surface_Area = New_Pop_Poc_50[[3]][,i])
  New_Pop_Poc_50_2013[[i]] = data.frame(Surface_Area = New_Pop_Poc_50[[4]][,i])
  New_Pop_Poc_50_2014[[i]] = data.frame(Surface_Area = New_Pop_Poc_50[[5]][,i])
  New_Pop_Poc_50_2015[[i]] = data.frame(Surface_Area = New_Pop_Poc_50[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POC_50_2010[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_50_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_50_2011[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_50_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_50_2012[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_50_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_50_2013[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_50_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_50_2014[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_50_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_50_2015[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_50_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Poc_50_2010[[i]] = sum(Pred_alo_calcif_POC_50_2010[[i]][,1])
  Sum_Calcif_Poc_50_2011[[i]] = sum(Pred_alo_calcif_POC_50_2011[[i]][,1])
  Sum_Calcif_Poc_50_2012[[i]] = sum(Pred_alo_calcif_POC_50_2012[[i]][,1])
  Sum_Calcif_Poc_50_2013[[i]] = sum(Pred_alo_calcif_POC_50_2013[[i]][,1])
  Sum_Calcif_Poc_50_2014[[i]] = sum(Pred_alo_calcif_POC_50_2014[[i]][,1])
  Sum_Calcif_Poc_50_2015[[i]] = sum(Pred_alo_calcif_POC_50_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Poc_50_2010 = abind::abind(Sum_Calcif_Poc_50_2010, along=1)
Sum_Calcif_Poc_50_2011 = abind::abind(Sum_Calcif_Poc_50_2011, along=1)
Sum_Calcif_Poc_50_2012 = abind::abind(Sum_Calcif_Poc_50_2012, along=1)
Sum_Calcif_Poc_50_2013 = abind::abind(Sum_Calcif_Poc_50_2013, along=1)
Sum_Calcif_Poc_50_2014 = abind::abind(Sum_Calcif_Poc_50_2014, along=1)
Sum_Calcif_Poc_50_2015 = abind::abind(Sum_Calcif_Poc_50_2015, along=1)

## Predict Metabolism trajectories for Porites w/ scenario R=100
Pred_alo_calcif_POR_50_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_50_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POR_50_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_50_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POR_50_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_50_2015 = vector(mode="list", length=100)
New_Pop_Por_50_2010 = vector(mode = "list", length = 100) ; New_Pop_Por_50_2011 = vector(mode = "list", length = 100)
New_Pop_Por_50_2012 = vector(mode = "list", length = 100) ; New_Pop_Por_50_2013 = vector(mode = "list", length = 100)
New_Pop_Por_50_2014 = vector(mode = "list", length = 100) ; New_Pop_Por_50_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Por_50_2010 = vector(mode="list", length=100) ; Sum_Calcif_Por_50_2011 = vector(mode="list", length=100)
Sum_Calcif_Por_50_2012 = vector(mode="list", length=100) ; Sum_Calcif_Por_50_2013 = vector(mode="list", length=100)
Sum_Calcif_Por_50_2014 = vector(mode="list", length=100) ; Sum_Calcif_Por_50_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Por_50_2010[[i]] = data.frame(Surface_Area = New_Pop_Por_50[[1]][,i])
  New_Pop_Por_50_2011[[i]] = data.frame(Surface_Area = New_Pop_Por_50[[2]][,i])
  New_Pop_Por_50_2012[[i]] = data.frame(Surface_Area = New_Pop_Por_50[[3]][,i])
  New_Pop_Por_50_2013[[i]] = data.frame(Surface_Area = New_Pop_Por_50[[4]][,i])
  New_Pop_Por_50_2014[[i]] = data.frame(Surface_Area = New_Pop_Por_50[[5]][,i])
  New_Pop_Por_50_2015[[i]] = data.frame(Surface_Area = New_Pop_Por_50[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POR_50_2010[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_50_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_50_2011[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_50_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_50_2012[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_50_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_50_2013[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_50_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_50_2014[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_50_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_50_2015[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_50_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Por_50_2010[[i]] = sum(Pred_alo_calcif_POR_50_2010[[i]][,1])
  Sum_Calcif_Por_50_2011[[i]] = sum(Pred_alo_calcif_POR_50_2011[[i]][,1])
  Sum_Calcif_Por_50_2012[[i]] = sum(Pred_alo_calcif_POR_50_2012[[i]][,1])
  Sum_Calcif_Por_50_2013[[i]] = sum(Pred_alo_calcif_POR_50_2013[[i]][,1])
  Sum_Calcif_Por_50_2014[[i]] = sum(Pred_alo_calcif_POR_50_2014[[i]][,1])
  Sum_Calcif_Por_50_2015[[i]] = sum(Pred_alo_calcif_POR_50_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Por_50_2010 = abind::abind(Sum_Calcif_Por_50_2010, along=1)
Sum_Calcif_Por_50_2011 = abind::abind(Sum_Calcif_Por_50_2011, along=1)
Sum_Calcif_Por_50_2012 = abind::abind(Sum_Calcif_Por_50_2012, along=1)
Sum_Calcif_Por_50_2013 = abind::abind(Sum_Calcif_Por_50_2013, along=1)
Sum_Calcif_Por_50_2014 = abind::abind(Sum_Calcif_Por_50_2014, along=1)
Sum_Calcif_Por_50_2015 = abind::abind(Sum_Calcif_Por_50_2015, along=1)

## Define the final table R=100
R_50_2010 = c() ; R_50_2011 = c() ; R_50_2012 = c() ; R_50_2013 = c() ; R_50_2014 = c() ; R_50_2015 = c()
for (i in 1:100){
  R_50_2010[[i]] = sum(Sum_Calcif_Acr_50_2010[i],Sum_Calcif_Poc_50_2010[i],Sum_Calcif_Por_50_2010[i])
  R_50_2011[[i]] = sum(Sum_Calcif_Acr_50_2011[i],Sum_Calcif_Poc_50_2011[i],Sum_Calcif_Por_50_2011[i])
  R_50_2012[[i]] = sum(Sum_Calcif_Acr_50_2012[i],Sum_Calcif_Poc_50_2012[i],Sum_Calcif_Por_50_2012[i])
  R_50_2013[[i]] = sum(Sum_Calcif_Acr_50_2013[i],Sum_Calcif_Poc_50_2013[i],Sum_Calcif_Por_50_2013[i])
  R_50_2014[[i]] = sum(Sum_Calcif_Acr_50_2014[i],Sum_Calcif_Poc_50_2014[i],Sum_Calcif_Por_50_2014[i])
  R_50_2015[[i]] = sum(Sum_Calcif_Acr_50_2015[i],Sum_Calcif_Poc_50_2015[i],Sum_Calcif_Por_50_2015[i])
}
# Final_Table R_50
R_50_Final = data.frame(Year = seq(2010,2015,1),
                        Calcif_Mean = c(mean(R_50_2010/10000),mean(R_50_2011/10000),mean(R_50_2012/10000),
                                        mean(R_50_2013/10000),mean(R_50_2014/10000),mean(R_50_2015/10000)), 
                        Calcif_sd = c(sd(R_50_2010/10000),sd(R_50_2011/10000),sd(R_50_2012/10000),
                                      sd(R_50_2013/10000),sd(R_50_2014/10000),sd(R_50_2015/10000)),
                        Nb_Colonies = c(sum(Recruits$Nb_Colony[01], Recruits$Nb_Colony[07], Recruits$Nb_Colony[13]),
                                        sum(Recruits$Nb_Colony[02], Recruits$Nb_Colony[08], Recruits$Nb_Colony[14]),
                                        sum(Recruits$Nb_Colony[03], Recruits$Nb_Colony[09], Recruits$Nb_Colony[15]),
                                        sum(Recruits$Nb_Colony[04], Recruits$Nb_Colony[10], Recruits$Nb_Colony[16]),
                                        sum(Recruits$Nb_Colony[05], Recruits$Nb_Colony[11], Recruits$Nb_Colony[17]),
                                        sum(Recruits$Nb_Colony[06], Recruits$Nb_Colony[12], Recruits$Nb_Colony[18])))
# Saving
write.table(R_50_Final, "~/Desktop/R_50_Final.xls", dec = ",", sep="\t", row.names = F)

## Predict Metabolism trajectories for Acropora w/ scenario R=100
Pred_alo_calcif_ACR_25_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_25_2011 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_25_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_25_2013 = vector(mode="list", length=100)
Pred_alo_calcif_ACR_25_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_ACR_25_2015 = vector(mode="list", length=100)
New_Pop_Acr_25_2010 = vector(mode = "list", length = 100) ; New_Pop_Acr_25_2011 = vector(mode = "list", length = 100)
New_Pop_Acr_25_2012 = vector(mode = "list", length = 100) ; New_Pop_Acr_25_2013 = vector(mode = "list", length = 100)
New_Pop_Acr_25_2014 = vector(mode = "list", length = 100) ; New_Pop_Acr_25_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Acr_25_2010 = vector(mode="list", length=100) ; Sum_Calcif_Acr_25_2011 = vector(mode="list", length=100)
Sum_Calcif_Acr_25_2012 = vector(mode="list", length=100) ; Sum_Calcif_Acr_25_2013 = vector(mode="list", length=100)
Sum_Calcif_Acr_25_2014 = vector(mode="list", length=100) ; Sum_Calcif_Acr_25_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Acr_25_2010[[i]] = data.frame(Surface_Area = New_Pop_Acr_25[[1]][,i])
  New_Pop_Acr_25_2011[[i]] = data.frame(Surface_Area = New_Pop_Acr_25[[2]][,i])
  New_Pop_Acr_25_2012[[i]] = data.frame(Surface_Area = New_Pop_Acr_25[[3]][,i])
  New_Pop_Acr_25_2013[[i]] = data.frame(Surface_Area = New_Pop_Acr_25[[4]][,i])
  New_Pop_Acr_25_2014[[i]] = data.frame(Surface_Area = New_Pop_Acr_25[[5]][,i])
  New_Pop_Acr_25_2015[[i]] = data.frame(Surface_Area = New_Pop_Acr_25[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_ACR_25_2010[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_25_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_25_2011[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_25_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_25_2012[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_25_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_25_2013[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_25_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_25_2014[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_25_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_ACR_25_2015[[i]] <- predict(model_alo_Calcif[[1]], newdata = New_Pop_Acr_25_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Acr_25_2010[[i]] = sum(Pred_alo_calcif_ACR_25_2010[[i]][,1])
  Sum_Calcif_Acr_25_2011[[i]] = sum(Pred_alo_calcif_ACR_25_2011[[i]][,1])
  Sum_Calcif_Acr_25_2012[[i]] = sum(Pred_alo_calcif_ACR_25_2012[[i]][,1])
  Sum_Calcif_Acr_25_2013[[i]] = sum(Pred_alo_calcif_ACR_25_2013[[i]][,1])
  Sum_Calcif_Acr_25_2014[[i]] = sum(Pred_alo_calcif_ACR_25_2014[[i]][,1])
  Sum_Calcif_Acr_25_2015[[i]] = sum(Pred_alo_calcif_ACR_25_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Acr_25_2010 = abind::abind(Sum_Calcif_Acr_25_2010, along=1)
Sum_Calcif_Acr_25_2011 = abind::abind(Sum_Calcif_Acr_25_2011, along=1)
Sum_Calcif_Acr_25_2012 = abind::abind(Sum_Calcif_Acr_25_2012, along=1)
Sum_Calcif_Acr_25_2013 = abind::abind(Sum_Calcif_Acr_25_2013, along=1)
Sum_Calcif_Acr_25_2014 = abind::abind(Sum_Calcif_Acr_25_2014, along=1)
Sum_Calcif_Acr_25_2015 = abind::abind(Sum_Calcif_Acr_25_2015, along=1)

## Predict Metabolism trajectories for Pocillopora w/ scenario R=100
Pred_alo_calcif_POC_25_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_25_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POC_25_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_25_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POC_25_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POC_25_2015 = vector(mode="list", length=100)
New_Pop_Poc_25_2010 = vector(mode = "list", length = 100) ; New_Pop_Poc_25_2011 = vector(mode = "list", length = 100)
New_Pop_Poc_25_2012 = vector(mode = "list", length = 100) ; New_Pop_Poc_25_2013 = vector(mode = "list", length = 100)
New_Pop_Poc_25_2014 = vector(mode = "list", length = 100) ; New_Pop_Poc_25_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Poc_25_2010 = vector(mode="list", length=100) ; Sum_Calcif_Poc_25_2011 = vector(mode="list", length=100)
Sum_Calcif_Poc_25_2012 = vector(mode="list", length=100) ; Sum_Calcif_Poc_25_2013 = vector(mode="list", length=100)
Sum_Calcif_Poc_25_2014 = vector(mode="list", length=100) ; Sum_Calcif_Poc_25_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Poc_25_2010[[i]] = data.frame(Surface_Area = New_Pop_Poc_25[[1]][,i])
  New_Pop_Poc_25_2011[[i]] = data.frame(Surface_Area = New_Pop_Poc_25[[2]][,i])
  New_Pop_Poc_25_2012[[i]] = data.frame(Surface_Area = New_Pop_Poc_25[[3]][,i])
  New_Pop_Poc_25_2013[[i]] = data.frame(Surface_Area = New_Pop_Poc_25[[4]][,i])
  New_Pop_Poc_25_2014[[i]] = data.frame(Surface_Area = New_Pop_Poc_25[[5]][,i])
  New_Pop_Poc_25_2015[[i]] = data.frame(Surface_Area = New_Pop_Poc_25[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POC_25_2010[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_25_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_25_2011[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_25_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_25_2012[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_25_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_25_2013[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_25_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_25_2014[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_25_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POC_25_2015[[i]] <- predict(model_alo_Calcif[[2]], newdata = New_Pop_Poc_25_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Poc_25_2010[[i]] = sum(Pred_alo_calcif_POC_25_2010[[i]][,1])
  Sum_Calcif_Poc_25_2011[[i]] = sum(Pred_alo_calcif_POC_25_2011[[i]][,1])
  Sum_Calcif_Poc_25_2012[[i]] = sum(Pred_alo_calcif_POC_25_2012[[i]][,1])
  Sum_Calcif_Poc_25_2013[[i]] = sum(Pred_alo_calcif_POC_25_2013[[i]][,1])
  Sum_Calcif_Poc_25_2014[[i]] = sum(Pred_alo_calcif_POC_25_2014[[i]][,1])
  Sum_Calcif_Poc_25_2015[[i]] = sum(Pred_alo_calcif_POC_25_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Poc_25_2010 = abind::abind(Sum_Calcif_Poc_25_2010, along=1)
Sum_Calcif_Poc_25_2011 = abind::abind(Sum_Calcif_Poc_25_2011, along=1)
Sum_Calcif_Poc_25_2012 = abind::abind(Sum_Calcif_Poc_25_2012, along=1)
Sum_Calcif_Poc_25_2013 = abind::abind(Sum_Calcif_Poc_25_2013, along=1)
Sum_Calcif_Poc_25_2014 = abind::abind(Sum_Calcif_Poc_25_2014, along=1)
Sum_Calcif_Poc_25_2015 = abind::abind(Sum_Calcif_Poc_25_2015, along=1)

## Predict Metabolism trajectories for Porites w/ scenario R=100
Pred_alo_calcif_POR_25_2010 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_25_2011 = vector(mode="list", length=100)
Pred_alo_calcif_POR_25_2012 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_25_2013 = vector(mode="list", length=100)
Pred_alo_calcif_POR_25_2014 = vector(mode="list", length=100) ; Pred_alo_calcif_POR_25_2015 = vector(mode="list", length=100)
New_Pop_Por_25_2010 = vector(mode = "list", length = 100) ; New_Pop_Por_25_2011 = vector(mode = "list", length = 100)
New_Pop_Por_25_2012 = vector(mode = "list", length = 100) ; New_Pop_Por_25_2013 = vector(mode = "list", length = 100)
New_Pop_Por_25_2014 = vector(mode = "list", length = 100) ; New_Pop_Por_25_2015 = vector(mode = "list", length = 100)
Sum_Calcif_Por_25_2010 = vector(mode="list", length=100) ; Sum_Calcif_Por_25_2011 = vector(mode="list", length=100)
Sum_Calcif_Por_25_2012 = vector(mode="list", length=100) ; Sum_Calcif_Por_25_2013 = vector(mode="list", length=100)
Sum_Calcif_Por_25_2014 = vector(mode="list", length=100) ; Sum_Calcif_Por_25_2015 = vector(mode="list", length=100)
for (i in 1:100){ # Load datasets in dataframes
  New_Pop_Por_25_2010[[i]] = data.frame(Surface_Area = New_Pop_Por_25[[1]][,i])
  New_Pop_Por_25_2011[[i]] = data.frame(Surface_Area = New_Pop_Por_25[[2]][,i])
  New_Pop_Por_25_2012[[i]] = data.frame(Surface_Area = New_Pop_Por_25[[3]][,i])
  New_Pop_Por_25_2013[[i]] = data.frame(Surface_Area = New_Pop_Por_25[[4]][,i])
  New_Pop_Por_25_2014[[i]] = data.frame(Surface_Area = New_Pop_Por_25[[5]][,i])
  New_Pop_Por_25_2015[[i]] = data.frame(Surface_Area = New_Pop_Por_25[[6]][,i])
  # Hindcast models
  Pred_alo_calcif_POR_25_2010[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_25_2010[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_25_2011[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_25_2011[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_25_2012[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_25_2012[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_25_2013[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_25_2013[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_25_2014[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_25_2014[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  Pred_alo_calcif_POR_25_2015[[i]] <- predict(model_alo_Calcif[[3]], newdata = New_Pop_Por_25_2015[[i]], allow_new_levels = F, summary = T, probs = c(0.025, 0.975), ntrys = 5) 
  # Sum the 100 iterations
  Sum_Calcif_Por_25_2010[[i]] = sum(Pred_alo_calcif_POR_25_2010[[i]][,1])
  Sum_Calcif_Por_25_2011[[i]] = sum(Pred_alo_calcif_POR_25_2011[[i]][,1])
  Sum_Calcif_Por_25_2012[[i]] = sum(Pred_alo_calcif_POR_25_2012[[i]][,1])
  Sum_Calcif_Por_25_2013[[i]] = sum(Pred_alo_calcif_POR_25_2013[[i]][,1])
  Sum_Calcif_Por_25_2014[[i]] = sum(Pred_alo_calcif_POR_25_2014[[i]][,1])
  Sum_Calcif_Por_25_2015[[i]] = sum(Pred_alo_calcif_POR_25_2015[[i]][,1])
}
# Combine in only one factor
Sum_Calcif_Por_25_2010 = abind::abind(Sum_Calcif_Por_25_2010, along=1)
Sum_Calcif_Por_25_2011 = abind::abind(Sum_Calcif_Por_25_2011, along=1)
Sum_Calcif_Por_25_2012 = abind::abind(Sum_Calcif_Por_25_2012, along=1)
Sum_Calcif_Por_25_2013 = abind::abind(Sum_Calcif_Por_25_2013, along=1)
Sum_Calcif_Por_25_2014 = abind::abind(Sum_Calcif_Por_25_2014, along=1)
Sum_Calcif_Por_25_2015 = abind::abind(Sum_Calcif_Por_25_2015, along=1)

## Define the final table R=100
R_25_2010 = c() ; R_25_2011 = c() ; R_25_2012 = c() ; R_25_2013 = c() ; R_25_2014 = c() ; R_25_2015 = c()
for (i in 1:100){
  R_25_2010[[i]] = sum(Sum_Calcif_Acr_25_2010[i],Sum_Calcif_Poc_25_2010[i],Sum_Calcif_Por_25_2010[i])
  R_25_2011[[i]] = sum(Sum_Calcif_Acr_25_2011[i],Sum_Calcif_Poc_25_2011[i],Sum_Calcif_Por_25_2011[i])
  R_25_2012[[i]] = sum(Sum_Calcif_Acr_25_2012[i],Sum_Calcif_Poc_25_2012[i],Sum_Calcif_Por_25_2012[i])
  R_25_2013[[i]] = sum(Sum_Calcif_Acr_25_2013[i],Sum_Calcif_Poc_25_2013[i],Sum_Calcif_Por_25_2013[i])
  R_25_2014[[i]] = sum(Sum_Calcif_Acr_25_2014[i],Sum_Calcif_Poc_25_2014[i],Sum_Calcif_Por_25_2014[i])
  R_25_2015[[i]] = sum(Sum_Calcif_Acr_25_2015[i],Sum_Calcif_Poc_25_2015[i],Sum_Calcif_Por_25_2015[i])
}
# Final_Table R_25
R_25_Final = data.frame(Year = seq(2010,2015,1),
                        Calcif_Mean = c(mean(R_25_2010/10000),mean(R_25_2011/10000),mean(R_25_2012/10000),
                                        mean(R_25_2013/10000),mean(R_25_2014/10000),mean(R_25_2015/10000)), 
                        Calcif_sd = c(sd(R_25_2010/10000),sd(R_25_2011/10000),sd(R_25_2012/10000),
                                      sd(R_25_2013/10000),sd(R_25_2014/10000),sd(R_25_2015/10000)),
                        Nb_Colonies = c(sum(Recruits$Nb_Colony[01], Recruits$Nb_Colony[07], Recruits$Nb_Colony[13]),
                                        sum(Recruits$Nb_Colony[02], Recruits$Nb_Colony[08], Recruits$Nb_Colony[14]),
                                        sum(Recruits$Nb_Colony[03], Recruits$Nb_Colony[09], Recruits$Nb_Colony[15]),
                                        sum(Recruits$Nb_Colony[04], Recruits$Nb_Colony[10], Recruits$Nb_Colony[16]),
                                        sum(Recruits$Nb_Colony[05], Recruits$Nb_Colony[11], Recruits$Nb_Colony[17]),
                                        sum(Recruits$Nb_Colony[06], Recruits$Nb_Colony[12], Recruits$Nb_Colony[18])))
# Saving
write.table(R_25_Final, "~/Desktop/R_25_Final.xls", dec = ",", sep="\t", row.names = F)

# Final Plot
Calcif_Prediction_loss_recruits <- read_excel("~/Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "Calcification")
# Geom polygon
Geom_Confint_Up = aggregate(Calcif_Normalized+Calcif_sd_Normalized~Year + Scenario, data = Calcif_Prediction_loss_recruits, FUN = mean)
Geom_Confint_Dw = aggregate(Calcif_Normalized-Calcif_sd_Normalized~Year + Scenario, data = Calcif_Prediction_loss_recruits, FUN = mean)
Geom_Confint <- data.frame(Pred = c(Geom_Confint_Up[,3],rev(Geom_Confint_Dw[,3])),
                           Year = c(Geom_Confint_Dw$Year,rev(Geom_Confint_Dw$Year)),
                           Scenario = c(Geom_Confint_Dw$Scenario,rev(Geom_Confint_Dw$Scenario)))
# Final Plot
Fig_5 = ggplot(Calcif_Prediction_loss_recruits, aes(y = Calcif_Normalized, x = Year, col = Scenario)) + geom_line() +
  geom_polygon(data=Geom_Confint, mapping=aes(x=Year, y=Pred, fill = Scenario), alpha = 0.4) +
  scale_color_manual("Scenario loss \nof recruits", values=c("firebrick2", "orange", "gold", "olivedrab3")) + 
  scale_fill_manual("Scenario loss \nof recruits", values=c("firebrick2", "orange", "gold", "olivedrab3")) +
  scale_x_continuous(name="", labels = c("Year 0", "Year 1", "Year 2", "Year 3", "Year 4", "Year 5")) + 
  scale_y_continuous(name = "Normalized calcification rate") 
# Saving Plot
export::graph2eps(Fig_5, file = "/Results/Figure 5.esp", cairo = TRUE)