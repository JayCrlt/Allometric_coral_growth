rm(list=ls()) ; library(ggplot2)

#################
### FIGURE S1 ###
#################

load("Data_Coral_SnS_2008_2009_2010.RData")
load("Data_SNS_Complete_100iter.RData")
random_iteration = sample(1:100,9) ; 
Analyse_S1.1 = subset(Data_Coral_SnS, nb_iter == random_iteration[1] & Year == "2009" | nb_iter == random_iteration[2] & Year == "2009" | 
                                      nb_iter == random_iteration[3] & Year == "2009" | nb_iter == random_iteration[4] & Year == "2009" | 
                                      nb_iter == random_iteration[5] & Year == "2009" | nb_iter == random_iteration[6] & Year == "2009" | 
                                      nb_iter == random_iteration[7] & Year == "2009" | nb_iter == random_iteration[8] & Year == "2009" | 
                                      nb_iter == random_iteration[9] & Year == "2009")
Analyse_S1.2 = subset(Data_Coral_SnS_2008_2009_2010, Date == "01.01.2009")
Analyse_S1.1$Population = rep("Modelled", length(Analyse_S1.1$Year)) ; Analyse_S1.1 = Analyse_S1.1[,c(7,11)]
Analyse_S1.2$Population = rep("Observed", length(Analyse_S1.2$Date)) ; Analyse_S1.2 = Analyse_S1.2[,c(7,8)]
colnames(Analyse_S1.1) = colnames(Analyse_S1.2) ; Analyse_S1 = rbind(Analyse_S1.1, Analyse_S1.2)
xbp_S1 <- ggpubr::ggboxplot(Analyse_S1, "Population", "Mean_Diam", width = 0.3, fill = "lightgrey") + ggpubr::rotate() + ggpubr::theme_transparent() ; xbp_grob <- ggplotGrob(xbp)

t.test(Analyse_S1.1$Mean_Diam,Analyse_S1.2$Mean_Diam, alternative = "two.sided")

Fig_S1_2009 = ggplot(data = Analyse_S1, aes(x = Mean_Diam)) + 
  geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 1) + 
  annotation_custom(grob = xbp_grob, xmin = min(na.omit(Analyse_S1$Mean_Diam)), xmax = max(na.omit(Analyse_S1$Mean_Diam)), ymin = 700, ymax = 1100) +
  facet_wrap(~Population) + scale_x_continuous(name="Mean diameter (cm)")

Analyse_S1.1 = subset(Data_Coral_SnS, nb_iter == random_iteration[1] & Year == "2008" | nb_iter == random_iteration[2] & Year == "2008" | 
                        nb_iter == random_iteration[3] & Year == "2008" | nb_iter == random_iteration[4] & Year == "2008" | 
                        nb_iter == random_iteration[5] & Year == "2008" | nb_iter == random_iteration[6] & Year == "2008" | 
                        nb_iter == random_iteration[7] & Year == "2008" | nb_iter == random_iteration[8] & Year == "2008" | 
                        nb_iter == random_iteration[9] & Year == "2008")
Analyse_S1.2 = subset(Data_Coral_SnS_2008_2009_2010, Date == "01.01.2008")
Analyse_S1.1$Population = rep("Modelled", length(Analyse_S1.1$Year)) ; Analyse_S1.1 = Analyse_S1.1[,c(7,11)]
Analyse_S1.2$Population = rep("Observed", length(Analyse_S1.2$Date)) ; Analyse_S1.2 = Analyse_S1.2[,c(7,8)]
colnames(Analyse_S1.1) = colnames(Analyse_S1.2) ; Analyse_S1 = rbind(Analyse_S1.1, Analyse_S1.2)

Fig_S1_2008 = ggplot(data = Analyse_S1, aes(x = Mean_Diam)) + 
  geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 1) + 
  facet_wrap(~Population) + scale_x_continuous(name="Mean diameter (cm)")

Analyse_S1.1 = subset(Data_Coral_SnS, nb_iter == random_iteration[1] & Year == "2010" | nb_iter == random_iteration[2] & Year == "2010" | 
                        nb_iter == random_iteration[3] & Year == "2010" | nb_iter == random_iteration[4] & Year == "2010" | 
                        nb_iter == random_iteration[5] & Year == "2010" | nb_iter == random_iteration[6] & Year == "2010" | 
                        nb_iter == random_iteration[7] & Year == "2010" | nb_iter == random_iteration[8] & Year == "2010" | 
                        nb_iter == random_iteration[9] & Year == "2010")
Analyse_S1.2 = subset(Data_Coral_SnS_2008_2009_2010, Date == "01.01.2010")
Analyse_S1.1$Population = rep("Modelled", length(Analyse_S1.1$Year)) ; Analyse_S1.1 = Analyse_S1.1[,c(7,11)]
Analyse_S1.2$Population = rep("Observed", length(Analyse_S1.2$Date)) ; Analyse_S1.2 = Analyse_S1.2[,c(7,8)]
colnames(Analyse_S1.1) = colnames(Analyse_S1.2) ; Analyse_S1 = rbind(Analyse_S1.1, Analyse_S1.2)

ggplot(data = Analyse_S1, aes(x = Mean_Diam)) + 
  geom_histogram(aes(y=..density..), color="#e9ecef", alpha=0.6, binwidth = 1) + 
  facet_wrap(~Population) + scale_x_continuous(name="Mean diameter (cm)")

#################
### FIGURE S2 ###
#################

# ACROPORA
sample = seq(1,14,0.01) ; x = sample(sample,30)
Acropora_Equation = function(x) 7e-04*x^1.727
sampling_random = Acropora_Equation(x)
bruit <- rnorm(length(sampling_random), mean=0, sd=.01)
New_Acropora_dataset = data.frame(Mean_Diam = x, Mean_Area = sampling_random + bruit)
New_Acropora_dataset = subset(New_Acropora_dataset, Mean_Area>0)
plot(New_Acropora_dataset$Mean_Diam,New_Acropora_dataset$Mean_Area, pch = 20, las = 1, xlab = "Mean diameter (cm)", ylab = "Surface area (m²)")
reg = nls(data = New_Acropora_dataset, formula = Mean_Area~a*Mean_Diam^b+0, start = list(a=0.0007,b=1))
reg = summary(reg)$coef[1]*sample^summary(reg)$coef[2] ; lines(sample,reg, col = "red", lty = 2)
# POCILLOPORA
sample = seq(1,16,0.01) ; x = sample(sample,30)
Pocillopora_Equation = function(x) 5e-04*x^1.849
sampling_random = Pocillopora_Equation(x)
bruit <- rnorm(length(sampling_random), mean=0, sd=.01)
New_Pocillopora_dataset = data.frame(Mean_Diam = x, Mean_Area = sampling_random + bruit)
New_Pocillopora_dataset = subset(New_Pocillopora_dataset, Mean_Area>0)
plot(New_Pocillopora_dataset$Mean_Diam,New_Pocillopora_dataset$Mean_Area, pch = 20, las = 1, xlab = "Mean diameter (cm)", ylab = "Surface area (m²)")
reg = nls(data = New_Pocillopora_dataset, formula = Mean_Area~a*Mean_Diam^b+0, start = list(a=0.0007,b=1))
reg = summary(reg)$coef[1]*sample^summary(reg)$coef[2] ; lines(sample,reg, col = "red", lty = 2)
# PORITES
sample = seq(1,18,0.01) ; x = sample(sample,30)
Porites_Equation = function(x) 3e-04*x^2.144
sampling_random = Porites_Equation(x)
bruit <- rnorm(length(sampling_random), mean=0, sd=.01)
New_Porites_dataset = data.frame(Mean_Diam = x, Mean_Area = sampling_random + bruit)
New_Porites_dataset = subset(New_Porites_dataset, Mean_Area>0)
plot(New_Porites_dataset$Mean_Diam,New_Porites_dataset$Mean_Area, pch = 20, las = 1, xlab = "Mean diameter (cm)", ylab = "Surface area (m²)")
reg = nls(data = New_Porites_dataset, formula = Mean_Area~a*Mean_Diam^b+0, start = list(a=0.0007,b=1))
reg = summary(reg)$coef[1]*sample^summary(reg)$coef[2] ; lines(sample,reg, col = "red", lty = 2)
# SAR
New_SAR_dataset = data.frame(rbind(New_Acropora_dataset,New_Pocillopora_dataset,New_Porites_dataset),
                             Species = c(rep("ACR",length(New_Acropora_dataset$Mean_Diam)),
                                         rep("POC",length(New_Pocillopora_dataset$Mean_Diam)),
                                         rep("POR",length(New_Porites_dataset$Mean_Diam))),
                             Trendline = rep("average", sum(c(length(New_Porites_dataset$Mean_Diam),
                                                              length(New_Pocillopora_dataset$Mean_Diam),
                                                              length(New_Acropora_dataset$Mean_Diam)))))
ggplot(New_SAR_dataset) + geom_point(aes(x = Mean_Diam, y = Mean_Area*10000, col = Species)) + theme_classic() +
  geom_smooth(aes(x = Mean_Diam, y = Mean_Area*10000, col = Species), method = 'nls', se=F, method.args = list(formula = y~a*x^b, start = list(a=5,b=1))) +
  geom_smooth(aes(x = Mean_Diam, y = Mean_Area*10000, fill = Trendline), linetype = "dotted", col = "black", size = .6, method = 'nls', se=F, method.args = list(formula = y~a*x^b, start = list(a=5,b=1))) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("A. hyacinthus", "P. verrucosa", "P. lutea")) + 
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3")) + theme(legend.text = element_text(face = "italic")) +
  scale_y_continuous(name = expression("Coral live surface area (cm"^2*")")) + 
  scale_x_continuous(name = expression("Mean diameter (cm)"))

#################
### FIGURE S3 ###
#################

IMP_Outputs <- read_excel("~/Desktop/IPM-simul M.Kayal v.26.11.2019.xlsx", sheet = "Aggregated_Outputs", 
                          col_types = c("numeric", "text", "numeric", "numeric", "text"))

IMP_Outputs_A = subset(IMP_Outputs, Species == "ACR")
(Fig_S3A = ggplot(data = IMP_Outputs_A, aes(x = Surface_Area, y = Proba, col = Scenario)) + geom_line() + 
    facet_wrap(~Year) + scale_x_continuous(name = "coral live surface area (cm²)", limits = c(0,2500)) + 
    scale_y_continuous(name = "IPM outputs (number of individuals)") + 
    theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) +
    scale_color_manual("Scenario loss \nof recruits", values=c("firebrick2", "orange", "gold", "olivedrab3")))

IMP_Outputs_B = subset(IMP_Outputs, Species == "POC")
(Fig_S3B = ggplot(data = IMP_Outputs_B, aes(x = Surface_Area, y = Proba, col = Scenario)) + geom_line() + 
    facet_wrap(~Year) + scale_x_continuous(name = "coral live surface area (cm²)", limits = c(0,2500)) + 
    scale_y_continuous(name = "IPM outputs (number of individuals)") +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) +
    scale_color_manual("Scenario loss \nof recruits", values=c("firebrick2", "orange", "gold", "olivedrab3")))

IMP_Outputs_C = subset(IMP_Outputs, Species == "POR")
(Fig_S3C = ggplot(data = IMP_Outputs_C, aes(x = Surface_Area, y = Proba, col = Scenario)) + geom_line() + 
    facet_wrap(~Year) + scale_x_continuous(name = "coral live surface area (cm²)", limits = c(0,2500)) + 
    scale_y_continuous(name = "IPM outputs (number of individuals)") +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) +
    scale_color_manual("Scenario loss \nof recruits", values=c("firebrick2", "orange", "gold", "olivedrab3")))

cowplot::plot_grid(Fig_S3A,Fig_S3B,Fig_S3C, labels = c("a","b","c"), ncol = 1, label_x = 0, label_y = 1)

#################
### FIGURE S3 ###
#################

Calcif_Alizarin_NCC <- read_excel("Desktop/Calcif_Alizarin_NCC.xlsx")
(Fig_S4 = ggplot(Calcif_Alizarin_NCC, aes(x = Bathymetry_m, y = Calcification_g_cm2_year, col = Species)) + 
  geom_point() + scale_color_manual(labels = c("A. hyacinthus", "P. verrucosa", "P. lutea"), 
                                    values=c("firebrick2","goldenrod1","royalblue3")) +
  theme(legend.text = element_text(face = "italic")) +
  scale_y_continuous(name = expression("Area-normalized calcification rate (g.cm"^-2*"yr"^-1*")")) + 
  scale_x_continuous(name = expression("Depth (m)")))

(mixt =  lmer(Calcification_g_cm2_year ~ Bathymetry_m + (Bathymetry_m||Species), Calcif_Alizarin_NCC))
summary(mixt)
  
Calcif_Alizarin_ACR = subset(Calcif_Alizarin_NCC, subset = Species == "Acropora_hyacinthus")
summary(lm(Calcif_Alizarin_ACR$Calcification_g_cm2_year~Calcif_Alizarin_ACR$Bathymetry_m))
Calcif_Alizarin_POC = subset(Calcif_Alizarin_NCC, subset = Species == "Pocillopora_verrucosa")
summary(lm(Calcif_Alizarin_POC$Calcification_g_cm2_year~Calcif_Alizarin_POC$Bathymetry_m))
Calcif_Alizarin_POR = subset(Calcif_Alizarin_NCC, subset = Species == "Porites_lutea")
summary(lm(Calcif_Alizarin_POR$Calcification_g_cm2_year~Calcif_Alizarin_POR$Bathymetry_m))

#################
### FIGURE S4 ###
#################

##############################################################
# PART 2 - ELABORATING A CORAL GROWTH MODEL FOR EACH SPECIES #
##############################################################

# 1st dataset
load("Calcification_Alizarin_Metabo.RData")
Respiro = subset(Calcification_Alizarin_Metabo, Method == "Respirometry_Chamber")
Alizarin = subset(Calcification_Alizarin_Metabo, Method == "Alizarin staining")
Calcification_Alizarin_Metabo = Alizarin
## Spliting the Growth dataset for the 3 main species
Met01  <- subset(Calcification_Alizarin_Metabo, Species == "Acropora_hyacinthus"  )
Met02  <- subset(Calcification_Alizarin_Metabo, Species == "Pocillopora_verrucosa")
Met03  <- subset(Calcification_Alizarin_Metabo, Species == "Porites_lutea"        )
Met <- list(Met01,Met02,Met03) ; Pred_meta <- list(NA,NA,NA) ; model_Alizarin = list(NA,NA,NA)
Surface_Alizarin = c(Met01$Surface_Area,Met02$Surface_Area,Met03$Surface_Area)
# Bayesian calcification model
for (i in 1:3) {
  model_Alizarin[[i]] <- brm(bf(Production_g_yr ~ a*Surface_Area^b+0, a ~ 1, b ~ 1, nl = TRUE), iter = 3000,
                             data = Met[[i]], family = gaussian(),
                             prior = c(prior(normal(10,10), nlpar = "a"),prior(normal(0.5, 0.5), nlpar = "b")),
                             control = list(adapt_delta = 0.999, max_treedepth = 30))}
(Alizarin_ACR = fitted(model_Alizarin[[1]])) ; (Alizarin_POC = fitted(model_Alizarin[[2]])) ; (Alizarin_POR = fitted(model_Alizarin[[3]]))
# 2nd dataset
load("Calcification_Alizarin_Metabo.RData")
## Spliting the Growth dataset for the 3 main species
Met01  <- subset(Calcification_Alizarin_Metabo, Species == "Acropora_hyacinthus"  )
Met02  <- subset(Calcification_Alizarin_Metabo, Species == "Pocillopora_verrucosa")
Met03  <- subset(Calcification_Alizarin_Metabo, Species == "Porites_lutea"        )
Met <- list(Met01,Met02,Met03) ; Pred_meta <- list(NA,NA,NA) ; model_all = list(NA,NA,NA)
Surface_All = c(Met01$Surface_Area,Met02$Surface_Area,Met03$Surface_Area)
# Bayesian calcification model
for (i in 1:3) {
  model_all[[i]] <- brm(bf(Production_g_yr ~ a*Surface_Area^b+0, a ~ 1, b ~ 1, nl = TRUE), iter = 3000,
                        data = Met[[i]], family = gaussian(),
                        prior = c(prior(normal(10,10), nlpar = "a"),prior(normal(0.5, 0.5), nlpar = "b")),
                        control = list(adapt_delta = 0.999, max_treedepth = 30))}
(All_ACR = fitted(model_all[[1]])) ; (All_POC = fitted(model_all[[2]])) ; (All_POR = fitted(model_all[[3]]))

data_Fig4 = data.frame(Surface_Area = c(Surface_All, Surface_Alizarin),
                       Calcification = c(All_ACR[,1],All_POC[,1],All_POR[,1],Alizarin_ACR[,1],Alizarin_POC[,1],Alizarin_POR[,1]),
                       Ribbon_low = c(All_ACR[,3],All_POC[,3],All_POR[,3],Alizarin_ACR[,3],Alizarin_POC[,3],Alizarin_POR[,3]),
                       Ribbon_upp = c(All_ACR[,4],All_POC[,4],All_POR[,4],Alizarin_ACR[,4],Alizarin_POC[,4],Alizarin_POR[,4]),
                       Species = c(rep("ACR", length(All_ACR[,1])), rep("POC", length(All_POC[,1])), rep("POR", length(All_POR[,1])), 
                                   rep("ACR", length(Alizarin_ACR[,1])),rep("POC", length(Alizarin_POC[,1])), rep("POR", length(Alizarin_POR[,1]))),
                       Models = c(rep("All", length(Surface_All)), rep("Alizarin", length(Surface_Alizarin))))

(Fig_S4 = ggplot(data_Fig4, aes(x = Surface_Area, y = Calcification)) + facet_wrap(Models~Species, labeller = label_both) +
    geom_smooth(aes(y = Calcification, col = Species), method = 'nls', se=F, formula = y~a*x^b, method.args = list(start=c(a = 1, b = 1))) +
    scale_y_continuous(name = expression("Calcification rate (kg.yr"^-1*")")) +
    scale_x_continuous(name = expression("Surface Area (cm"^2*")")) +
    geom_ribbon(aes(x = Surface_Area, ymin = Ribbon_low, ymax = Ribbon_upp, fill = Species), alpha = .5) +
    scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3")) + scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3")))