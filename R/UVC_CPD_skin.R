##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<HEAD>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##*********************************************************************************************************
## load packages
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggprism)

## define custom functions

## function to calculate dunnett test for each factor combination
custom_dunnett <- function(data, type, location){
  tmp <- data[data$Type %in% c(type, "Control") & data$Location == location,]
  stat <- DescTools::DunnettTest(tmp$mean_pos, tmp$Dose, control = "Control", conf.level = 0.95)
  stat <- as.data.frame(stat$Control)
  stat <- rownames_to_column(stat, "comparison")
  rownames(stat) <- paste(str_replace(stat$comparison, "-Control", ""), type, location)
  stat
}

## load data 
dat_raw <- read.csv("Data/UVC_Auswertung_jungeHaut.csv") %>%
  mutate(Group = paste(Dose, Type, sep = " ")) %>%
  mutate(Group = factor(Group, levels = c("control control", 
                                          "30 J/m² 222nm", "300 J/m² 222nm", "1000 J/m² 222nm", "2000 J/m² 222nm",
                                          "30 J/m² 254nm", "300 J/m² 254nm", "1000 J/m² 254nm", "2000 J/m² 254nm"),
                        labels = c("Control", 
                                   "30 J/m² 222nm", "300 J/m² 222nm", "1000 J/m² 222nm", "2000 J/m² 222nm",
                                   "30 J/m² 254nm", "300 J/m² 254nm", "1000 J/m² 254nm", "2000 J/m² 254nm"))) %>%
  mutate(Dose = factor(Dose, levels = c("control", "30 J/m²", "300 J/m²", "1000 J/m²", "2000 J/m²"),
                       labels = c("Control", "30 J/m²", "300 J/m²", "1000 J/m²", "2000 J/m²"))) %>%
  mutate(Type = factor(Type, levels = c("control", "222nm", "254nm"), labels = c("Control", "222nm", "254nm"))) %>%
  mutate(Location = factor(Location, levels = c("basal", "suprabasal"), labels = c("Basal", "Suprabasal")))

#####################################################
##      1. Percentage of CPD positive cells        ##
#####################################################

## define data for analysis of percentage of CPD positive cells 
dat_perc <- dat_raw
dat_perc <- dat_perc %>%
  mutate(Count_perc = ifelse(Count_total != 0, round(Count_pos/Count_total,3)*100, 0))
  
svg("Results/UVC_CPD_percentage.svg",  width=8, height=4)
ggbarplot(dat_perc, x = "Dose", y = "Count_perc", fill = "Type", position = position_dodge(0.7), width = c(0.35, 0.35, rep(0.7, 16)),
          facet.by = "Location", add = "mean_sd", error.plot = "upper_errorbar") +
  scale_fill_manual(values = rev(pal_npg(alpha = 0.9)(3))) +
  scale_x_discrete(guide = "prism_offset") +
  scale_y_continuous(guide = "prism_offset_minor", limits = c(0,60), breaks = seq(0,60,10)) +
  theme_prism(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab("") +
  ylab("Percentage of CPD positive cells (%)")
dev.off()

#####################################################
##    2. Mean fluorescence of CPD positive cells   ##
#####################################################

dat_flow <- dat_raw

## calculate pvalues and adjust for multiple testing
pvals <- rbind(
  custom_dunnett(data = dat_flow, type = "222nm", location = "Suprabasal"),
  custom_dunnett(data = dat_flow, type = "254nm", location = "Basal"),
  custom_dunnett(data = dat_flow, type = "254nm", location = "Suprabasal") 
) %>% 
  adjust_pvalue(method = "fdr")

write.csv(pvals, "Results/UVC_mean_fluorescence_pvals.csv")

svg("Results/UVC_mean_fluorescence.svg",  width=8, height=4)
ggbarplot(dat_flow, x = "Dose", y = "mean_pos", fill = "Type", position = position_dodge(0.7), width = c(0.35, 0.35, rep(0.7, 16)),
          facet.by = "Location", add = "mean_sd", error.plot = "upper_errorbar") +
  scale_fill_manual(values = rev(pal_npg(alpha = 0.9)(3))) +
  scale_x_discrete(guide = "prism_offset") +
  scale_y_continuous(guide = "prism_offset_minor", limits = c(0,50), breaks = seq(0,50,10)) +
  theme_prism(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab("") +
  ylab("Mean fluorescence of CPD positive cells (a.u.)")
dev.off()


########################################################
## 3. Correlation of skin thickness and CPD induction ##
########################################################

dat_corr <- dat_raw %>% filter(Location == "Suprabasal")

svg("Results/UVC_correlation_fluorescence_epiThick.svg",  width=7, height=5)
ggscatter(data = dat_corr, x = "minEpidermTick", y = "mean_pos", facet.by = "Group", scales = "free", shape = 17, color = "#4DBBD5E5") +
  geom_smooth(method = "lm", se = F, lty = 1, color = "grey50", size = 0.5) +
  theme_prism(base_size = 12) +
  scale_x_continuous(guide = "prism_offset_minor") +
  scale_y_continuous(guide = "prism_offset_minor") +
  ylab("Mean fluorescence (a.u.)") +
  xlab("Minimal epidermal thickness (µm)")
dev.off()



