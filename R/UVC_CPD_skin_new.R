##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<HEAD>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##*********************************************************************************************************
## load packages
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggprism)
library(ggsci)
library(openxlsx)

## load data
young_skin <- read.csv("Data/UVC_Auswertung_Marc_young skin_V1.csv") %>% mutate(Origin = "Young_skin")
old_skin <- read.csv("Data/UVC_Auswertung_Marc_old skin_V2.csv") %>% mutate(Origin = "Old_skin")

## combine data from young and old skin and wrangle data
dat_combined <- rbind(young_skin, old_skin) 

## define controls for each irradiation device for later statistical testing
control_222nm <- dat_combined %>%
  filter(Type == "control") %>%
  mutate(Type = "222nm", Dose = "0 J/m\xb2")
control_254nm <- dat_combined %>%
  filter(Type == "control") %>%
  mutate(Type = "254nm", Dose = "0 J/m\xb2")

## replace original controls by duplicated controls assigned to the 222nm and 254nm lamps
dat_raw <- dat_combined %>% filter(Type != "control") %>%
  bind_rows(control_222nm, control_254nm)

## convert data to factors
dat_raw <- dat_raw %>%
  mutate(Group = paste(Dose, Type, sep = " ")) %>% 
  mutate(Group = factor(Group, levels = c("0 J/m\xb2 222nm", 
                                          "30 J/m\xb2 222nm", "300 J/m\xb2 222nm", "1000 J/m\xb2 222nm", "2000 J/m\xb2 222nm", "0 J/m\xb2 254nm",
                                          "30 J/m\xb2 254nm", "300 J/m\xb2 254nm", "1000 J/m\xb2 254nm", "2000 J/m\xb2 254nm"),
                        labels = c("0 J/m² 222nm", 
                                   "30 J/m² 222nm", "300 J/m² 222nm", "1000 J/m² 222nm", "2000 J/m² 222nm",
                                   "0 J/m² 254nm",  "30 J/m² 254nm", "300 J/m² 254nm", "1000 J/m² 254nm", "2000 J/m² 254nm"))) %>%
  mutate(Dose = factor(Dose, levels = c("0 J/m\xb2", "30 J/m\xb2", "300 J/m\xb2", "1000 J/m\xb2", "2000 J/m\xb2"),
                       labels = c("0 J/m²", "30 J/m²", "300 J/m²", "1000 J/m²", "2000 J/m²"))) %>%
  mutate(Type = factor(Type, levels = c("222nm", "254nm"), labels = c("222nm", "254nm"))) %>%
  mutate(Location = factor(Location, levels = c("basal", "suprabasal"), labels = c("Basal", "Suprabasal")))

##*********************************************************************************************************
## Percentage of CPD positive cells

## define data for analysis of percentage of CPD positive cells 
dat_perc <- dat_raw %>%
  mutate(Count_perc = ifelse(Count_total != 0 & !is.na(Count_total), round(Count_pos/Count_total,3)*100, Count_total)) 



###############################
##      1. Statistics        ##
###############################

## plot histogram to examine distribution
svg("Results/histogram.svg",  width=7, height=10)
p_hist <- dat_perc %>% ggplot(aes(x=Count_perc)) + geom_histogram()
print(p_hist)
dev.off()

## plot qqplot for whole data
svg("Results/qqplot_total.svg",  width=7, height=10)
p_hist <- dat_perc %>% ggplot(aes(sample=Count_perc)) + geom_qq()
print(p_hist)
dev.off()

## plot qq plots for each group separately
svg("Results/qqplots_by_group.svg",  width=7, height=10)
p_qq <- dat_perc %>% ggplot(aes(sample = Count_perc)) + geom_qq() + facet_grid(Dose ~ Location + Origin, scales = "free")
print(p_qq)
dev.off()

## calculate pvalues for Dose effect
pvals_dose <- dat_perc %>% filter(!(Type == "222nm" & Location == "Basal")) %>% 
  group_by(Location, Origin, Type) %>% 
  wilcox_test(Count_perc~Dose, p.adjust.method = "none") %>% 
  filter(group1 == "0 J/m²") %>%
  mutate(effect = "Dose")

## calcuate pvalues for Origin effect
pvals_type <- dat_perc %>% 
  group_by(Dose, Location, Origin) %>% 
  wilcox_test(Count_perc~Type, p.adjust.method = "none") %>% 
  mutate(effect = "Type") 

## calcuate pvalues for Origin effect
pvals_origin <- dat_perc %>% filter(!(Type == "222nm" & Location == "Basal")) %>% 
  group_by(Dose, Location, Type) %>% 
  wilcox_test(Count_perc~Origin, p.adjust.method = "none") %>% 
  mutate(effect = "Origin") 

## calculate pvalues for Location effect
pvals_location <- dat_perc %>%  
  group_by(Dose, Origin, Type) %>% 
  wilcox_test(Count_perc~Location, p.adjust.method = "none") %>% 
  mutate(effect = "Location")

## adjust pvalues based on all tests which were performed
pvals <- pvals_dose %>% select(effect, p) %>%
  bind_rows(pvals_type %>% select(effect,p)) %>%
  bind_rows(pvals_origin %>% select(effect,p)) %>%
  bind_rows(pvals_location %>% select(effect,p)) %>%
  adjust_pvalue(method = "fdr")

## add adjusted pvalues for each comparison
pvals_dose$p.adj <- pvals %>% filter(effect == "Dose") %>% .$p.adj
pvals_type$p.adj <- pvals %>% filter(effect == "Type") %>% .$p.adj
pvals_origin$p.adj <- pvals %>% filter(effect == "Origin") %>% .$p.adj
pvals_location$p.adj <- pvals %>% filter(effect == "Location") %>% .$p.adj

## replace orignal asterisks with adjusted pvalue significance
add_significance <- function(data){
  tmp <- ifelse(data$p.adj >= 0.05, "ns", NA)
  tmp <- ifelse(data$p.adj <  0.05, "*", tmp)
  tmp <- ifelse(data$p.adj <  0.01, "**", tmp)
  tmp <- ifelse(data$p.adj <  0.001, "***", tmp)
  tmp <- ifelse(data$p.adj <  0.0001, "****", tmp)
  return(tmp)
}

pvals_dose$p.adj.signif <- add_significance(pvals_dose)
pvals_type$p.adj.signif <- add_significance(pvals_type)
pvals_origin$p.adj.signif <- add_significance(pvals_origin)
pvals_location$p.adj.signif <- add_significance(pvals_location)

## save pvals as xlsx
write.xlsx(pvals_dose, "Results/01_UVC_CPD_percentage_pvals_dose.xlsx")
write.xlsx(pvals_type, "Results/02_UVC_CPD_percentage_pvals_type.xlsx")
write.xlsx(pvals_origin, "Results/03_UVC_CPD_percentage_pvals_origin.xlsx")
write.xlsx(pvals_location, "Results/04_UVC_CPD_percentage_pvals_location.xlsx")

###############################
##      2. Plotting          ##
###############################

plot_cpd_percentage0 <- function(type, upper_limit = 100){
  svg(paste0("Results/UVC_CPD_percentage_",type, "_dose.svg"),  width=6, height=5)
  p <- dat_perc %>% 
    filter(Type == type) %>% 
    group_by(Dose, Location, Origin) %>%
    summarize(mean = mean(Count_perc, na.rm =T),
              se = sd(Count_perc, na.rm = T)) %>%
    mutate(Dose = parse_number(as.character(Dose))) %>%
    mutate(lower = mean-se, upper = mean+se) %>%
    ggplot(aes(Dose, mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_line(lty = 3) +
    facet_wrap(Origin~Location, scales = "free") + 
    scale_x_continuous(guide = "prism_offset") +
    scale_y_continuous(guide = "prism_offset_minor", limits = c(-7, upper_limit)) + 
    theme_prism(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    xlab(paste0("UVC ", type, "Dosis (J/m²)")) +
    ylab("CPD-positive Zellkerne (%)")
  print(p)
  dev.off()
}

plot_cpd_percentage0("222nm", upper_limit = 25)
plot_cpd_percentage0("254nm")

## function to plot and save data filtered by skin origin, faceted by basal/suprabasal and grouped by irradiation type
plot_cpd_percentage <- function(origin){
  svg(paste0("Results/UVC_CPD_percentage_",origin, ".svg"),  width=8, height=4)
  p <- dat_perc %>% filter(Origin == origin) %>%
    ggboxplot(x = "Dose", y = "Count_perc", fill = "Type", outlier.shape = NA) +
    geom_point(aes(shape = Type), position = position_jitterdodge()) +
    facet_wrap(~Location) +
    scale_fill_manual(values = pal_npg(alpha = 0.9)(2)) +
    scale_x_discrete(guide = "prism_offset") +
    scale_y_continuous(guide = "prism_offset_minor", limits = c(0, 100)) + # Hier wird die y-Achse auf 0–100 % begrenzt
    theme_prism(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    xlab("") +
    ylab("CPD-positive Zellkerne (%)")
  print(p)
  dev.off()
}

## plot and save data for young and old skin
plot_cpd_percentage("Young_skin")
plot_cpd_percentage("Old_skin")

## function to plot and save data filtered by location, faceted by type of irradiation and grouped by location
plot_cpd_percentage2 <- function(location){
  svg(paste0("Results/UVC_CPD_percentage_",location, ".svg"),  width=8, height=4)
  p <- dat_perc %>% filter(Location == location) %>%
    ggboxplot(x = "Dose", y = "Count_perc", fill = "Origin", outlier.shape = NA) +
    geom_point(aes(shape = Origin), position = position_jitterdodge()) +
    facet_wrap(~Type) +
    scale_fill_manual(values = pal_npg(alpha = 0.9)(2)) +
    scale_x_discrete(guide = "prism_offset") +
    scale_y_continuous(guide = "prism_offset_minor", limits = c(0, 100)) + # Hier wird die y-Achse auf 0–100 % begrenzt
    theme_prism(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    xlab("") +
    ylab("CPD-positive Zellkerne (%)")
  print(p)
  dev.off()
}

## plot and save data for basal and suprabasal data
plot_cpd_percentage2("Basal")
plot_cpd_percentage2("Suprabasal")


## function to plot and save data filtered 
plot_cpd_percentage3 <- function(type){
  svg(paste0("Results/UVC_CPD_percentage_",type, "_location.svg"),  width=8, height=4)
  p <- dat_perc %>% filter(Type == type) %>%
    ggboxplot(x = "Dose", y = "Count_perc", fill = "Location", outlier.shape = NA) +
    geom_point(aes(shape = Location), position = position_jitterdodge()) +
    facet_wrap(~Origin) +
    scale_fill_manual(values = pal_npg(alpha = 0.9)(2)) +
    scale_x_discrete(guide = "prism_offset") +
    scale_y_continuous(guide = "prism_offset_minor", limits = c(0, 100)) + # Hier wird die y-Achse auf 0–100 % begrenzt
    theme_prism(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    xlab("") +
    ylab("CPD-positive Zellkerne (%)")
  print(p)
  dev.off()
}

## plot and save data for basal and suprabasal data
plot_cpd_percentage3("222nm")
plot_cpd_percentage3("254nm")

