##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<HEAD>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##*********************************************************************************************************
## load packages
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggprism)
library(ggsci)
library(openxlsx)

## define custom functions

## function to calculate dunnett test for each factor combination
custom_dunnett <- function(data, type, location, stat.var){
  tmp <- data[data$Type %in% c(type, "Control") & data$Location == location,]
  tmp <- tmp[!is.na(tmp$Count_total),]
  stat <- DescTools::DunnettTest(tmp[[stat.var]], tmp$Dose, control = "Control", conf.level = 0.95)
  stat <- as.data.frame(stat$Control)
  stat <- rownames_to_column(stat, "comparison")
  rownames(stat) <- paste(str_replace(stat$comparison, "-Control", ""), type, location)
  stat
}

file.name <- "UVC_Auswertung_Marc_young skin_V1.csv"

analyze_UVC <- function(file.name){
  input_file <- paste0("Data/", file.name)
  suffix <- stringr::str_remove(file.name, "UVC_Auswertung_Marc_")
  suffix <- stringr::str_remove(suffix, ".csv")
  ## load data 
  dat_raw <- read.csv2(input_file) %>%
    mutate(Group = paste(Dose, Type, sep = " ")) %>%
    mutate(Group = factor(Group, levels = c("control control", 
                                            "30 J/m\xb2 222nm", "300 J/m\xb2 222nm", "1000 J/m\xb2 222nm", "2000 J/m\xb2 222nm",
                                            "30 J/m\xb2 254nm", "300 J/m\xb2 254nm", "1000 J/m\xb2 254nm", "2000 J/m\xb2 254nm"),
                          labels = c("Control", 
                                     "30 J/m² 222nm", "300 J/m² 222nm", "1000 J/m² 222nm", "2000 J/m² 222nm",
                                     "30 J/m² 254nm", "300 J/m² 254nm", "1000 J/m² 254nm", "2000 J/m² 254nm"))) %>%
    mutate(Dose = factor(Dose, levels = c("control", "30 J/m\xb2", "300 J/m\xb2", "1000 J/m\xb2", "2000 J/m\xb2"),
                         labels = c("Control", "30 J/m²", "300 J/m²", "1000 J/m²", "2000 J/m²"))) %>%
    mutate(Type = factor(Type, levels = c("control", "222nm", "254nm"), labels = c("Control", "222nm", "254nm"))) %>%
    mutate(Location = factor(Location, levels = c("basal", "suprabasal"), labels = c("Basal", "Suprabasal"))) %>%
    mutate(mean_pos = parse_number(mean_pos),
           StratCornThick = parse_number(StratCornThick),
           minEpidermTick = parse_number(minEpidermTick))
  
  #####################################################
  ##      1. Percentage of CPD positive cells        ##
  #####################################################

  ## define data for analysis of percentage of CPD positive cells 
  dat_perc <- dat_raw
  dat_perc <- dat_perc %>%
    mutate(Count_perc = ifelse(Count_total != 0 & !is.na(Count_total), round(Count_pos/Count_total,3)*100, Count_total)) 
  
  pvals <- rbind(
    custom_dunnett(data = dat_perc, type = "222nm", location = "Basal", stat.var = "Count_perc"),
    custom_dunnett(data = dat_perc, type = "222nm", location = "Suprabasal", stat.var = "Count_perc"),
    custom_dunnett(data = dat_perc, type = "254nm", location = "Basal", stat.var = "Count_perc"),
    custom_dunnett(data = dat_perc, type = "254nm", location = "Suprabasal", stat.var = "Count_perc") 
  ) %>% 
    adjust_pvalue(method = "fdr") %>%
    rownames_to_column("type") %>%
    mutate(type = str_extract(.$type,"\\d+nm Basal|\\d+nm Suprabasal"))
  
  res.name <- paste0("Results/UVC_CPD_percentage_", suffix, ".xlsx")
  write.xlsx(pvals, res.name)
  
  svg(paste0("Results/UVC_CPD_percentage_",suffix, ".svg"),  width=8, height=4)
  p <- ggbarplot(dat_perc, x = "Dose", y = "Count_perc", fill = "Type", position = position_dodge(0.7), width = c(0.35, 0.35, rep(0.7, 16)),
                 facet.by = "Location", add = "mean_sd", error.plot = "upper_errorbar") +
    scale_fill_manual(values = rev(pal_npg(alpha = 0.9)(3))) +
    scale_x_discrete(guide = "prism_offset") +
    scale_y_continuous(guide = "prism_offset_minor") +
    theme_prism(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    xlab("") +
    ylab("Percentage of CPD positive cells (%)")
  print(p)
  dev.off()
  
  #####################################################
  ##    2. Mean fluorescence of CPD positive cells   ##
  #####################################################
  
  dat_flow <- dat_raw
  
  ## calculate pvalues and adjust for multiple testing
  pvals <- rbind(
    custom_dunnett(data = dat_flow, type = "222nm", location = "Suprabasal", stat.var = "mean_pos"),
    custom_dunnett(data = dat_flow, type = "254nm", location = "Basal", stat.var = "mean_pos"),
    custom_dunnett(data = dat_flow, type = "254nm", location = "Suprabasal", stat.var = "mean_pos") 
  ) %>% 
    adjust_pvalue(method = "fdr") %>%
    rownames_to_column("type") %>%
    mutate(type = str_extract(.$type,"\\d+nm Basal|\\d+nm Suprabasal"))
  
  res.name <- paste0("Results/UVC_mean_fluorescence_pvals_", suffix, ".xlsx")
  write.xlsx(pvals, res.name)

  svg(paste0("Results/UVC_mean_fluorescence_",suffix, ".svg"),  width=8, height=4)
  p <- ggbarplot(dat_flow, x = "Dose", y = "mean_pos", fill = "Type", position = position_dodge(0.7), width = c(0.35, 0.35, rep(0.7, 16)),
                 facet.by = "Location", add = "mean_sd", error.plot = "upper_errorbar") +
    scale_fill_manual(values = rev(pal_npg(alpha = 0.9)(3))) +
    scale_x_discrete(guide = "prism_offset") +
    scale_y_continuous(guide = "prism_offset_minor") +
    theme_prism(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    xlab("") +
    ylab("Mean fluorescence of CPD positive cells (a.u.)")
  print(p)
  dev.off()
  
  
  ########################################################
  ## 3. Correlation of skin thickness and CPD induction ##
  ########################################################
  
  dat_corr <- dat_raw %>% filter(Location == "Suprabasal")
 
  svg(paste0("Results/UVC_correlation_fluorescence_epiThick_",suffix, ".svg"),  width=7, height=5)
  p <- ggscatter(data = dat_corr, x = "minEpidermTick", y = "mean_pos", facet.by = "Group", scales = "free", shape = 17, color = "#4DBBD5E5") +
    geom_smooth(method = "lm", se = F, lty = 1, color = "grey50", size = 0.5) +
    theme_prism(base_size = 12) +
    scale_x_continuous(guide = "prism_offset_minor") +
    scale_y_continuous(guide = "prism_offset_minor") +
    ylab("Mean fluorescence (a.u.)") +
    xlab("Minimal epidermal thickness (µm)")
  print(p)
  dev.off()
  
  svg(paste0("Results/UVC_correlation_fluorescence_cornThick_",suffix, ".svg"),  width=7, height=5)
  p <- ggscatter(data = dat_corr, x = "StratCornThick", y = "mean_pos", facet.by = "Group", scales = "free", shape = 17, color = "#4DBBD5E5") +
    geom_smooth(method = "lm", se = F, lty = 1, color = "grey50", size = 0.5) +
    theme_prism(base_size = 12) +
    scale_x_continuous(guide = "prism_offset_minor") +
    scale_y_continuous(guide = "prism_offset_minor") +
    ylab("Mean fluorescence (a.u.)") +
    xlab("Minimal corneal thickness (µm)")
  print(p)
  dev.off()
}

## perform analysis
analyze_UVC(file.name = "UVC_Auswertung_Marc_young skin_V1.csv")
analyze_UVC(file.name = "UVC_Auswertung_Marc_old skin_V2.csv")




