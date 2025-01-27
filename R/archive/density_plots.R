#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<HEAD>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##*********************************************************************************************************
## load packages
library(tidyverse)
library(ggpubr)
library(ggprism)
library(readxl)
library(ggsci)

###################
##   Load data   ##
###################

## suprabasal data
dat_hist_nbl01 <- read.csv("Data/01 cumulativeResults-nbl.csv")   %>% mutate(Treatment = "Control")
dat_hist_nbl05 <- read.csv("Data/05 cumulativeResults-nbl.csv")   %>% mutate(Treatment = "2000 J/m² 222nm")
dat_hist_nbl09 <- read.csv("Data/09 cumulativeResults-nbl.csv")   %>% mutate(Treatment = "2000 J/m² 254nm")
dat_nbl <- rbind(dat_hist_nbl01, dat_hist_nbl05, dat_hist_nbl09)  %>% mutate(layer = "suprabasal")


p99.9_suprabasal <- quantile(dat_nbl[dat_nbl$Treatment == "Control",]$Mean.G, 0.999)

p_nbl_full <- dat %>% 
  filter(layer == "suprabasal") %>%
  ggplot(aes(x=Mean.G, fill = Treatment)) + 
  geom_density() +
  geom_vline(xintercept = 3.052, lty = 2, color = "blue")
  scale_x_continuous(expand = c(0,0), limits = c(0, 88)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_jco(alpha = 0.5) +
  labs(subtitle = "Suprabasal cells (Full range)",
       caption = "Blue dashed line: 99.9th percentile of control cells.",
       xlab = "Mean fluorescence (a.u.)",
       ylab = "Density") +
  theme_bw()


p_nbl_zoom <- dat %>% 
  filter(layer == "suprabasal") %>%
  ggplot(aes(x=Mean.G, fill = Treatment)) + 
  geom_density() +
  geom_vline(xintercept = p99.9_suprabasal, lty = 2, color = "blue") + # Linie für 99,9. Perzentil
  scale_x_continuous(expand = c(0,0), limits = c(0, 15)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_jco(alpha = 0.5) +
  labs(subtitle = "Suprabasal cells (Zoomed range)",
       caption = "Blue dashed line: 99.9th percentile of control cells.",
       xlab = "Mean fluorescence (a.u.)",
       ylab = "Density") +
  theme_bw()


p_combined <- ggarrange(p_nbl_full, p_nbl_zoom, common.legend = TRUE, legend = "bottom")

svg("Results/suprabasal_density_plots_full_and_zoom.svg", width=12, height=6)
annotate_figure(p_combined, top = text_grob("Fluorescence Intensity: Full vs Zoomed View", face = "bold", size = 14))
dev.off()
