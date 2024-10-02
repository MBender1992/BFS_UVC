##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<HEAD>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

## basal data
dat_hist_bl01 <- read.csv("Data/01 cumulativeResults-bl.csv") %>% mutate(Treatment = "Control")
dat_hist_bl05 <- read.csv("Data/05 cumulativeResults-bl.csv") %>% mutate(Treatment = "2000 J/m² 222nm")
dat_hist_bl09 <- read.csv("Data/09 cumulativeResults-bl.csv") %>% mutate(Treatment = "2000 J/m² 254nm")
dat_bl <- rbind(dat_hist_bl01, dat_hist_bl05, dat_hist_bl09)  %>% mutate(layer = "basal")

## suprabasal data
dat_hist_nbl01 <- read.csv("Data/01 cumulativeResults-nbl.csv")   %>% mutate(Treatment = "Control")
dat_hist_nbl05 <- read.csv("Data/05 cumulativeResults-nbl.csv")   %>% mutate(Treatment = "2000 J/m² 222nm")
dat_hist_nbl09 <- read.csv("Data/09 cumulativeResults-nbl.csv")   %>% mutate(Treatment = "2000 J/m² 254nm")
dat_nbl <- rbind(dat_hist_nbl01, dat_hist_nbl05, dat_hist_nbl09)  %>% mutate(layer = "suprabasal")

## combine both datasets
dat <- rbind(dat_bl, dat_nbl) %>% mutate(Treatment = factor(Treatment, levels = c("Control", "2000 J/m² 222nm", "2000 J/m² 254nm")))
 
## plot basal cells
p_bl <- dat %>% 
  filter(layer == "basal") %>%
  ggplot(aes(x=Mean.G, fill = Treatment)) + 
  geom_density() +
  geom_vline(xintercept = quantile(dat_bl[dat_bl$Treatment == "Control",]$Mean.G, 0.99), lty = 2, color = "red") +
  scale_x_continuous(expand = c(0,0), limits = c(0, 7.5))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.5))+
  scale_fill_jco(alpha = 0.5) +
  labs(subtitle = "Basal cells") +
  xlab("Mean fluorescence (a.u.)") +
  ylab("Density") + 
  theme_bw() 
  
## plot suprabasal cells
p_nbl <- dat %>% 
  filter(layer == "suprabasal") %>%
  ggplot(aes(x=Mean.G, fill = Treatment)) + 
  geom_density() +
  geom_vline(xintercept = quantile(dat_bl[dat_nbl$Treatment == "Control",]$Mean.G, 0.99), lty = 2, color = "red") +
  scale_x_continuous(expand = c(0,0), limits = c(0, 7.5))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.5))+
  scale_fill_jco(alpha = 0.5) +
  labs(subtitle = "Suprabasal cells",
       caption = "Red dashed line indicates 99th percentile of control cells.") +
  xlab("Mean fluorescence (a.u.)") +
  ylab("Density") +
  theme_bw()

p <- ggarrange(p_bl, p_nbl, common.legend = TRUE, legend = "bottom")

svg("Results/density_plot_99th_percentiles.svg",  width=9, height=5)
annotate_figure(p, top = text_grob("Irradiation of skin biopsies with UVC 222nm and 254nm", face = "bold", size = 14))
dev.off()


