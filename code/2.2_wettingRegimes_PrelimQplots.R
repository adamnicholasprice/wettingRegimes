library(ggplot2)
library(tidyverse)
library(ggridges)

dat = read_csv("data/zz_archive/wettingRegimes_wettingLengths_Peaks.csv") %>%
  na.omit()


cols <- 
  c("0.05" = "#4477AA",
    "0.1" = "#66CCEE",
    "0.15" = "#228833",
    "0.25" = "#CCBB44",
    "0.5" = "lightgrey")

dat <- dat %>%
  mutate(peakDiff = wetPeakLength - wetLength)

### Plot sd by quantile

ggplot(data=dat) +
  geom_boxplot(aes(x = factor(Q_threshold),y = peakDiff, group = factor(Q_threshold)),outlier.shape = NA)+
  ylim(0,10)+
  theme_minimal()

ggplot(data=dat) +
  geom_boxplot(aes(x = factor(Q_threshold),y = peakDiff, group = factor(Q_threshold)),outlier.shape = NA)+
  ylim(0,10)+
  theme_minimal()


sd(dat$wetLength)


ggplot(data=dat, 
                aes(x = peakDiff , y= factor(Q_threshold) ,group=factor(Q_threshold),fill=factor(Q_threshold))) + 
  geom_density_ridges(stat = "binline",bins = 15,scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  # geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = cols) +
  xlim(0,20)+
  # scale_x_continuous(breaks=c(0,93,184,275,365),limits = c(0,366),labels = c("March 1","June 1","September 1","December 1", "February 28"))+
  xlab("Rewetting Day (Atmospheric Day Of Year)")+
  # scale_x_date(labels = date_format("%B"),expand = c(0,0),date_breaks = "months")+
  theme_minimal()
