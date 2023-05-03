#####################################################################
##
## Script name: wettingRegimes_PlotWetDate.R
##
## Author: Adam N. Price
##
## Date Created: 2022-02-25
##
## Copyright (c) Adam N. Price, 2022
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Ridgeplots and stacked histograms of date of rewetting faceted
## by Drying Regimes cluster membership
##
############################# Packages #############################
library(ggplot2)
library(tidyverse)
library(ggridges)
library(viridis)
library(patchwork)
##
############################# Code ################################

############################# Code ################################

########## Load Data ##########
wt_df <- read_csv("data/wettingRegimes_wetDate.csv")



################################################
# Ridge Plots
################################################

cols <- 
  c("1" = "#4477AA",
    "2" = "#66CCEE",
    "3" = "#228833",
    "4" = "#CCBB44",
    "all" = "lightgrey")

wt_df$cluster = as.factor(wt_df$cluster)

wt_df<-wt_df %>%
  mutate(wet_aday = if_else(wet_yday>61, wet_yday-61, wet_yday+305))

wt_df$wet_rday = lubridate::as_date(wt_df$wet_aday)

## Spoof the dates to make the plot labels work.....
wt_df<-wt_df %>% 
  mutate(wet_rday = if_else(wet_rday<"1970-03-01", wet_rday+365, wet_rday+0))


clean = wt_df
tt = wt_df
tt$cluster = "all"

wt_df = rbind(wt_df,tt)
### Plot
ridges = ggplot(data=wt_df, 
       aes(x = wet_aday , y= cluster ,group=cluster,fill=cluster)) + 
  # geom_density_ridges(stat = "binline",bins = 60,scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  geom_density_ridges(scale=2,alpha = .75,panel_scaling = FALSE,rel_min_height=0.01) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks=c(0,93,184,275,365),limits = c(0,366),labels = c("March 1","June 1","September 1","December 1", "February 28"))+
  xlab("Rewetting Day (Atmospheric Day Of Year)")+
  # scale_x_date(labels = date_format("%B"),expand = c(0,0),date_breaks = "months")+
  theme_minimal()

wt_df = clean

hist = ggplot(data=wt_df, 
       aes(x = wet_aday,group=cluster,fill=cluster)) + 
  geom_histogram(binwidth = 12,alpha=0.8)+
  scale_x_continuous(breaks=c(0,93,184,275,365),limits = c(0,366),labels = c("March 1","June 1","September 1","December 1", "February 28"))+
  xlab("Rewetting Day (Atmospheric Day Of Year)")+
  scale_fill_manual(values = cols)+
  theme_minimal()


p <- ridges / hist


### Save plots #####
pdf("docs/wetMet_wetDate.pdf")
p
dev.off()
