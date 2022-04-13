#####################################################################
##
## Script name: 2.2_wettingRegimes_SamplePlot.R
##
## Author: Adam N. Price
##
## Date Created: 2022-03-02
##
## Copyright (c) Adam N. Price, 2022
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Make preliminary plots of wetting metrics
##   
##
############################# Packages #############################
##
library(tidyverse)
library(ggplot2)
##   
##
############################# Code ################################
## Load data
df = read_csv("./data/wettingRegimes_MetricsSample.csv")

## Build a dataset on the event specific metrics
df_s = df %>%
  select(gage,
         rewetting_event,
         inital_rewetting_rate,
         rewetting_rate,
         event,
         cluster,
         wet_date,
         wetLengthDate,
         wetLength,
         wetLengthQ,
         wetPeak,
         wetPeakLength,
         wetPeakQ,
         false_starts) %>%
  unique()

df_s = df_s[!is.na(df_s$gage),]

####################################################
############## Plots
####################################################


## Crossplot of inital wetting rate vs total rewetting rate
p <- ggplot(df_s) +
  geom_point(aes(x = abs(inital_rewetting_rate),
                 y = abs(rewetting_rate))
  )+
  xlim(0,20)+
  ylim(0,20)+
  theme_minimal()+
  geom_abline(slope = 1, intercept = 0)
p

## Crossplot of wetLength vs false starts
p <- ggplot(df_s) +
  geom_point(aes(x = abs(wetLength),
                 y = abs(false_starts))
  )+
  ylab("Number of False Starts")+
  xlab("Number of Days in Rewetting Period")+
  theme_minimal()+
  geom_abline(slope = 1, intercept = 0)
p


### Histogram of number of false starts
p <- ggplot(df_s,aes(false_starts))+
  geom_histogram(bins=40)+
  theme_minimal()
p

## Histogram of rewetting rates

## Hydrograph of false starts
gages = as.data.frame(
  c(01401650,
          11493500,
          06404998,
          08136000,
          05540060)
) %>%
  setNames("gage")

fs_sub = gages %>%
  left_join(.,df_s,by = "gage")

df_sub = gages %>%
  left_join(.,df,by = "gage")



get_hydro <- function(n){
  library(lubridate)
  library(tidyverse)
  
  
  siteNumber = str_pad(fs_sub$gage[n], 8, pad = "0")
  parameterCd = "00060"
  startDate = fs_sub$wet_date[n]-1
  endDate = fs_sub$wetLengthDate[n]
  
  dat = dataRetrieval::readNWISuv(
    siteNumber,
    parameterCd,
    startDate,
    endDate
  )
}

n = 5
tt = get_hydro(n)
peaks = df_sub[df_sub$gage==gages$gage[n],]

p <- ggplot(data = tt) +
  geom_line(aes(x = dateTime,
                y = X_00060_00000))+
  scale_y_continuous(trans='log10') +
  theme_minimal()


p <-p + geom_point(data = peaks,aes(x = wet_DateTime,
                         y = round(q,1)), color="red")


p + geom_line(aes(x = dateTime,
                y = round(X_00060_00000,1)),color = 'blue')+
  scale_y_continuous(trans='log10') +
  theme_minimal()

## Hydrograph of wetLengthPeak

gages = c("",
          "",
          "",
          "",
          "")
