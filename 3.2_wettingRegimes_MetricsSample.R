#####################################################################
##
## Script name: wettingRegimes_MetricsSample.R
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
## Get a random sample of wetting events from all 909 gages 
## (1 event per gage)
##
############################# Packages #############################
##
library(lubridate)
library(tidyverse)
library(dataRetrieval)
library(parallel)
##
############################# Code ################################


### Load data
wetEvents = read_csv("data/wettingRegimes_wettingLengths.csv") %>%
  na.omit()


## Filter by 0.25 percentile streamflow return
wetEvents = wetEvents[wetEvents$Q_threshold==0.25,]

## Get a random sample of events
set.seed(42)
wetEvents = wetEvents %>%
  group_by(gage) %>%
  slice_sample(n=1)


# ## Plot disribution of events
# p <- ggplot(wetEvents,aes(x = wetLength)) +
#   geom_histogram()
# p

n = which(wetEvents$gage==11149400)
## Define function to extract wetting metrics
metrics_fun <- function(n){
  library(lubridate)
  library(tidyverse)
  

  siteNumber = str_pad(wetEvents$gage[n], 8, pad = "0")
  parameterCd = "00060"
  startDate = wetEvents$wet_date[n]-1
  endDate = wetEvents$wetLengthDate[n]
  
  dat = dataRetrieval::readNWISuv(
    siteNumber,
    parameterCd,
    startDate,
    endDate
  )

  ################################### 
  ####### Peak Filtering 
  ###################################
  
  dat$X_00060_00000 = round(dat$X_00060_00000,1)
  dat$q_peak = loess(X_00060_00000~as.numeric(dateTime),
                     data=dat,
                     span = 1/10,
                     degree = 1,
                     surface = "direct")$fitted
  
  
  dat[which(dat$q_peak<0),'q_peak']=0
  dat<-dat %>% 
    #Define forward and backward slope at each point
    mutate(
      slp_b = (q_peak-lag(q_peak))/(as.numeric(dat$dateTime-lag(dat$dateTime))), 
      slp_f = (lead(q_peak)-q_peak)/(as.numeric(lead(dat$dateTime)-dat$dateTime))
    ) %>% 
    # Smooth the slope a bit
    mutate(slp_b = zoo::rollmean(slp_b,k=3,fill=NA),
           slp_f = zoo::rollmean(slp_f,k=3,fill=NA)) %>%
    # mutate(peak_flag = if_else(slp_b>var(slp_b,na.rm = T)*1.5 & slp_f<=var(slp_f,na.rm = T)*1.5 | dat$X_00060_00000>=wetEvents$wetPeakQ[n], 1,0),
    mutate(peak_flag = if_else(slp_b>var(slp_b,na.rm = T)*1.5 & slp_f<=var(slp_f,na.rm = T)*1.5, 1,0),
           peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) 
  
  
  tt <- dat %>%
    filter(peak_flag==1) %>%
    select(site_no,dateTime,X_00060_00000) %>%
    setNames(c("gage","wet_DateTime","q"))
  
  
  initEvent = dat[dat$dateTime<=tt$wet_DateTime[1] & dat$q_peak>0,]
  
  #Create linear model of dQ vs q
  t<- initEvent %>% mutate(dQ = abs(lag(X_00060_00000) - X_00060_00000)) %>% filter(dQ>=0)
  model<-lm(log10(dQ+0.1)~log10(X_00060_00000+0.11), data=t)

  inital_rewetting_rate <- tryCatch(as.numeric(model$coefficients[2]), error = function(e) NA)
  p_value_i <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)

  entireEvent = dat[dat$dateTime<=tt$wet_DateTime[nrow(tt)] & dat$dateTime>=initEvent$dateTime[1],]

  t<- entireEvent %>% mutate(dQ = abs(lag(X_00060_00000) - X_00060_00000)) %>% filter(dQ>=0)
  model<-lm(log10(dQ+0.1)~log10(X_00060_00000+0.11), data=t)

  rewetting_rate <- tryCatch(as.numeric(model$coefficients[2]), error = function(e) NA)
  p_value <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
  
  output <- dat %>%
    tibble() %>%
    filter(peak_flag==1) %>%
    select(site_no,dateTime,X_00060_00000) %>%
    setNames(c("gage","wet_DateTime","q")) %>%
    mutate(rewetting_event = wetEvents$event[n],
           gage =  wetEvents$gage[n],
           threshold = wetEvents$Q_threshold[n],
           weting_subevent = row_number(),
           inital_rewetting_rate = inital_rewetting_rate,
           p_value_i = p_value_i,
           rewetting_rate = rewetting_rate,
           p_value = p_value,
           sample_rate = as.numeric(difftime(dat$dateTime[2],dat$dateTime[1],units='hours')))
  
  tempInit = initEvent[1,] %>%
    tibble() %>%
    select(site_no,dateTime,X_00060_00000) %>%
    setNames(c("gage","wet_DateTime","q")) %>%
    mutate(rewetting_event = wetEvents$event[n],
           gage =  wetEvents$gage[n],
           threshold = wetEvents$Q_threshold[n],
           weting_subevent = 0,
           inital_rewetting_rate = inital_rewetting_rate,
           p_value_i = p_value_i,
           rewetting_rate = rewetting_rate,
           p_value = p_value,
           sample_rate = as.numeric(difftime(dat$dateTime[2],dat$dateTime[1],units='hours')))
  
  output <- output %>% add_row(tempInit,.before=1)
  ## Call output
  output
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Execute and write-----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Start timer
t0<-Sys.time()

#Create error handling function
execute<-function(a){
  tryCatch(metrics_fun(a), error=function(e){
    tibble(
      gage = NA,
      threshold = NA,
      wet_DateTime = NA,
      q = NA,
      rewetting_event =NA,
      weting_subevent = NA,
      inital_rewetting_rate = NA,
      p_value_i = NA,
      rewetting_rate = NA,
      p_value = NA,
      sample_rate = NA)}
  )
}

# get number of cores
n.cores <- detectCores()

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('metrics_fun','wetEvents'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, nrow(wetEvents)),execute) #length(files)


# Stop the cluster
stopCluster(cl)

#gather output
output<-bind_rows(x)
output = output[rowSums(is.na(output)) != ncol(output), ]

#Capture finishing time
tf<-Sys.time()
tf-t0



### Join the sample dataset to the output dataset
tempOut = output %>%
  left_join(.,wetEvents,by = c("gage","rewetting_event"="event"))

## Filter out time after percentile is reached
tempOut = tempOut %>%
  group_by(gage) %>%
  filter(.,q<=Q_threshold_q)

## Calculate number of false starts
tempOut = tempOut %>%
  group_by(gage) %>%
  mutate(false_starts = n()-1)


###########################################
################ Plots ####################
###########################################
summary(tempOut$false_starts)
quantile(tempOut$false_starts,0.1)
quantile(tempOut$false_starts,0.9)



######## Get hydrographs
getHydro <- function(i){
  n = which(wetEvents$gage==i)
  print(n)
  siteNumber = str_pad(wetEvents$gage[n], 8, pad = "0")
  parameterCd = "00060"
  startDate = wetEvents$wet_date[n]-1
  endDate = wetEvents$wetLengthDate[n]
  
  dat = dataRetrieval::readNWISuv(
    siteNumber,
    parameterCd,
    startDate,
    endDate
  )
  dat$X_00060_00000 = round(dat$X_00060_00000,1)
  dat$q_peak = loess(X_00060_00000~as.numeric(dateTime),
                     data=dat,
                     span = 1/25,
                     degree = 1)$fitted
  return(dat)
}

getHydroDaily<- function(i){
  n = which(wetEvents$gage==i)
  print(n)
  siteNumber = str_pad(wetEvents$gage[n], 8)
  parameterCd = "00060"
  startDate = wetEvents$wet_date[n]-1
  endDate = wetEvents$wetLengthDate[n]
  
  dat = dataRetrieval::readNWISdv(
    siteNumber,
    parameterCd,
    startDate,
    endDate
  )}


############################
output = metrics_fun(n)
peaks = output[output$gage==g,]

wetEvents = read_csv("data/wettingRegimes_wettingLengths.csv") %>%
  na.omit()
wetEventsTemp = wetEvents[which(wetEvents$event==peaks$rewetting_event[1]),]

#############################

######## Plots ############
gages = as.data.frame(
  c(02227500,
    07142300,
    08198500,
    06836500,
    11149400)
) %>%
  setNames("gage")
i=5

      g = gages$gage[i]
      print(i)
      dat = getHydro(g)
      # dat_daily = getHydroDaily(g)
      
      p <- ggplot(data = dat) +
        geom_line(aes(x = dateTime,
                      y = round(X_00060_00000,1)),
                  alpha = 0.2)+
        theme_classic()+
        xlab("Date")+
        ylab("Discharge (cfs)")+
        ggtitle(paste0(g))
      
      ## Add peaks
      p <- p + geom_point(data = yy,
                          aes(x = wet_DateTime,
                              y = q),
                         color="#C8695B",
                         shape = 15,
                         size=3)
      
      # Add LOESS data
      p <- p + geom_line(data=dat,
                  aes(x = dateTime,
                      y = q_peak),color = '#4770C2')
      
      ## Add LOESS peaks
      p <- p + geom_point(data = peaks,
                          aes(x = wet_DateTime,
                              y = q),
                          color="#4770C2",
                          shape = 15,
                          size=3)
      
      # ## Add daily data
      # p <- p + geom_line(data=dat_daily,
      #                   aes(x = lubridate::ymd(Date,tz="UTC"),
      #                       y = X_00060_00003),color = '#25616F')
      
      ## Add annotations 
      ## 25th percentile
      p <- p + geom_hline(yintercept = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"Q_threshold_q"][[1]]),
                     color = "red") +
        annotate("text",label = "q_25",x =dat$dateTime[1] ,
                 y = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"Q_threshold_q"][[1]]),
                 color = "red",vjust = -1)
      
      ## 5th percentile
      p <- p + geom_hline(yintercept = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.05,"Q_threshold_q"][[1]]),
                          color = "red") +
        annotate("text",label = "q_5",
                 x =dat$dateTime[1] ,
                 y = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.05,"Q_threshold_q"][[1]]),
                 color = "red",vjust = -1)
      
      ## Daily_q peak Date
      p <- p + geom_vline(xintercept = lubridate::ymd(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"wetPeak"][[1]],tz="UTC"),color = "#C27047")+
        annotate("text",label = "Daily_q Peak Date",
                 x =lubridate::ymd(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"wetPeak"][[1]],tz="UTC"),
                 y = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"Q_threshold_q"][[1]]),
                 color = "#C27047",
                 vjust = -1,angle = 90)
      
      ## Unit value peak date
      p <- p + geom_vline(xintercept = yy$wet_DateTime[nrow(yy)],color = "#C27047",alpha = 0.5)+
        annotate("text",label = "UV Peak Date",
                 x = yy$wet_DateTime[nrow(yy)],
                 y = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"Q_threshold_q"][[1]]),
                 color = "#C27047",
                 vjust = -1,angle = 90)
      
      ## Daily_q rewetting date
      p <- p + geom_vline(xintercept = lubridate::ymd(wetEventsTemp$wet_date[1],tz="UTC"),color = "#C27047")+
        annotate("text",label = "Daily_q rewetting Date",
                 x =lubridate::ymd(wetEventsTemp$wet_date[1],tz="UTC"),
                 y = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"Q_threshold_q"][[1]]),
                 color = "#C27047",
                 vjust = -1,angle = 90)
      
      ## Unit value rewetting date
      p <- p + geom_vline(xintercept = peaks$wet_DateTime[1],color = "#C27047")+
        annotate("text",label = "Unit value rewetting Date",
                 x =lubridate::ymd(wetEventsTemp$wet_date[1],tz="UTC"),
                 y = as.numeric(wetEventsTemp[wetEventsTemp$Q_threshold==0.25,"Q_threshold_q"][[1]]),
                 color = "#C27047",
                 vjust = -1,angle = 90)
      
      
      
      # pdf(paste0("../figures/",g,".pdf"))      
      pdf(paste0("../figures/test_",g,".pdf"))
      p
      dev.off()

p


# Write the data
# write_csv(tempOut,"data/wettingRegimes_MetricsSample.csv")
