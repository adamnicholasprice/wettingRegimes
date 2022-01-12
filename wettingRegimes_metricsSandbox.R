library(parallel)
library(lubridate)
library(tidyverse)


# Get list of files
files <- list.files('../../DryingRegimes/data/daily_data_with_ climate_and_PET/csv',pattern = "*.csv",full.names = TRUE)

gage <- as.character(tools::file_path_sans_ext(basename(files)))

# Load wetting date data
wet = read_csv("data/wettingRegimes_wetDate.csv")
wet$wet_date = lubridate::as_date(wet$wet_date)

# output = tibble(event = NA,
#                 gage = NA,
#                 cluster = NA,
#                 wet_date =NA,
#                 wetLengthDate = NA,
#                 wetLength = NA,
#                 wetLengthQ = NA,
#                 wetPeak = NA,
#                 wetPeakLength = NA,
#                 wetPeakQ = NA,
#                 Q_threshold = NA)


metrics_fun <- function(n){
  library(lubridate)
  library(tidyverse)
  
  # for (n in 1:nrow(wet)){
  
  
  output = tibble(
    event = NA,
    gage = NA,
    cluster = NA,
    wet_date =NA,
    wetLengthDate = NA,
    wetLength = NA,
    wetLengthQ = NA,
    wetPeak = NA,
    wetPeakLength = NA,
    wetPeakQ = NA,
    Q_threshold = NA)
  
  
  whichfile <- grep(
    x = files,
    pattern = wet$gage[n],
    value = TRUE
  )
  
  
  dat <- read_csv(file = whichfile, 
                 col_types = 'dDdddddddd') %>% 
    mutate(date=as_date(Date), 
           num_date = as.numeric(Date), 
           q = X_00060_00003) %>% 
    select(date, num_date, q) %>% 
    na.omit() 
    
  dat$date = lubridate::as_date(dat$date)
  wet_date = wet$wet_date[n]
  gage = wet$gage[n]
  cluster = wet$cluster[n]
  
  p_list = c(0.05,0.1,0.15,0.25,0.50)
  for (i in p_list){
    df<-dat %>%
      #Round to nearest tenth
      mutate(q = round(q, 1)) %>%
      #1% quantile thresholds
      mutate(q_peak = if_else(q>quantile(q,i),  q, 0))
    
    df<-df %>% 
      #Define forward and backward slope at each point
      mutate(
        slp_b = (q_peak-lag(q_peak))/(num_date-lag(num_date)), 
        slp_f = (lead(q_peak)-q_peak)/(lead(num_date)-num_date), 
        slp_f = (lead(q_peak)-q_peak)/(lead(num_date)-num_date),  
      ) %>% 
      #now flag those derivative changes
      mutate(peak_flag = if_else(slp_b>0.000 & slp_f>0, 0,1),
             peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) 
  
        # Filter out any data before wet date
        temp = df[df$date>=wet_date,]
        
        # Determine the wet length date
        wetLengthDate  = temp[temp$q_peak>0,]$date[1]
        
        # Determine the wetting length
        wetLength =  as.numeric(wetLengthDate - wet_date,units = "days")
          
        # Value at that length
        wetLengthQ = temp[temp$q_peak>0,]$q_peak[1]
        
        ## Get the date for the next wetPeak
        wetPeak = temp[temp$slp_f<0,]$date[1]
        
        ## Get the length to the next wetPeak
        wetPeakLength =  as.numeric(wetPeak - wet_date,units = "days")
        
        ## Get the value at that date
        wetPeakQ = temp[temp$slp_f<0,]$q_peak[1]
        
        ## Write the percentile used in filering
        Q_threshold = i
        
        event = n
        # output = output %>% add_row(event,gage,cluster,wet_date,wetLengthDate,wetLength,wetLengthQ,wetPeak,wetPeakLength,wetPeakQ,Q_threshold)
        output = output %>% add_row(event,gage,cluster,wet_date,wetLengthDate,wetLength,wetLengthQ,wetPeak,wetPeakLength,wetPeakQ,Q_threshold)
  }
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
      event = NA,
      gage = NA,
      cluster = NA,
      wet_date =NA,
      wetLengthDate = NA,
      wetLength = NA,
      wetLengthQ = NA,
      wetPeak = NA,
      wetPeakLength = NA,
      wetPeakQ = NA,
      Q_threshold = NA)}
  )
}

# get number of cores
n.cores <- detectCores()

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('files', 'metrics_fun','wet'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, nrow(wet)),execute) #length(files)

# Stop the cluster
stopCluster(cl)

#gather output
output<-bind_rows(x)

#Capture finishing time
tf<-Sys.time()
tf-t0
  
# Write the data
write_csv(output,"data/wettingRegimes_wettingLengths_Peaks.csv")
###########
# output<-tibble(
#   wet_date5 = wet_date,
#   wetdateQ5 = NA,
#   wetLength5 = NA,
#   wetPeak5 = NA,
#   wetPeakQ5 = NA,
#   wet_date1= wet_date,
#   wetdateQ1 = NA,
#   wetLength1 = NA,
#   wetPeak1 = NA,
#   wetPeakQ1 = NA,
#   wet_date15 = wet_date,
#   wetdateQ15 = NA,
#   wetLength15 = NA,
#   wetPeak15 = NA,
#   wetPeakQ15 = NA,
#   wet_date25 = wet_date,
#   wetdateQ25 = NA,
#   wetLength25 = NA,
#   wetPeak25 = NA,
#   wetPeakQ25 = NA
# )
# if (i == 0.05){
#   output5<-tibble(wet_date5 = wet_date,
#          wetdateQ5 = df[df$date==wet_date,'q'],
#          wetLength5 = tt$lengths[which(t==which(df$date==wet_date)-1)+1],
#          wetPeak5 = wet_date + wetLength5,
#          wetPeakQ5 = df[df$date==wetPeak5,'q'])
# } else if (i == 0.1){
#   output1<-tibble(wet_date1= wet_date,
#          wetdateQ1 = df[df$date==wet_date,'q'],
#          wetLength1 = tt$lengths[which(t==which(df$date==wet_date)-1)+1],
#          wetPeak1 = wet_date + wetLength1,
#          wetPeakQ1 = df[df$date==wetPeak1,'q'])
# } else if (i == 0.15){
#   output15<-tibble(wet_date15 = wet_date,
#          wetdateQ15 = df[df$date==wet_date,'q'],
#          wetLength15 = tt$lengths[which(t==which(df$date==wet_date)-1)+1],
#          wetPeak15 = wet_date + wetLength15,
#          wetPeakQ15 = df[df$date==wetPeak15,'q'])
# } else if (i == 0.25){
#   output25<-tibble(wet_date25 = wet_date,
#          wetdateQ25 = df[df$date==wet_date,'q'],
#          wetLength25 = tt$lengths[which(t==which(df$date==wet_date)-1)+1],
#          wetPeak25 = wet_date + wetLength25,
#          wetPeakQ25 = df[df$date==wetPeak25,'q'])
# } else {output<-tibble(
#   wet_date5 = wet_date,
#   wetdateQ5 = NA,
#   wetLength5 = NA,
#   wetPeak5 = NA,
#   wetPeakQ5 = NA,
#   wet_date1= wet_date,
#   wetdateQ1 = NA,
#   wetLength1 = NA,
#   wetPeak1 = NA,
#   wetPeakQ1 = NA,
#   wet_date15 = wet_date,
#   wetdateQ15 = NA,
#   wetLength15 = NA,
#   wetPeak15 = NA,
#   wetPeakQ15 = NA,
#   wet_date25 = wet_date,
#   wetdateQ25 = NA,
#   wetLength25 = NA,
#   wetPeak25 = NA,
#   wetPeakQ25 = NA
# )}
