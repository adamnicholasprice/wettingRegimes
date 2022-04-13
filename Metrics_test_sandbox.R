wetEvents = read_csv("data/wettingRegimes_wettingLengths.csv") %>%
  na.omit()
# 
# 
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
# n = which(wetEvents$gage==10315500)
# n = which(wetEvents$gage==11149400)
n = which(wetEvents$gage==06836500)
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
dat$q_peak = dat$X_00060_00000

###################################
###################################

###################################
###### Peak Filtering w LOESS
###################################

# dat$q_peak = loess(X_00060_00000~as.numeric(dateTime),
#                    data=dat,
#                    span = 1/10,
#                    degree = 1,
#                    surface = "direct")$fitted
# dat[which(dat$q_peak<0),'q_peak']=0

###################################
###################################

dat<-dat %>% 
  #Define forward and backward slope at each point
  mutate(
    lag_q = (q_peak-lag(q_peak)),
    lead_q = (lead(q_peak)-q_peak),
    lag_t = as.numeric(dat$dateTime-lag(dat$dateTime)),
    lead_t = as.numeric(lead(dat$dateTime)-dat$dateTime)
  ) %>% 
  mutate(slp_b = lag_q/lag_t,
         slp_f = lead_q / lead_t)%>%
  # Smooth the slope a bit
  # mutate(slp_b = zoo::rollmean(slp_b,k=3,fill=NA),
  #        slp_f = zoo::rollmean(slp_f,k=3,fill=NA)) %>%
  # mutate(peak_flag = if_else(slp_b>var(slp_b,na.rm = T)*1.5 & slp_f<=var(slp_f,na.rm = T)*1.5 | dat$X_00060_00000>=wetEvents$wetPeakQ[n], 1,0),
  mutate(peak_flag = if_else(slp_b>0 & slp_f<=0,1,0),
                             # abs(slp_b)>abs(var(slp_b,na.rm = T)*1.5) & abs(slp_f)>abs(var(slp_f,na.rm = T)*1.5) &
                             # (abs(lag_q)>=quantile(X_00060_00000,0.1) | abs(lead_q)==0), 1,0),
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


### Subset to get rid of all events after an event reaches 25% percentile
tt = tt[1:min(which(tt$q>=as.numeric(wetEvents[n,]["wetLengthQ"]))),]

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

## Subset to get rid of all events after an event reaches 25% percentile
output = output[1:min(which(output$q>=as.numeric(wetEvents[n,]["wetLengthQ"]))),]
output = output[output$q>quantile(output$q,.9),]

tt <-
  output %>% 
  mutate(
    lead_q = (lead(q)-q),
    lead_t = as.numeric(lead(wet_DateTime)-wet_DateTime)
  ) %>% 
  mutate(slp_f = lead_q / lead_t)


## Call output
peaks = output

# filter(min(which(X_00060_00000>=as.numeric(wetEvents[wetEvents$gage==siteNumber,]["wetLengthQ"]))))%>%


p <- ggplot(data = dat) +
  geom_line(aes(x = dateTime,
                # y = round(X_00060_00000,1)),
            y = q_peak),
            alpha = 0.5)+
  # scale_y_continuous(trans='log10') +
  theme_classic()+
  xlab("Date")+
  ylab("Discharge (cfs)")

p <- p + geom_point(data = peaks,
                    aes(x = wet_DateTime,
                        y = q),
                    color="#C8695B",
                    shape = 15,
                    size=3)
p

plotly::ggplotly(p)
