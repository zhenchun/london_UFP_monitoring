
library(lattice)
library(plyr)
library(openair)
library(data.table)
library(ggplot2)
library(readr)
library(hydroTSM)
library(ggplot2)
library(chron)
library(lubridate)
library(dplyr)
library(StreamMetabolism)
library(RQuantLib)


Sys.setenv(TZ='GMT') 

########import functions first


#ufp data input
path <- "C:/Users/zy125/Box Sync/PhD/London/monitoring paper/md_nk_output"
file.names <- dir(path, pattern =".txt")
MD_NK <- list()
setwd(path)

for (f in file.names) {
  print(f)
  NKdat<- importMD_dm(f)
  NKdat_filt<- filterMDv4(NKdat[[1]])
  MD_NK[[f]] <- NKdat_filt[[1]][, c(1,4,5,6,7,8,16)] #select MD data by columns
}

MD_NK<-rbindlist(MD_NK, fill=TRUE) #bind list by rows by times 


#wind data 
#import north kensington data
nk_wind <- read_csv("C:/Users/zy125/Box Sync/PhD/London/monitoring paper/AQDH.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
            Time = col_time(format = "%H:%M:%S")),skip = 4)

heathrow$datetime<-as.POSIXct(paste(heathrow$time, heathrow$time_local))
colnames(nk_wind)<-c("date","time","wd","s1","ws","s2","tem","s3","datetime")
nk_wind_v1<-nk_wind[,c(9,3,5,7)]



########heathrow met data##################################

heathrow <- read_csv("C:/Users/zy125/Box Sync/PhD/London/monitoring paper/heathrow_airport_mete_data.csv", 
                     col_types = cols(time = col_date(format = "%m/%d/%Y"), 
                     time_local = col_number()))


heathrow$datetime<-as.POSIXct(paste(heathrow$time, heathrow$time_local), format="%Y-%m-%d %H")



######################################################################################################
###############################average them into hourly data##########################################
######################################################################################################

MD_NK$hour<-cut(MD_NK$PosixTime, breaks="hour")
MD_NK$day<-cut(MD_NK$PosixTime, breaks="day")
MD_NK$weekday<-wday(MD_NK$PosixTime)
MD_NK$month<-cut(MD_NK$PosixTime, breaks="month")

MDnk_hour_number<-aggregate(Number ~ hour, MD_NK, mean)
MDnk_hour_size<-aggregate(Size ~ hour, MD_NK, mean)

MDnk_hour_number$day<-as.Date(paste(MDnk_hour_number$hour), format="%Y-%m-%d")
colnames(MDnk_hour_number)<-c("datetime", "number", "day")
MDnk_hour_number$datetime<-as.POSIXct(paste(MDnk_hour_number$datetime), format="%Y-%m-%d %H:%M:%S")
colnames(MDnk_hour_size)<-c("datetime", "size")
MDnk_hour_size$datetime<-as.POSIXct(paste(MDnk_hour_size$datetime), format="%Y-%m-%d %H:%M:%S")
MDnk_hour<-merge(MDnk_hour_number, MDnk_hour_size, by="datetime", all=TRUE)
MDnk_hour1<-merge(MDnk_hour, nk_wind_v1, by="datetime")

MDnk_hour2<-merge(MDnk_hour, heathrow, by="datetime")
MDnk_hour2<-MDnk_hour2[, -c(5,6)]


dat_nk<-MDnk_hour1
dat_nk_htr<-MDnk_hour2


attr(dat_nk$datetime, "tzone") <- "GMT"
attr(dat_nk_htr$datetime, "tzone") <- "GMT"

#################################################################
##################add season information#########################

dat_nk$season<-time2season(dat_nk$datetime, out.fmt="seasons")

dat_nk_htr$season<-time2season(dat_nk_htr$datetime, out.fmt="seasons")

############################################################################################
#####################add weekdays and weekend info##########################################
###########################################################################################

dat_nk_htr$weekdays<-as.factor(ifelse(weekdays(dat_nk_htr$datetime) %in% c("Saturday", "Sunday"), "weekend", "weekday"))






##################################################################################################
#####################add day and night info#######################################################
##################################################################################################

Sys.setenv(TZ='GMT') #change the system to UK to make sure the output is in the right timezone

for (i in 1: nrow(dat_nk)){
  dat_nk[i,10]<-sunrise.set(51.521050, -0.213492, dat_nk[i,3], timezone="GB", num.days=1)[1,1]
  
  
  dat_nk[i,11]<-sunrise.set(51.521050, -0.213492, dat_nk[i,3], timezone="GB", num.days=1)[1,2]
}


colnames(dat_nk)[c(10,11)]<-c("sunrise", "sunset")


dat_nk$dayNight<-ifelse(dat_nk$datetime > dat_nk$sunrise & dat_nk$datetime < dat_nk$sunset, 'day', 'night')


#####heathrow

Sys.setenv(TZ='GMT') #change the system to UK to make sure the output is in the right timezone

for (i in 1: nrow(dat_nk_htr)){
  dat_nk_htr[i,12]<-sunrise.set(51.521050, -0.213492, dat_nk_htr[i,3], timezone="GB", num.days=1)[1,1]
  
  
  dat_nk_htr[i,13]<-sunrise.set(51.521050, -0.213492, dat_nk_htr[i,3], timezone="GB", num.days=1)[1,2]
}


colnames(dat_nk_htr)[c(12,13)]<-c("sunrise", "sunset")


dat_nk_htr$dayNight<-ifelse(dat_nk_htr$datetime > dat_nk_htr$sunrise & dat_nk_htr$datetime < dat_nk_htr$sunset, 'day', 'night')






###############################################################################################
#####################add peak and off-peak#######################################################
###############################################################################################
dat_nk<-dat_nk %>%
  mutate(peak = case_when(
    is.weekend(as.Date(datetime)) ~ FALSE,
    (hour(datetime)==6 & minute(datetime)>=30 | hour(datetime)>6) & 
      (hour(datetime)==9 & minute(datetime)<=30 | hour(datetime)<9) ~ TRUE,
    (hour(datetime)>=16 & hour(datetime)<=19) ~ TRUE,
    TRUE ~ FALSE)
  )



dat_nk_htr<-dat_nk_htr %>%
  mutate(peak = case_when(
    is.weekend(as.Date(datetime)) ~ FALSE,
    (hour(datetime)==6 & minute(datetime)>=30 | hour(datetime)>6) & 
      (hour(datetime)==9 & minute(datetime)<=30 | hour(datetime)<9) ~ TRUE,
    (hour(datetime)>=16 & hour(datetime)<=19) ~ TRUE,
    TRUE ~ FALSE)
  )


##################################################################################################
#####################add business day info########################################################
##################################################################################################


dat_nk$businessday<-isBusinessDay(calendar="UnitedKingdom", date=dat_nk$day)
dat_nk$wdays<-weekdays(dat_nk$day)
dat_nk$hours<-hour(dat_nk$datetime)

dat_nk_htr$businessday<-isBusinessDay(calendar="UnitedKingdom", date=dat_nk_htr$day)
dat_nk_htr$wdays<-weekdays(dat_nk_htr$day)
dat_nk_htr$hours<-hour(dat_nk_htr$datetime)


###############################################################################################
########################pm2.5#################################################################
##############################################################################################


nk_pm25<- read.csv("C:/Users/zy125/Box Sync/PhD/London/monitoring paper/NK_pm25.csv",skip = 4)
nk_pm25$datetime<-as.POSIXct(paste(nk_pm25$Date, nk_pm25$Time), format="%d/%m/%Y %H:%M:%S")
colnames(nk_pm25)<-c("date1","time","PM2.5","Status","datetime")
nk_pm25<-nk_pm25[,c(5,3)]
nk_pm25<-as.data.frame(nk_pm25)

nk_pm25<-na.omit(nk_pm25)


dat_nk<-merge(dat_nk, nk_pm25, by="datetime", all.x=TRUE)


#################################################################################################
##############################plot##############################################################
ggplot(data=MDnk_hour2, aes(x=time2, y=number))+stat_smooth(method="gam", formula=y~s(x), size=1)+
  scale_x_continuous(breaks=seq(0,24,3))+theme_classic()






###############################################################################################
########streatham green ufp data input###############################################################
###############################################################################################
path <- "C:/Users/zy125/Box Sync/PhD/London/monitoring paper/md_st_output"
file.names <- dir(path, pattern =".txt")
MD_ST <- list()
setwd(path)

for (f in file.names) {
  print(f)
  STdat<- importMD_dm(f)
  STdat_filt<- filterMDv4(STdat[[1]])
  MD_ST[[f]] <- STdat_filt[[1]][, c(1,4,5,6,7,8,16)] #select MD data by columns
}


MD_ST<-rbindlist(MD_ST, fill=TRUE) #bind list by rows by times 



######################################################################################################
###############################average them into hourly data##########################################
######################################################################################################

MD_ST$hour<-cut(MD_ST$PosixTime, breaks="hour")
MD_ST$day<-cut(MD_ST$PosixTime, breaks="day")
MD_ST$weekday<-wday(MD_ST$PosixTime)
MD_ST$month<-cut(MD_ST$PosixTime, breaks="month")

MDst_hour_number<-aggregate(Number ~ hour, MD_ST, mean)
MDst_hour_size<-aggregate(Size ~ hour, MD_ST, mean)

MDst_hour_number$day<-as.Date(paste(MDst_hour_number$hour), format="%Y-%m-%d")
colnames(MDst_hour_number)<-c("datetime", "number", "day")
MDst_hour_number$datetime<-as.POSIXct(paste(MDst_hour_number$datetime), format="%Y-%m-%d %H:%M:%S")
colnames(MDst_hour_size)<-c("datetime", "size")
MDst_hour_size$datetime<-as.POSIXct(paste(MDst_hour_size$datetime), format="%Y-%m-%d %H:%M:%S")
MDst_hour<-merge(MDst_hour_number, MDst_hour_size, by="datetime", all=TRUE)

MDst_hour1<-merge(MDst_hour, heathrow, by="datetime")
MDst_hour1<-MDst_hour1[, -c(5,6)]

dat_st<-MDst_hour1




attr(dat_st$datetime, "tzone") <- "GMT"


#################################################################
##################add season information#########################

dat_st$season<-time2season(dat_st$datetime, out.fmt="seasons")


############################################################################################
#####################add weekdays and weekend info##########################################
###########################################################################################

dat_st$weekdays<-as.factor(ifelse(weekdays(dat_st$datetime) %in% c("Saturday", "Sunday"), "weekend", "weekday"))




#####################add day and night info#######################################################
##################################################################################################

Sys.setenv(TZ='GMT') #change the system to UK to make sure the output is in the right timezone

for (i in 1: nrow(dat_st)){
  dat_st[i,12]<-sunrise.set(51.521050, -0.213492, dat_st[i,3], timezone="GB", num.days=1)[1,1]
  
  
  dat_st[i,13]<-sunrise.set(51.521050, -0.213492, dat_st[i,3], timezone="GB", num.days=1)[1,2]
}


colnames(dat_st)[c(12,13)]<-c("sunrise", "sunset")


dat_st$dayNight<-ifelse(dat_st$datetime > dat_st$sunrise & dat_st$datetime < dat_st$sunset, 'day', 'night')


###############################################################################################
#####################add peak and off-peak#######################################################
###############################################################################################
dat_st<-dat_st %>%
  mutate(peak = case_when(
    is.weekend(as.Date(datetime)) ~ FALSE,
    (hour(datetime)==6 & minute(datetime)>=30 | hour(datetime)>6) & 
      (hour(datetime)==9 & minute(datetime)<=30 | hour(datetime)<9) ~ TRUE,
    (hour(datetime)>=16 & hour(datetime)<=19) ~ TRUE,
    TRUE ~ FALSE)
  )





##################################################################################################
#####################add business day info########################################################
##################################################################################################


dat_st$businessday<-isBusinessDay(calendar="UnitedKingdom", date=dat_st$day)
dat_st$wdays<-weekdays(dat_st$day)
dat_st$hours<-hour(dat_st$datetime)




###############################################################################################
########wenlock road  ufp data input###############################################################
###############################################################################################
path <- "C:/Users/zy125/Box Sync/PhD/London/monitoring paper/md_wl_output"
file.names <- dir(path, pattern =".txt")
MD_WL <- list()
setwd(path)

for (f in file.names) {
  print(f)
  WLdat<- importMD_dm(f)
  WLdat_filt<- filterMDv4(WLdat[[1]])
  MD_WL[[f]] <- WLdat_filt[[1]][, c(1,4,5,6,7,8,16)] #select MD data by columns
}


MD_WL<-rbindlist(MD_WL, fill=TRUE) #bind list by rows by times 



######################################################################################################
###############################average them into hourly data##########################################
######################################################################################################

MD_WL$hour<-cut(MD_WL$PosixTime, breaks="hour")
MD_WL$day<-cut(MD_WL$PosixTime, breaks="day")
MD_WL$weekday<-wday(MD_WL$PosixTime)
MD_WL$month<-cut(MD_WL$PosixTime, breaks="month")

MDwl_hour_number<-aggregate(Number ~ hour, MD_WL, mean)
MDwl_hour_size<-aggregate(Size ~ hour, MD_WL, mean)

MDwl_hour_number$day<-as.Date(paste(MDwl_hour_number$hour), format="%Y-%m-%d")
colnames(MDwl_hour_number)<-c("datetime", "number", "day")
MDwl_hour_number$datetime<-as.POSIXct(paste(MDwl_hour_number$datetime), format="%Y-%m-%d %H:%M:%S")
colnames(MDwl_hour_size)<-c("datetime", "size")
MDwl_hour_size$datetime<-as.POSIXct(paste(MDwl_hour_size$datetime), format="%Y-%m-%d %H:%M:%S")
MDwl_hour<-merge(MDwl_hour_number, MDwl_hour_size, by="datetime", all=TRUE)

MDwl_hour1<-merge(MDwl_hour, heathrow, by="datetime")
MDwl_hour1<-MDwl_hour1[, -c(5,6)]

dat_wl<-MDwl_hour1




attr(dat_wl$datetime, "tzone") <- "GMT"


#################################################################
##################add season information#########################

dat_wl$season<-time2season(dat_wl$datetime, out.fmt="seasons")


############################################################################################
#####################add weekdays and weekend info##########################################
###########################################################################################

dat_wl$weekdays<-as.factor(ifelse(weekdays(dat_wl$datetime) %in% c("Saturday", "Sunday"), "weekend", "weekday"))




#####################add day and night info#######################################################
##################################################################################################

Sys.setenv(TZ='GMT') #change the system to UK to make sure the output is in the right timezone

for (i in 1: nrow(dat_wl)){
  dat_wl[i,12]<-sunrise.set(51.521050, -0.213492, dat_wl[i,3], timezone="GB", num.days=1)[1,1]
  
  
  dat_wl[i,13]<-sunrise.set(51.521050, -0.213492, dat_wl[i,3], timezone="GB", num.days=1)[1,2]
}


colnames(dat_wl)[c(12,13)]<-c("sunrise", "sunset")


dat_wl$dayNight<-ifelse(dat_wl$datetime > dat_wl$sunrise & dat_wl$datetime < dat_wl$sunset, 'day', 'night')


###############################################################################################
#####################add peak and off-peak#######################################################
###############################################################################################
dat_wl<-dat_wl %>%
  mutate(peak = case_when(
    is.weekend(as.Date(datetime)) ~ FALSE,
    (hour(datetime)==6 & minute(datetime)>=30 | hour(datetime)>6) & 
      (hour(datetime)==9 & minute(datetime)<=30 | hour(datetime)<9) ~ TRUE,
    (hour(datetime)>=16 & hour(datetime)<=19) ~ TRUE,
    TRUE ~ FALSE)
  )





##################################################################################################
#####################add business day info########################################################
##################################################################################################


dat_wl$businessday<-isBusinessDay(calendar="UnitedKingdom", date=dat_wl$day)
dat_wl$wdays<-weekdays(dat_wl$day)
dat_wl$hours<-hour(dat_wl$datetime)


################################################################plot

ggplot(data=hrs, aes(x=hours, y=number, color=station))+stat_smooth(method="gam", formula=y~s(x), size=1)+
  scale_x_continuous(breaks=seq(0,24,3))+theme_classic()

hrs<-rbind(dat_nk_htr[,c(2, 18,19)], dat_st[,c(2, 18,19)], dat_wl[,c(2, 18,19)])

wdays<-rbind(dat_nk_htr[,c(2, 17,19)], dat_st[,c(2, 17,19)], dat_wl[,c(2, 17,19)])

