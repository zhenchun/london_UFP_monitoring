
library(lattice)
library(plyr)
library(openair)
library(data.table)
library(ggplot2)
library(readr)


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

nk_wind$datetime<-as.POSIXct(paste(nk_wind$Date, nk_wind$Time), format="%Y-%m-%d %H:%M:%S")
colnames(nk_wind)<-c("date","time","wd","s1","ws","s2","tem","s3","datetime")
nk_wind_v1<-nk_wind[,c(9,3,5,7)]



#average them into hourly data


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







