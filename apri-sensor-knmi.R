options(warn=-1)

# example: Rscript apri-sensor-knmi.R 06323 "Wilhelminadorp" 2025-02-09T00:00:00 2025-02-10T00:00:00

## Sys.setenv(TZ="Europe/Amsterdam")
#print(sessionInfo())
#print(version)
#update.packages(checkBuilt=TRUE, ask=FALSE)

# defaults:
#scriptPath='/data/Alfresco/opt/R/apri-sensor-report/'
scriptPath='./'
subPath=paste0(scriptPath,'sub/')
reportPath=paste0(scriptPath,'report/')
configPath=paste0(scriptPath,'config/')
imagePath<-paste0(scriptPath,'image/')
cachePath<-paste0(scriptPath,'tmp/cache/')
plotPath<-paste0(scriptPath,'plot/')

source(paste0(configPath,"apri-sensor-config.R"))

#install.packages("ggplot2")
#install.packages("scales")

#library(methods)
library(testthat)
library(gsw)
#library(ggplot2)
#library(reshape2)
#try(library(scales), silent=TRUE)
#library(plyr)
#library(dplyr)
#install.packages("oce")
suppressWarnings(library(oce))
##
##
# include modules
#source("/opt/SCAPE604/R/aprisensor-fiware/aprisensor-knmi-fiware-global.R")
#source(paste0(subPath,"aprisensor-fiware-global-knmi-v1.R"))
source(paste0(subPath,"aprisensor-knmi-v1.R"))



hours<-1

args <- commandArgs(trailingOnly = TRUE)
print(args)

parmStation <- args[1]
parmStationName <- args[2]
argDateFrom <- args[3]
argDateTo <- args[4]
#parmStation <- '06323'
#parmStationName <-'test'
keeps <- c("sensorId","dateObserved","sensorValue")
actualGraph<-TRUE

print(argDateFrom)
print(argDateTo)
if (is.na(argDateFrom) || is.na(argDateTo) ) {
  actualGraph<-TRUE
  fiwareStart<-format(Sys.time()-((2+24+hours)*60*60+(60*20)),"%Y-%m-%dT%H:%M:%S") # twee meer terug als start plus 20 minuten extra
  fiwareEnd<-format(Sys.time()-((hours)*60*60),"%Y-%m-%dT%H:%M:%S")
  yesterdayStart<-format(Sys.time()-((24+2+24+hours)*60*60+(60*20)),"%Y-%m-%dT%H:%M:%S") # twee meer terug als start plus 20 minuten extra
  yesterdayEnd<-format(Sys.time()-((2+24+hours)*60*60),"%Y-%m-%dT%H:%M:%S")
  
  # fiwarePeriodStart<-format(Sys.time()-((12+hours)*60*60),"%Y-%m-%dT%H:%M:%S")
  # fiwarePeriodEnd<-format(Sys.time()-((hours)*60*60),"%Y-%m-%dT%H:%M:%S")
} else {
  actualGraph<-FALSE
  #fiwareStart<-as.POSIXct(argDateFrom, format="%Y-%m-%dT%H:%M")
  #fiwareEnd<-as.POSIXct(argDateTo, format="%Y-%m-%dT%H:%M")
  fiwareStart<-argDateFrom
  fiwareEnd<-argDateTo
  #yesterdayStart<-format(Sys.time()-((24+2+24+hours)*60*60+(60*20)),"%Y-%m-%dT%H:%M:%S") # twee meer terug als start plus 20 minuten extra
  #yesterdayEnd<-format(Sys.time()-((2+24+hours)*60*60),"%Y-%m-%dT%H:%M:%S")
  
  # fiwarePeriodStart<-format(fiwareStart,"%Y-%m-%dT%H:%M:%S")
  # fiwarePeriodEnd<-format(fiwareEnd,"%Y-%m-%dT%H:%M:%S")
  
  print("test")
  print(fiwareStart)
  print(fiwareEnd)
}

#fiwareStart='2022-02-27T00:00:00'
#fiwareEnd='2022-02-27T22:00:00'
#yesterdayStart='2022-02-26T00:00:00'
#yesterdayEnd='2022-02-26T22:00:00'
#dfTmpWind<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'windSpeed,windDirection',opPerRow='false',dateFrom=fiwareStart,dateTo=fiwareEnd)
dfTmpWind<-getStationSelectRecordsKnmi(NULL,parmStation,dateFrom=fiwareStart,dateTo=fiwareEnd)
dfTmpWind$date <- as.POSIXct(dfTmpWind$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
if (actualGraph==TRUE) {
  dfTmpWindY<-getStationSelectRecordsKnmi(NULL,parmStation,dateFrom=yesterdayStart,dateTo=yesterdayEnd)
  dfTmpWindY$date <- as.POSIXct(dfTmpWindY$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60 + (24*60*60) ;
}

if (parmStation=="06225") { # merge two stations
  keeps<-c('station','dateObserved','date','name','wd','ws')
  keeps_06257<-c('date','rainfall','pressure','rainDuration','temperature','rHum', 'solar')
  dfTmpWind_06257<-getStationSelectRecordsKnmi(NULL,'06257',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpWind_06257$date <- as.POSIXct(dfTmpWind_06257$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
  dfTmpWind_06257<-dfTmpWind_06257[keeps_06257]
  dfTmpWind<-dfTmpWind[keeps]
  dfTmpWind<-merge(dfTmpWind,dfTmpWind_06257, by= c('date'),all.x=TRUE,all.y=T,sort=T)
  if (actualGraph==TRUE) {
    dfTmpWindY_06257<-getStationSelectRecordsKnmi(NULL,'06257',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
    dfTmpWindY_06257$date <- as.POSIXct(dfTmpWindY_06257$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60 + (24*60*60) ;
    dfTmpWindY_06257<-dfTmpWindY_06257[keeps_06257]
    dfTmpWindY<-dfTmpWindY[keeps]
    dfTmpWindY<-merge(dfTmpWindY,dfTmpWindY_06257, by= c('date'),all.x=TRUE,all.y=T,sort=T)
  } 
}

total0 <- dfTmpWind

if (actualGraph==TRUE) {
  # begrenzing op max. 24 uur terug in de tijd
  ymdHM <- format(Sys.time() - as.difftime(1, unit="days"), "%y%m%d%H%M");
  # overrule e.g.
  #ymdHM <- "1809161100"
  
  total0Y <- dfTmpWindY
  
  total <- subset(total0, ( !is.na(total0$ws) & format(total0$date, "%y%m%d%H%M") >= ymdHM) )
  totalRain <- subset(total0, (!is.na(total0$rainfall) & format(total0$date, "%y%m%d%H%M") >= ymdHM))
  totalTemperature <- subset(total0,( !is.na(total0$temperature) &  format(total0$date, "%y%m%d%H%M") >= ymdHM))
  totalTemperatureY <- subset(total0Y,( !is.na(total0Y$temperature) &  format(total0Y$date, "%y%m%d%H%M") >= ymdHM))
  totalRelativeHumidity <- subset(total0,( !is.na(total0$rHum) &  format(total0$date, "%y%m%d%H%M") >= ymdHM))
  totalRelativeHumidityY <- subset(total0Y,( !is.na(total0Y$rHum) &  format(total0Y$date, "%y%m%d%H%M") >= ymdHM))
  totalSolar <- subset(total0,( !is.na(total0$solar) &  format(total0$date, "%y%m%d%H%M") >= ymdHM))
  totalSolarY <- subset(total0Y,( !is.na(total0Y$solar) &  format(total0Y$date, "%y%m%d%H%M") >= ymdHM))
} else {
  total <- subset(total0, ( !is.na(total0$ws) ) )
  totalRain <- subset(total0, (!is.na(total0$rainfall) ))
  totalTemperature <- subset(total0,( !is.na(total0$temperature) ))
  totalRelativeHumidity <- subset(total0,( !is.na(total0$rHum) ))
  totalSolar <- subset(total0,( !is.na(total0$solar) ))
}

total$angle<-(270-as.numeric(total$wd))*pi/180
total$wforce <- as.numeric(total$ws)

period <- range(total$date);
periodetext1 <- strftime(period[1], format = "%Y-%m-%d %H:%M" )
periodetext2 <- strftime(period[2], format = "%Y-%m-%d %H:%M")

foilabel = paste(parmStation,'_wind',sep='');
labels <- c('windDirection' = "Windrichting",
            'windSpeed' = "Windkracht"
);

foitext <- paste('Data van het KNMI https://data.knmi.nl/ (KNMI)',sep="");

fileprefix <- paste(plotPath,'/knmi','_',foilabel,sep='')
if (actualGraph==TRUE) {
  filedate = ''
} else {
  filedate = paste0('_',fiwareStart,'_',fiwareEnd)
}  
filename <- paste0(fileprefix, filedate,'.png');
png(filename=filename,width = 1000, height = 600 )
par(mar = c(0,0,0,0)+5)
par(new = T)
scalePosXTick=(max(total$date)-min(total$date))/100
scalePosX=min(total$date)-scalePosXTick*4
plotSticks(total$date,rep(0,3),total$wforce*cos(total$angle),
           total$wforce*sin(total$angle)
          ,lwd=0.6
          ,mar=c(5.5,7.3,5,7.3)
          ,ylim=c(-10, 10)
          ,yaxt="n"
          ,main=paste('KNMI station',parmStation,parmStationName)
          ,ylab=""
           )
lines(total$date,total$wforce,col="black", lwd=3)
lines(totalRain$date,totalRain$rainfall,col="blue", lwd=3)
axis(2, at=c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30),col = "black", col.axis = "black", las=1, lwd = 1,outer=FALSE, pos=scalePosX) #+scalePosXTick*4.20)

if(nrow(totalTemperature)>0) {
 par(new = T)
 minT=min(totalTemperature$temperature)
 maxT=max(totalTemperature$temperature)
 minTAll=minT
 maxTAll=maxT
 if (actualGraph==TRUE) {
   minTY=min(totalTemperatureY$temperature)
   maxTY=max(totalTemperatureY$temperature)
   if (minTY<minTAll) minTAll <- minTY
   if (maxTY>maxTAll) maxTAll <- maxTY
 }

 scalePosXTick=(max(totalRelativeHumidity$date)-min(totalRelativeHumidity$date))/100
 scalePosX=max(totalRelativeHumidity$date)+scalePosXTick*4
 plot(totalTemperature$date,totalTemperature$temperature,col="red", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minTAll,maxTAll))
 if (actualGraph==TRUE) lines(totalTemperatureY$date,totalTemperatureY$temperature,col="red", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
 abline(h=40.4,col="blue")
 axis(4, at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,40), col = "red", col.axis = "dark red", las=1, lwd = 1,outer=FALSE, pos=scalePosX+scalePosXTick*0.90)

}

if(nrow(totalRelativeHumidity)>0) {
 par(new = T)
 minT=min(totalRelativeHumidity$rHum)
 maxT=max(totalRelativeHumidity$rHum)
 minTAll=minT
 maxTAll=maxT
 if (actualGraph==TRUE) {
   minTY=min(totalRelativeHumidityY$rHum)
   maxTY=max(totalRelativeHumidityY$rHum)
   if (minTY<minTAll) minTAll <- minTY
   if (maxTY>maxTAll) maxTAll <- maxTY
 }
 
 plot(totalRelativeHumidity$date,totalRelativeHumidity$rHum,col="green", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minTAll,maxTAll))
 if (actualGraph==TRUE) lines(totalRelativeHumidityY$date,totalRelativeHumidityY$rHum,col="green", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
 axis(4, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),col = "green", col.axis = "dark green", las=1, lwd = 1,outer=FALSE, pos=scalePosX+scalePosXTick*4.80)
}

if(nrow(totalSolar>0)) {
 par(new = T)

 scalePosXTick=(max(totalSolar$date)-min(totalSolar$date))/100
 scalePosX=min(totalSolar$date)-scalePosXTick*8
 plot(totalSolar$date,totalSolar$solar,col="orange", lwd=3, axes=F, xlab=NA, ylab=NA, type="l")
 if (actualGraph==TRUE) lines(totalSolarY$date,totalSolarY$solar,col="orange", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
 axis(2, at=c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000),col = "orange", col.axis = "dark orange", las=1, lwd = 1,outer=FALSE, pos=scalePosX) #+scalePosXTick*4.20)
}

if (actualGraph==TRUE) {
  legend("bottomleft", legend=c("wind", "regen","zonnestraling"),
         col=c("black", "blue","orange"), lwd=3, lty=1, cex=0.9)
  legend("bottomright", legend=c("temperature","temperature-24h","rel.Humidity","rel.Humidity-24h"),
         col=c("red","red","green","green"), lwd=c(3,1,3,1), lty=c(1,2,1,2), cex=0.9)
} else {
  legend("bottomleft", legend=c("wind", "regen","zonnestraling"),
         col=c("black", "blue","orange"), lwd=3, lty=1, cex=0.9)
  legend("bottomright", legend=c("temperature","rel.Humidity"),
         col=c("red","green"), lwd=c(3,3), lty=c(1,1), cex=0.9)
  
}

mtext(side = 2, line=5, 'Wind (m/s), regen (mm) en zonnestraling (W/m2)')
mtext(side = 4, line=5, 'temperatuur C en relatieve luchtvochtigheid %')
mtext(side = 1, line=3, paste('Periode:',periodetext1,'-',periodetext2))
