options(warn=-1)
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
source(paste0(subPath,"aprisensor-fiware-global-knmi-v1.R"))



hours<-1
meanMinutes <- 1  # 1 or 5

args <- commandArgs(trailingOnly = TRUE)
print(args)

parmStation <- args[1]
parmStationName <- args[2]
#parmStation <- '06323'
#parmStationName <-'test'
keeps <- c("sensorId","dateObserved","sensorValue")

fiwareStart<-format(Sys.time()-((2+24+hours)*60*60+(60*20)),"%Y-%m-%dT%H:%M:%S") # twee meer terug als start plus 20 minuten extra
fiwareEnd<-format(Sys.time()-((hours)*60*60),"%Y-%m-%dT%H:%M:%S")
yesterdayStart<-format(Sys.time()-((24+2+24+hours)*60*60+(60*20)),"%Y-%m-%dT%H:%M:%S") # twee meer terug als start plus 20 minuten extra
yesterdayEnd<-format(Sys.time()-((2+24+hours)*60*60),"%Y-%m-%dT%H:%M:%S")

fiwarePeriodStart<-format(Sys.time()-((12+hours)*60*60),"%Y-%m-%dT%H:%M:%S")
fiwarePeriodEnd<-format(Sys.time()-((hours)*60*60),"%Y-%m-%dT%H:%M:%S")

#fiwareStart='2022-02-27T00:00:00'
#fiwareEnd='2022-02-27T22:00:00'
#yesterdayStart='2022-02-26T00:00:00'
#yesterdayEnd='2022-02-26T22:00:00'
dfTmpWind<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'windSpeed,windDirection',opPerRow='false',dateFrom=fiwareStart,dateTo=fiwareEnd)

if (parmStation=="06225") {
  dfTmpRain<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','rainfall',dateFrom=fiwareStart,dateTo=fiwareEnd) #,rainDuration')
  dfTmpTemperature<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','temperature',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpTemperatureY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','temperature',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
  dfTmpRelativeHumidity<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','relativeHumidity',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpRelativeHumidityY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','relativeHumidity',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
  dfTmpSolar<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','solar',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpSolarY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station','06257','solar',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
} else{
  dfTmpRain<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'rainfall',dateFrom=fiwareStart,dateTo=fiwareEnd) #,rainDuration')
  dfTmpTemperature<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'temperature',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpTemperatureY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'temperature',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
  dfTmpRelativeHumidity<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'relativeHumidity',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpRelativeHumidityY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'relativeHumidity',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
  dfTmpSolar<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'solar',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpSolarY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'solar',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
}
#head(dfTmpWind)
#head(dfTmpWind)
#head(dfTmpTemperature)
#head(dfTmpRelativeHumidity)
#dfTmp<-''
#dfTmp<-fiwareGetRecordsKnmi(dfTmp,fiwarePeriodStart,fiwarePeriodEnd,'knmi','/knmi',parmStation)  # ,station
##saveRDS(dfTmp, file = paste("/opt/SCAPE604/R/datasets/knmi/","knmi_",parmStation,"_",fiwarePeriodStart,"-",fiwarePeriodStart,".rds",sep=''))
##dfTmp<-fiwareGetRecordsKnmi(dfTmp,fiwarePeriodStart,fiwarePeriodEnd,'knmi','/knmi','06203')  # ,station
##dfTmp<-fiwareGetRecordsKnmi(dfTmp,fiwarePeriodStart,fiwarePeriodEnd,'knmi','/knmi','06203')  # ,station

#set.seed(1)
#df       <- expand.grid(x = 1:10, y=1:10)
#df$angle <- runif(100, 0, 2*pi)
#ggplot(df, aes(x, y)) +
#  geom_point()        +
#  coord_equal()       +
#  geom_spoke(aes(angle = angle), radius = 0.7, arrow=arrow(length = unit(0.2,"cm")))

#onehour <- 1*60*60
#plushours <- onehour*2

meanMinutes <- 10 #  1 or 5 or 60

dfTmpWind$date <- as.POSIXct(dfTmpWind$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpRain$date <- as.POSIXct(dfTmpRain$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpTemperature$date <- as.POSIXct(dfTmpTemperature$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpTemperatureY$date <- as.POSIXct(dfTmpTemperatureY$dateObserved, format="%Y-%m-%dT%H:%M")+ (24*60*60) + (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpRelativeHumidity$date <- as.POSIXct(dfTmpRelativeHumidity$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpRelativeHumidityY$date <- as.POSIXct(dfTmpRelativeHumidityY$dateObserved, format="%Y-%m-%dT%H:%M")+ (24*60*60) + (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpSolar$date <- as.POSIXct(dfTmpSolar$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpSolarY$date <- as.POSIXct(dfTmpSolarY$dateObserved, format="%Y-%m-%dT%H:%M")+ (24*60*60) + (as.numeric(format(Sys.time(),'%z'))/100)*60*60;


# begrenzing op max. 24 uur terug in de tijd
ymdHM <- format(Sys.time() - as.difftime(1, unit="days"), "%y%m%d%H%M");
# overrule e.g.
ymdHM <- "1809161100"

total0 <- dfTmpWind

total <- subset(total0, format(total0$date, "%y%m%d%H%M") >= ymdHM)
totalRain <- subset(dfTmpRain, format(dfTmpRain$date, "%y%m%d%H%M") >= ymdHM)
totalTemperature <- subset(dfTmpTemperature, format(dfTmpTemperature$date, "%y%m%d%H%M") >= ymdHM)
#head(dfTmpTemperature)
#head(dfTmpTemperatureY)
totalTemperatureY <- subset(dfTmpTemperatureY, format(dfTmpTemperatureY$date, "%y%m%d%H%M") >= ymdHM)
totalRelativeHumidity <- subset(dfTmpRelativeHumidity, format(dfTmpRelativeHumidity$date, "%y%m%d%H%M") >= ymdHM)
totalRelativeHumidityY <- subset(dfTmpRelativeHumidityY, format(dfTmpRelativeHumidityY$date, "%y%m%d%H%M") >= ymdHM)
totalSolar <- subset(dfTmpSolar, format(dfTmpSolar$date, "%y%m%d%H%M") >= ymdHM)
totalSolarY <- subset(dfTmpSolarY, format(dfTmpSolarY$date, "%y%m%d%H%M") >= ymdHM)

total$angle<-(270-as.numeric(total$windDirection))*pi/180
total$wforce <- as.numeric(total$windSpeed)

period <- range(total$date);
periodetext1 <- strftime(period[1], format = "%Y-%m-%d %H:%M" )
periodetext2 <- strftime(period[2], format = "%Y-%m-%d %H:%M")

foilabel = paste(parmStation,'_wind',sep='');
labels <- c('windDirection' = "Windrichting",
            'windSpeed' = "Windkracht"
);

foitext <- paste('Data van het KNMI https://data.knmi.nl/ (KNMI)',sep="");

fileprefix <- paste(plotPath,'/knmi','_',foilabel,sep='')
filedate = ''
filename <- paste(fileprefix, filedate,'.png',sep='');
png(filename=filename,width = 1000, height = 600 )
par(mar = c(0,0,0,0)+5)
par(new = T)
#par(mar = c(0,0,0,0)+5)
#set_plot_dimensions(4, 4)
scalePosXTick=(max(total$date)-min(total$date))/100
scalePosX=min(total$date)-scalePosXTick*4
plotSticks(total$date,rep(0,3),total$wforce*cos(total$angle),
           total$wforce*sin(total$angle)
          #,yscale=0.4
          ,lwd=0.6
        #  ,mar=c(4,4,3,6)
        #  ,mar=c(4.5,5.2,3,6.5)
          ,mar=c(5.5,7.3,5,7.3)
          #,ylim=c(-10, 10)
          ,ylim=c(-10, 10)
          ,yaxt="n"
          ,main=paste('KNMI station',parmStation,parmStationName)
#          ,xlab=paste('KNMI station',parmStation,parmStationName)
#          ,ylab="Wind (m/s), regen (mm) en zonnestraling (W/m2)"
          ,ylab=""
           )
lines(total$date,total$wforce,col="black", lwd=3)
lines(totalRain$date,totalRain$sensorValue,col="blue", lwd=3)
axis(2, at=c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30),col = "black", col.axis = "black", las=1, lwd = 1,outer=FALSE, pos=scalePosX) #+scalePosXTick*4.20)

par(new = T)
minT=min(totalTemperature$sensorValue)
maxT=max(totalTemperature$sensorValue)
minTY=min(totalTemperatureY$sensorValue)
maxTY=max(totalTemperatureY$sensorValue)
minTAll=minT
if (minTY<minTAll) minTAll <- minTY
maxTAll=maxT
if (maxTY>maxTAll) maxTAll <- maxTY

scalePosXTick=(max(totalRelativeHumidity$date)-min(totalRelativeHumidity$date))/100
scalePosX=max(totalRelativeHumidity$date)+scalePosXTick*4
plot(totalTemperature$date,totalTemperature$sensorValue,col="red", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minTAll,maxTAll))
lines(totalTemperatureY$date,totalTemperatureY$sensorValue,col="red", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
abline(h=40.4,col="blue")
axis(4, at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,40), col = "red", col.axis = "dark red", las=1, lwd = 1,outer=FALSE, pos=scalePosX+scalePosXTick*0.90)


par(new = T)
minT=min(totalRelativeHumidity$sensorValue)
maxT=max(totalRelativeHumidity$sensorValue)
minTY=min(totalRelativeHumidityY$sensorValue)
maxTY=max(totalRelativeHumidityY$sensorValue)
minTAll=minT
if (minTY<minTAll) minTAll <- minTY
maxTAll=maxT
if (maxTY>maxTAll) maxTAll <- maxTY

plot(totalRelativeHumidity$date,totalRelativeHumidity$sensorValue,col="green", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minTAll,maxTAll))
lines(totalRelativeHumidityY$date,totalRelativeHumidityY$sensorValue,col="green", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
axis(4, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),col = "green", col.axis = "dark green", las=1, lwd = 1,outer=FALSE, pos=scalePosX+scalePosXTick*4.80)

if(nrow(totalSolar>0)) {
par(new = T)

scalePosXTick=(max(totalSolar$date)-min(totalSolar$date))/100
scalePosX=min(totalSolar$date)-scalePosXTick*8
#plot(totalSolar$date,totalSolar$sensorValue,col="orange", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minSAll,maxSAll))
plot(totalSolar$date,totalSolar$sensorValue,col="orange", lwd=3, axes=F, xlab=NA, ylab=NA, type="l")
lines(totalSolarY$date,totalSolarY$sensorValue,col="orange", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
axis(2, at=c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000),col = "orange", col.axis = "dark orange", las=1, lwd = 1,outer=FALSE, pos=scalePosX) #+scalePosXTick*4.20)
 # test
}

legend("bottomleft", legend=c("wind", "regen","zonnestraling"),
       col=c("black", "blue","orange"), lwd=3, lty=1, cex=0.9)
legend("bottomright", legend=c("temperature","temperature-24h","rel.Humidity","rel.Humidity-24h"),
       col=c("red","red","green","green"), lwd=c(3,1,3,1), lty=c(1,2,1,2), cex=0.9)

mtext(side = 2, line=5, 'Wind (m/s), regen (mm) en zonnestraling (W/m2)')
mtext(side = 4, line=5, 'temperatuur C en relatieve luchtvochtigheid %')
mtext(side = 1, line=3, paste('Periode:',periodetext1,'-',periodetext2))
