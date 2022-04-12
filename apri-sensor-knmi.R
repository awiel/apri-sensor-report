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
} else{
  dfTmpRain<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'rainfall',dateFrom=fiwareStart,dateTo=fiwareEnd) #,rainDuration')
  dfTmpTemperature<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'temperature',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpTemperatureY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'temperature',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
  dfTmpRelativeHumidity<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'relativeHumidity',dateFrom=fiwareStart,dateTo=fiwareEnd)
  dfTmpRelativeHumidityY<-fiwareGetSensorSelectRecordsKnmi(NULL,'knmi','/knmi','station',parmStation,'relativeHumidity',dateFrom=yesterdayStart,dateTo=yesterdayEnd)
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

#fois_knmi10m <- ""
##fois_knmi10m <- "&foi=apri-sensor-knmi10m_KNMI06330";
##fois_knmi10m <- "&foi=apri-sensor-knmi10m_KNMI06380";
#periode <- ''
##periode <- '&date_start=2018-05-26T06:00:00+01:00&date_end=2018-12-31T00:00:00+01:00'
##periode <- "&date_start=3d";
#
#urlKnmi10m <- paste("https://openiod.org/SCAPE604/openiod?SERVICE=WPS&REQUEST=Execute&identifier=transform_observation&action=getobservation"
#                      , "&sensorsystem=apri-sensor-knmi10m"
#                      , periode
#                      , fois_knmi10m
#                      #, "&op=apri-sensor-luchtmeetnet-PM25"
#                      ,"&offering=offering_knmi10m_initial&format=csv",sep="");
#knmi10m <- read.csv(urlKnmi10m, header = FALSE, sep = ";", quote = "\"")
#knmi10m$date <- as.POSIXct(knmi10m$V5, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;#+ plushours # add 1 hours;
#knmi10m$minute <- sapply(format(knmi10m$date, "%M"), as.numeric)
#knmi10m$sensorValue <- knmi10m$V6
#knmi10m$sensorType <- knmi10m$V3
#knmi10m$foi <- knmi10m$V4
#knmi10m$date <- knmi10m$date - ( knmi10m$minute %% meanMinutes)*60 # +meanMinutes*60 # gemiddelde per 5 min
#knmi10m$treshold1 <-0
#knmi10m$treshold1[which(knmi10m$sensorType == "apri-sensor-knmi10m-dd")] <- 10
#knmi10m$treshold1[which(knmi10m$sensorType == "apri-sensor-knmi10m-ff")] <- 10
##dfKnmi10m <- aggregate(cbind(treshold1, sensorValue)~foi+sensorType+date, data=knmi10m, mean, na.rm=TRUE)
#head(dfTmpRain)
dfTmpWind$date <- as.POSIXct(dfTmpWind$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpRain$date <- as.POSIXct(dfTmpRain$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpTemperature$date <- as.POSIXct(dfTmpTemperature$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpTemperatureY$date <- as.POSIXct(dfTmpTemperatureY$dateObserved, format="%Y-%m-%dT%H:%M")+ (24*60*60) + (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpRelativeHumidity$date <- as.POSIXct(dfTmpRelativeHumidity$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
dfTmpRelativeHumidityY$date <- as.POSIXct(dfTmpRelativeHumidityY$dateObserved, format="%Y-%m-%dT%H:%M")+ (24*60*60) + (as.numeric(format(Sys.time(),'%z'))/100)*60*60;

#dfTmpRelativeHumidity$sensorValue<-((dfTmpRelativeHumidity$sensorValue) / 10)

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

#total$tmp2 = as.character(total$foi);
#total$foiLocation=factor(substr(total$tmp2,regexpr('[SCWM][SCRP].*$',total$tmp2),50)); #[SCRP]
#total$foiLocation=factor(substr(total$tmp2,regexpr('[LU].*$',total$tmp2),20)); #[SCRP]
#total$foiLocation=factor(substr(total$tmp2,regexpr('[SCWM].*$',total$tmp2),50)); #[SCRP]
#total$foiLocation=factor(substr(total$tmp2,regexpr('[SCRP].*$',total$tmp2),50)); #[SCRP]
#total$foiLocation=factor(substr(total$tmp2,regexpr('[LUFT][SCWM][SCRP].*$',total$tmp2),40)); #[SCRP]
#total$foiLocation=factor(substr(total$tmp2,regexpr('[SL].*$',total$tmp2),50)); #[SCRP]
#total$foiLocation=factor(substr(total$tmp2,regexpr('[LUFT].*$',total$tmp2),50));
#total$foiLocation=factor(substr(total$tmp2,regexpr('[K].*$',total$tmp2),50)); #[KNMI]

#total$sensorType = factor(total$sensorType,
#                          levels=c(
#                             'apri-sensor-knmi-dd',
#                             'apri-sensor-knmi-ff'
#                          ))
#total_dd <- subset(total, (total$sensorType == "apri-sensor-knmi-dd"))
#total_dd$angle <- (270-total_dd$sensorValue)*pi/180 #(total_dd$sensorValue-20)*0.01745329252   #-20)*(pi/180) #(total_dd$sensorValue/180)*pi
#total_ff <- subset(total, (total$sensorType == "apri-sensor-knmi-ff"))
##total_ff$wforce <- total_ff$sensorValue
#total_dd$wforce <- total_ff$sensorValue # /10
#total <- cbind(total_dd) #cbind(total_dd,total_ff)


#p
#total$wforce
#total$angle
#total_dd$sensorValue
#print(total)
#print(total$windDirection)
#print(270-as.numeric(total$windDirection))
total$angle<-(270-as.numeric(total$windDirection))*pi/180
total$wforce <- as.numeric(total$windSpeed)

period <- range(total$date);
periodetext1 <- strftime(period[1], format = "%Y-%m-%d %H:%M" )
periodetext2 <- strftime(period[2], format = "%Y-%m-%d %H:%M")

foilabel = paste(parmStation,'_wind',sep='');
labels <- c('windDirection' = "Windrichting",
            'windSpeed' = "Windkracht"
);

#foitext <- 'Dylos Aalten en Purmerend'
#foitext <- paste('Dylos, Josuino, ILM en PMS7003 sensoren in ','Aalten, \'s-Gravenpolder, Purmerend, Eindhoven, Breda, Helmond',sep="");
foitext <- paste('Data van het KNMI https://data.knmi.nl/ (KNMI)',sep="");
#foitext <- paste('Testbank: twee PMSA003 sensoren in binnenruimte', '\nTest: het afsteken van één lucifer! en openen raam om 08:30 uur',sep="");

#11 = 11 minuten

#total$type = c(0, cumsum(diff(total$date) >36000))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn
#total$type = c(0, cumsum(diff(total$date) >60))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn
#total$type = c(0, cumsum(diff(total$date) >300))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn

#gTotal <-
#  ggplot(data=total, aes(x=date,y=wforce,angle = angle) ) +
  #coord_cartesian(ylim = c(0, 200)) +
  #  scale_y_continuous(breaks = seq(-0,-110,-10), "y_first", sec.axis=sec_axis(~.*0.025+2.75, name="y_second") )
 # scale_y_continuous( sec.axis=sec_axis(~.*1) ) +
#  theme(text = element_text(size = rel(3.5))
#        , plot.title = element_text(face="bold",size = rel(5), hjust = 0.5)  #lineheight=rel(1),
#        , plot.subtitle=element_text(size = rel(3), hjust = 0.5) #,face="bold")
#        , plot.caption =element_text(size = rel(2.5))
#        , axis.text.y.right=element_text(size = rel(1))
# #       , strip.text=element_text(hjust=-5)
# #       , strip.background = element_blank()
# #       ,line = element_line(size=1,colour = "red", linetype = "dotted")
#        , legend.text=element_text(size = rel(3))
#        , legend.title=element_text(size = rel(3),face="bold")
#  )  + #
#  geom_line(aes(y=treshold1,color='grenswaarde',group=interaction(sensorType,type)),size=0.3)+
  #  geom_line(aes(colour=foi,group=interaction(foi,sensorType,type)),size=0.5)+ #group=foi))+#
#  geom_point(aes(colour=foi),size=0.5)+ #group=foi))+#
  #    geom_line(aes(colour=foiLocation
                  #,group=interaction(foi,sensorType,type)
#                  ),size=0.5)+ #group=foi))+#
 # geom_point()        +
 # coord_equal()       +
#    geom_spoke(radius = 0.7, arrow=arrow(length = unit(0.2,"cm")))
#  geom_spoke( radius = 0.7, arrow=arrow(angle=total$wdirection,length =unit(0.2,"cm"))) + # + #length = 4)) + #unit(0.2,"cm"))) +
#
#tail(totalTemperature)
#print(total)
#path <- "/opt/SCAPE604/images/R/apri-sensor/"
#fileprefix <- paste("",foitext,'_',foilabel,sep='')
fileprefix <- paste(plotPath,'knmi','_',foilabel,sep='')
filedate = ''
filename <- paste(fileprefix, filedate,'.png',sep='');
#ggsave(filename, path=filepath, width=12, height=5, plot = p)
#print(filename)
#dev.off()
#png(filename=filename, width=4, height=6)
png(filename=filename,width = 1000, height = 600 )
par(mar = c(0,0,0,0)+5)
#par(mar = c(15,15,15,15))
par(new = T)
par(mar = c(0,0,0,0)+5)
#set_plot_dimensions(4, 4)
plotSticks(total$date,rep(0,3),total$wforce*cos(total$angle),
           total$wforce*sin(total$angle)
          #,yscale=0.4
          ,lwd=0.6
          ,mar=c(4,4,3,6)
          #,ylim=c(-10, 10)
          ,ylim=c(-10, 25)
          ,main=paste('KNMI station',parmStation,parmStationName)
#          ,xlab=paste('KNMI station',parmStation,parmStationName)
          ,ylab="Wind en regen"
           )
lines(total$date,total$wforce,col="black", lwd=3)
lines(totalRain$date,totalRain$sensorValue,col="blue", lwd=3)
#lines(totalTemperature$date,totalTemperature$sensorValue,col="red", lwd=3)
#axis(side=4,gap.axis=1/4)
par(new = T)
minT=min(totalTemperature$sensorValue)
maxT=max(totalTemperature$sensorValue)
minTY=min(totalTemperatureY$sensorValue)
maxTY=max(totalTemperatureY$sensorValue)
minTAll=minT
if (minTY<minTAll) minTAll <- minTY
maxTAll=maxT
if (maxTY>maxTAll) maxTAll <- maxTY
print('min en max temp')
print(minTAll)
print(maxTAll)

scalePosXTick=(max(totalRelativeHumidity$date)-min(totalRelativeHumidity$date))/100
scalePosX=max(totalRelativeHumidity$date)+scalePosXTick*4
print(scalePosX)
plot(totalTemperature$date,totalTemperature$sensorValue,col="red", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minTAll,maxTAll))
lines(totalTemperatureY$date,totalTemperatureY$sensorValue,col="red", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
abline(h=40.4,col="blue")
axis(4, at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,40), col = "red", col.axis = "dark red", las=1, lwd = 1,outer=FALSE, pos=scalePosX+scalePosXTick*0.80)
#axis(4, at=c(-5,0,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,40), col = "red", col.axis = "dark red", las=1, lwd = 1,outer=FALSE, pos=Sys.time()-0600)
#par(new = T)
#plot(totalTemperatureY$date,totalTemperatureY$sensorValue,col="red", lwd=1, axes=F, xlab=NA, ylab=NA, type="l", lty=2)
par(new = T)
#lines(totalRelativeHumidity$date,totalRelativeHumidity$sensorValue,col="green", lwd=3, axis=F, xlab=NA, ylab=NA)
#plot(totalRelativeHumidity$date,totalRelativeHumidity$sensorValue,col="green", lwd=3, axis=F, xlab=NA, ylab=NA, type="l")
minT=min(totalRelativeHumidity$sensorValue)
maxT=max(totalRelativeHumidity$sensorValue)
minTY=min(totalRelativeHumidityY$sensorValue)
maxTY=max(totalRelativeHumidityY$sensorValue)
minTAll=minT
if (minTY<minTAll) minTAll <- minTY
maxTAll=maxT
if (maxTY>maxTAll) maxTAll <- maxTY
print(minTAll)
print(maxTAll)

plot(totalRelativeHumidity$date,totalRelativeHumidity$sensorValue,col="green", lwd=3, axes=F, xlab=NA, ylab=NA, type="l",ylim=c(minTAll,maxTAll))
lines(totalRelativeHumidityY$date,totalRelativeHumidityY$sensorValue,col="green", lwd=1, lty=2, axis=F, xlab=NA, ylab=NA)
#axis(4, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),col = "green", col.axis = "dark green", las=1, lwd = 1,outer=FALSE, pos=Sys.time()-(4*3600)) #+2400)
axis(4, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),col = "green", col.axis = "dark green", las=1, lwd = 1,outer=FALSE, pos=scalePosX+scalePosXTick*4.20)
print(Sys.time())
print(Sys.time()-(4*3600))
#axis(side = 4)
#mtext(side = 4, line = 3, 'relatieve luchtvochtigheid')
mtext(side = 4, line=4, 'temperatuur en relatieve luchtvochtigheid')
mtext(side = 1, line=2, paste('Periode:',periodetext1,'-',periodetext2))
#legend(total$date[1], -6, legend=c("wind", "regen", "temperatuur", "rel.luchtvochtigheid/10"),
legend("bottomleft", legend=c("wind", "regen"),
       col=c("black", "blue"), lwd=3, lty=1, cex=0.9)
legend("bottomright", legend=c("temperature","temperature-24h","rel.Humidity","rel.Humidity-24h"),
       col=c("red","red","green","green"), lwd=c(3,1,3,1), lty=c(1,2,1,2), cex=0.9)
#axis(side=3,outer=TRUE);
#dev.print(png,filename)
#dev.off()
