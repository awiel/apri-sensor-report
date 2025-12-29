#!/usr/bin/env Rscript
options(width = 100)

# start in terminal: Rscript apri-sensor-generic-plot.R AFF4-pm25-csv-locatie-2

#install.packages(c('purrr','tidyselect','tibble','utf8','fansi','cli','pillar','magrittr','lifecycle','glue','generics','R6','rlang','dplyr'))
#install.packages(c('withr','colorspace','scales','ggplot2'))
#install.packages('broom','carData','broom','tidyr')
#install.packages('zoo')
#install.packages('ggpubr')
#install.packages('lubridate')

library('dplyr')
library('zoo')
library('lubridate')
#install.packages("optparse")
library(optparse)

args = commandArgs(trailingOnly=TRUE)

option_list <- list(
  make_option(c("-r", "--report"), type="character"),
  make_option(c("-t", "--timezone"), type="character", default="CET"),
  make_option(c("-A", "--colorA"), type="character"), # brewer color scheme group A eg Dark2, Paired
  make_option(c("-B", "--colorB"), type="character"), # brewer color scheme group B eg Dark2, Paired
  make_option(c("-Z", "--colorZ"), type="character"), # brewer color scheme group Z eg Dark2, Paired, Z=old/classic style
  make_option(c("-C", "--colorC"), type="character"), # brewer color scheme group C eg Dark2, Paired, C=other (grey?)
  make_option(c("-n", "--n"), type="integer", default=1)
)
opt <- parse_args(OptionParser(option_list=option_list), positional_arguments = TRUE)

print('======== arguments')
#print(opt$options)  # opt$options$timezone, opt$options$report
#print(opt$options$report[1])
if (!is.null(opt$args[1]) && !is.na(opt$args[1])) {
  print('Script arg 1 without argument name:')
  reportId<-opt$args[1]
} else {
#  print('Script argument report:')
  reportId<-opt$options$report
}
print(paste0('report:',reportId))
print(paste0('tijdzone:',opt$options$timezone))
print(paste0('Brewer color scheme group A:',opt$options$colorA)) # always use full name
print(paste0('Brewer color scheme group B:',opt$options$colorB))
print(paste0('Brewer color scheme group C (other):',opt$options$colorC)) 
print(paste0('Brewer color scheme group Z (classic):',opt$options$colorZ))
#print(opt$args) # opt$args is string with all unnamed options seperated by space
#print(opt$args[1])
print('========')
# test if there is at least one argument: if not, return an error
#defaultReport<-'AFF4-pm25-csv-locatie-2'
#if (length(args)==0 && is.null(defaultReport)) {
#  stop(" Commandline parameter voor reportID is missing, process stopped.", call.=FALSE)
#}
#if (length(args)==0) {
#  print(" Commandline parameter voor reportID is missing, default taken")
#  reportId<-defaultReport
#} else reportId<-args[1]

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


###
#library(grid)
##install.packages("magick")
##install.packages("RJSONIO")
##install.packages("ggpubr")
library(ggpubr)
#library(RJSONIO)

#install.packages("tidyverse") # voor o.a. ggplot2
#install.packages("ggplot2")
library(ggplot2)
library(magick)
library(RColorBrewer)
library(jsonlite)

source(paste0(subPath,"apri-sensor-fiware.R"))
source(paste0(subPath,"apri-sensor-aprisensor.R"))
source(paste0(subPath,"apri-luchtmeetnet.R"))
source(paste0(subPath,"aprisensor-knmi-v1.R"))
source(paste0(subPath,"aprisensor-cams.R"))
source(paste0(subPath,"apri-sensor-plot.R"))

sensorTypes<-json_data<-fromJSON(paste0(configPath,"apri-sensor-sensorTypes.json"))
sensorIds<-json_data<-fromJSON(paste0(configPath,"apri-sensor-sensorIds.json"))
logo<-image_read(paste0(imagePath,"logo-scapeler.png"))

print(paste("Start report for reportId",reportId))
reportFileName<-paste0(reportPath,reportId,".json")
print(reportFileName)
json_data<-fromJSON(reportFileName)
reportConfig<-json_data
print('report config file ok')

print("as dataframe sensorIds")
dfSensorIds<-as.data.frame(sensorIds)

#str(reportConfig)
reportTitle <- reportConfig$title
reportFileLabel <- reportConfig$fileLabel
reportSubTitle<-reportConfig$subTitle #'fijnstof PM2.5'
reportYLim <- reportConfig$yLim
reportHeight <- reportConfig$height
reportWidth <- reportConfig$width
reportTreshold <- reportConfig$treshold
reportTresholdLabel <- reportConfig$tresholdLabel
reportLocal <- reportConfig$local
reportStats <- reportConfig$stats

periodType<-reportConfig$periodType
periodSpan<-reportConfig$periodSpan
if (!is.null(reportConfig$incident) && reportConfig$incident=='T') incident<-T else incident<-F;

# "colors": [
#   {"id": "1234","type": "pm25","color": "blue"},
#   {"id": "1235","type": "pm25","color": "green"},
#   {"id": "1236","type": "pm25","group": "A"},
#   {"id": "1237","type": "pm25","group": "B"}
# ]

reportColors <- reportConfig$colors
#print(str(reportColors))
#if (nrow(reportColors) == 0) {
#  color_config <- tibble::tribble(
#    ~id,   ~type,   ~color,  ~group,
#    "1234","pm25",  "blue",  NA,
#    "1236","pm25",  NA,      "A"
#  )
#}


#dateFrom<-'2020-11-15T11:00:00'
#dateTo<-'2020-11-15T13:00:00'
#dateFrom<-'2020-11-16T16:00:00'
#dateTo<-'2020-11-17T1600:00'

meanMinutes<-reportConfig$mean$nr
period<-''
keeps <- c("sensorId","dateObserved","sensorValue")

sensorIds<-reportConfig$sensorIds
pm25Treshold<-TRUE
dfTmp<-NULL
print("loop sensorIds")
dfTmpMlr1<-NULL
for (i in 1:nrow(sensorIds)) {
  #print(sensorIds$active[i])
  reportSensorTypes<-sensorIds$sensorTypes[[i]]
  if (sensorIds$active[i]!="FALSE") {
    observableProperties<-NULL
    #print(reportSensorTypes)
    #    print(sensorIds[i])
    for (j in 1:nrow(reportSensorTypes)) {
      #  if (reportSensorTypes$active[j]=="TRUE") {
      if (is.null(observableProperties)) {
        observableProperties<-reportSensorTypes$sensorType[j]
      } else {
        observableProperties<-paste(observableProperties,reportSensorTypes$sensorType[j],sep=',')
      }
      #  }
    }
    if (!is.null(reportConfig$mean$text) && reportConfig$mean$text=='dag') {
      aggregateInd<-'D'
    } else {
      if (meanMinutes==0) {
        aggregateInd<-NULL
      } else {
        if (meanMinutes==10) {
          aggregateInd<-'A' # 10 minutes
        } else {
          aggregateInd<-'M' #minute
        }
      }
    }
    if (periodType == "actual") {
      print('actual')
      if (!is.null(sensorIds$serviceDB[i]) && !is.na(sensorIds$serviceDB[i])) {
        dbGroup<-sensorIds$dbGroup[i]
        observationTypes<-observableProperties

        if (sensorIds$sensorType[i]=='cams') {
          
          if (!is.null(sensorIds$sensorIdAlias[i]) && !is.na(sensorIds$sensorIdAlias[i])){
            sensorIdAlias<- sensorIds$sensorIdAlias[i]
          } else sensorIdAlias<-NULL
          
          dfTmpOne<-getCamsData(dfIn=NULL,lat=52,lon=4,dateFrom=NULL,dateTo=NULL,periodSpan=periodSpan
                                                ,observationTypes=observationTypes
          )
          dfTmpOne$date <- as.POSIXct(dfTmpOne$dateObserved, format = "%Y-%m-%dT%H:%M:%S")
          keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
          dfTmpOne <- dfTmpOne[keeps]
          
        } else {
          if (sensorIds$sensorType[i]=='knmi') {
          
          if (!is.null(sensorIds$sensorIdAlias[i]) && !is.na(sensorIds$sensorIdAlias[i])){
            sensorIdAlias<- sensorIds$sensorIdAlias[i]
          } else sensorIdAlias<-NULL
          
          dfTmpOne<-getStationSelectRecordsKnmi(dfIn=NULL
                                                ,station=sensorIds$sensorId[i]
                                                ,dateFrom=NULL
                                                ,dateTo=NULL
                                                ,periodSpan=periodSpan
                                                # ,sensorIdAlias=sensorIdAlias
                                                #  ,observationTypes=observationTypes
          )
          dfTmpOne$sensorId<-dfTmpOne$station
          dfTmpOne$sensorValue<-dfTmpOne$solar
          dfTmpOne$sensorType<-'irradiance'
          dfTmpOne$date <- as.POSIXct(dfTmpOne$dateObserved, format = "%Y-%m-%dT%H:%M:%S")
          keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
          dfTmpOne <- dfTmpOne[keeps]
          
        } else {
          if (sensorIds$sensorType[i]=='lml' || sensorIds$sensorType[i]=='lml_m' || sensorIds$sensorType[i]=='lml_lki') {
            
            if (!is.null(sensorIds$sensorIdAlias[i]) && !is.na(sensorIds$sensorIdAlias[i])){
              sensorIdAlias<- sensorIds$sensorIdAlias[i]
            } else sensorIdAlias<-NULL
            
            dfTmpOne<-getLuchtmeetnetData(dfIn=NULL
                                          ,sensorId=sensorIds$sensorId[i]
                                          ,sensorIdAlias=sensorIdAlias
                                          ,observationTypes=observationTypes
                                          ,periodSpan=periodSpan
            )
          } else {
            minuteMod<- 1 # for minute aggregation only (1 is default)
            if (!is.null(sensorIds$aggregation[i]) && !is.na(sensorIds$aggregation[i])){
              aggregation<-sensorIds$aggregation[i]
              if (!is.null(sensorIds$minuteMod[i]) && !is.na(sensorIds$minuteMod[i])){  # modulus for minute aggregation
                minuteMod <-sensorIds$minuteMod[i]
              }  
              #      #aggregation<-"minute"
            } else {
              aggregation<-"minute"
            }
            
            if (!is.null(sensorIds$sensorIdAlias[i]) && !is.na(sensorIds$sensorIdAlias[i])){
              sensorIdAlias<- sensorIds$sensorIdAlias[i]
            } else sensorIdAlias<-NULL
            
            dfTmpOne<-getApriSensorData(dfIn=NULL
                                        ,aggregation=aggregation
                                        ,dbGroup=dbGroup
                                        ,source=sensorIds$source[i]
                                        ,sensorId=sensorIds$sensorId[i]
                                        ,sensorIdAlias=sensorIdAlias
                                        ,sensorType=sensorIds$sensorType[i]
                                        ,observationTypes=observationTypes
                                        ,cachePath=cachePath
                                        ,periodSpan=periodSpan
                                        ,minuteMod=minuteMod
            )
          }
        }
        } # else end on cams
        #print(head(dfTmpOne))
      } #else {
      #   #t<-strsplit(observableProperties, ":")#[[1,1]]
      #   dfTmpOne<-getFiwareData(dfIn=NULL
      #                           ,fiwareService=sensorIds$fiwareService[i],fiwareServicePath=sensorIds$fiwareServicePath[i]
      #                           ,key=sensorIds$key[i],foi=sensorIds$sensorId[i],ops=observableProperties
      #                           ,cachePath=cachePath
      #                           ,aggregateInd=aggregateInd
      #                           ,source=sensorIds$source[i]
      #                           ,sensorId=sensorIds$sensorId[i]
      #                           ,datastream=strsplit(observableProperties, ":")[[1]][[1]]
      #                           ,sensorType=strsplit(observableProperties, ":")[[1]][[2]]
      #                           ,periodSpan=periodSpan
      #   )
      # }
    } else { # hist
      print('hist')
      
      if (!is.null(sensorIds$serviceDB[i]) && !is.na(sensorIds$serviceDB[i])) {
        if (sensorIds$sensorType[i]=='knmi') {
          print("hist getStationSelectRecordsKnmi")
          
          if (!is.null(sensorIds$sensorIdAlias[i]) && !is.na(sensorIds$sensorIdAlias[i])){
            sensorIdAlias<- sensorIds$sensorIdAlias[i]
          } else sensorIdAlias<-NULL
          
          dfTmpOne<-getStationSelectRecordsKnmi(dfIn=NULL
                                                ,station=sensorIds$sensorId[i]
                                                ,dateFrom=NULL
                                                ,dateTo=NULL
                                                ,periodSpan=periodSpan
                                                # ,sensorIdAlias=sensorIdAlias
                                                #  ,observationTypes=observationTypes
          )
          dfTmpOne$sensorId<-dfTmpOne$station
          dfTmpOne$sensorValue<-dfTmpOne$solar
          dfTmpOne$sensorType<-'irradiance'
          dfTmpOne$date <- as.POSIXct(dfTmpOne$dateObserved, format = "%Y-%m-%dT%H:%M:%S")
          keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
          dfTmpOne <- dfTmpOne[keeps]
          
        } else {
          print("hist getApriSensorData")
          
          dbGroup<-sensorIds$dbGroup[i]
          observationTypes<-observableProperties
          minuteMod<- 1 # for minute aggregation only (1 is default)
          if (!is.null(sensorIds$aggregation[i]) && !is.na(sensorIds$aggregation[i])){
            aggregation<-sensorIds$aggregation[i]
            if (!is.null(sensorIds$minuteMod[i]) && !is.na(sensorIds$minuteMod[i])){  # modulus for minute aggregation
              minuteMod <-sensorIds$minuteMod[i]
            }  
          } else {
            aggregation<-"minute"
          }
          
          if (!is.null(sensorIds$sensorIdAlias[i]) && !is.na(sensorIds$sensorIdAlias[i])){
            sensorIdAlias<- sensorIds$sensorIdAlias[i]
          } else sensorIdAlias<-NULL
          
          dfTmpOne<-getApriSensorData(dfIn=NULL
                                      ,aggregation=aggregation
                                      ,dbGroup=dbGroup
                                      ,sensorId=sensorIds$sensorId[i]
                                      ,sensorIdAlias=sensorIdAlias
                                      ,sensorType=sensorIds$sensorType[i]
                                      ,observationTypes=observationTypes
                                      #                  ,cachePath=cachePath
                                      ,dateFrom=reportConfig$dateFrom
                                      ,dateTo=reportConfig$dateTo
                                      ,minuteMod=minuteMod
          )
        }
        #     print(str(dfTmpOne))
        
      }
      #else {
      #   print("hist getFiwareData")
      #   dfTmpOne<-getFiwareData(dfIn=NULL
      #                           ,fiwareService=sensorIds$fiwareService[i],fiwareServicePath=sensorIds$fiwareServicePath[i]
      #                           ,key=sensorIds$key[i],foi=sensorIds$sensorId[i],ops=observableProperties
      #                           ,dateFrom=reportConfig$dateFrom
      #                           ,dateTo=reportConfig$dateTo
      #                           ,cachePath=cachePath
      #                           ,aggregateInd=aggregateInd
      #                           ,source=sensorIds$source[i]
      #                           ,sensorId=sensorIds$sensorId[i]
      #                           ,datastream=strsplit(observableProperties, ":")[[1]][[1]]
      #                           ,sensorType=strsplit(observableProperties, ":")[[1]][[2]]
      #                           ,csvFileName=sensorIds$csvFileName[i]
      #                           ,csvPath=sensorIds$csvPath[i]
      #                           ,csvType=sensorIds$csvType[i]
      #                           ,rdaFileName=sensorIds$rdaFileName[i]
      #                           ,rdaPath=sensorIds$rdaPath[i]
      #   )
      # 
      # }
    }
    
    
    #    calib<-FALSE
    if (is.null(sensorIds$pmCoarseBase[i])==FALSE && is.na(sensorIds$pmCoarseBase[i])==FALSE) {
      if (sensorIds$pmCoarseBase[i]=="TRUE") {
        dfTmpOne$date<-strftime(dfTmpOne$date, format = "%Y%m%d%H%M" )
        dfTmpOne$coarseBaseValue<-dfTmpOne$sensorValue
        keeps <- c("date","coarseBaseValue")
        dfTmpCoarseBase<-dfTmpOne[keeps]
        dfTmpOne<-NULL
      }
    }
    if (is.null(sensorIds$pmCoarse[i])==FALSE && is.na(sensorIds$pmCoarse[i])==FALSE) {
      if (sensorIds$pmCoarse[i]=="TRUE") {
        dfTmpOne$date<-strftime(dfTmpOne$date, format = "%Y%m%d%H%M" )
        keeps <- c("sensorId","sensorType","date","sensorValue","dateObserved")
        dfTmpCoarse<-dfTmpOne[keeps]
        dfMerged<-merge(dfTmpCoarse,dfTmpCoarseBase, by= 'date', sort = TRUE)
        dfMerged$sensorValue<-dfMerged$sensorValue-dfMerged$coarseBaseValue
        dfTmpOne<-dfMerged[keeps]
      }
    }
    
    if (is.null(sensorIds$calibrateFaseOne[i])==FALSE && is.na(sensorIds$calibrateFaseOne[i])==FALSE) {
      if (sensorIds$calibrateFaseOne[i]=="TRUE") {
        # print(str(dfTmpOne))
        
        dfTmpCal1<-dfTmpOne %>% left_join(dfCalibrations, by = c("sensorType" = "sensorType","sensorId"="sensorId"))
        dfTmpCal1$sensorId<-as.factor(paste0(dfTmpCal1$sensorId,'-c1'))
        dfTmpCal1$sensorType<-as.factor(dfTmpCal1$sensorType)
        dfTmpCal1$sensorValue<-(dfTmpCal1$sensorValue+dfTmpCal1$offset)*dfTmpCal1$factor*dfTmpCal1$factor2
        #        str(dfTmpCal1)
        keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
        dfTmpCal1 <- dfTmpCal1[keeps]
        #        str(dfTmpCal1)
        if (sensorIds$showRawData[i]=="TRUE") {
          dfTmpStack<-rbind(dfTmpStack,dfTmpCal1)
        } else {
          dfTmpStack<-dfTmpCal1
        }
        #        calib<-TRUE
      }
      # data for the T and rHum parameters
      if (sensorIds$calibrateFaseOne[i]=="MLR") {
        dfTmpMlr1<-NULL
        dfRHum<- subset(dfTmpOne, dfTmpOne$sensorType=='rHum')
        dfRHum$rHum<-dfRHum$sensorValue
        dfTemperature<- subset(dfTmpOne, dfTmpOne$sensorType=='temperature')
        dfTemperature$temperature<-dfTemperature$sensorValue
        dfTmpMlr1<-dfTemperature %>% left_join(dfRHum, by = c("date" = "date"))
        keepsMlr1 <- c("date", "rHum","temperature")
        dfTmpMlr1 <- dfTmpMlr1[keepsMlr1]
        dfTmpOne<-NULL
      }
    }
    if (is.null(sensorIds$calibrateFaseTwo[i])==FALSE && is.na(sensorIds$calibrateFaseTwo[i])==FALSE) {
      if (nrow(dfTmpOne)!=0) {
        if (sensorIds$calibrateFaseTwo[i]=="MLR") {
          if(is.null(dfTmpMlr1)==FALSE) {
            #   print(str(dfTmpOne))
            
            if (sensorIds$mlrType[i]=="PN") {
              if (sensorIds$sensorType[i]=="pmsa003") {
                dfTmpData<-NULL
                dfRaw0_3<- subset(dfTmpOne, dfTmpOne$sensorType=='raw0_3')
                dfRaw0_3$raw0_3<-dfRaw0_3$sensorValue
                dfRaw0_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw0_5')
                dfRaw0_5$raw0_5<-dfRaw0_5$sensorValue
                dfRaw1_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw1_0')
                dfRaw1_0$raw1_0<-dfRaw1_0$sensorValue
                dfRaw2_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw2_5')
                dfRaw2_5$raw2_5<-dfRaw2_5$sensorValue
                dfRaw5_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw5_0')
                dfRaw5_0$raw5_0<-dfRaw5_0$sensorValue
                dfRaw10_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw10_0')
                dfRaw10_0$raw10_0<-dfRaw10_0$sensorValue
                #              keepsMlr2 <- c("date", "raw0_3","raw0_5", "raw1_0", "raw2_5", "raw5_0", "raw10_0")
                dfRaw0_3 <- dfRaw0_3[c("date", "raw0_3","dateObserved","sensorId","sensorType")]
                dfRaw0_5 <- dfRaw0_5[c("date", "raw0_5")]
                dfRaw1_0 <- dfRaw1_0[c("date", "raw1_0")]
                dfRaw2_5 <- dfRaw2_5[c("date", "raw2_5")]
                dfRaw5_0 <- dfRaw5_0[c("date", "raw5_0")]
                dfRaw10_0 <- dfRaw10_0[c("date", "raw10_0")]
                
                dfTmpData<-dfRaw0_3 %>% left_join(dfRaw0_5, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw1_0, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw2_5, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw5_0, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw10_0, by = c("date" = "date"))
                #              dfTmpData$sensorId<-dfTmpOne$sensorId[1]
                #              dfTmpData$sensorType<-dfTmpOne$sensorType[1]
                #              dfTmpData$dateObserved<-dfTmpOne$dateObserved
                dfTmpOne<- dfTmpData
                #              dfTmpOne<-dfTmpOne %>% left_join(dfTmpData, by = c("date" = "date"))
              }
              if (sensorIds$sensorType[i]=="sps30") {
                print('### merge raw data sps30')
                dfTmpData<-NULL
                dfRaw0_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw0_5')
                dfRaw0_5$raw0_5<-dfRaw0_5$sensorValue
                dfRaw1_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw1_0')
                dfRaw1_0$raw1_0<-dfRaw1_0$sensorValue
                dfRaw2_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw2_5')
                dfRaw2_5$raw2_5<-dfRaw2_5$sensorValue
                dfRaw4_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw4_0')
                dfRaw4_0$raw4_0<-dfRaw4_0$sensorValue
                dfRaw10_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw10_0')
                dfRaw10_0$raw10_0<-dfRaw10_0$sensorValue
                #              keepsMlr2 <- c("date", "raw0_3","raw0_5", "raw1_0", "raw2_5", "raw5_0", "raw10_0")
                dfRaw0_5 <- dfRaw0_5[c("date", "raw0_5","dateObserved","sensorId","sensorType")]
                dfRaw1_0 <- dfRaw1_0[c("date", "raw1_0")]
                dfRaw2_5 <- dfRaw2_5[c("date", "raw2_5")]
                dfRaw4_0 <- dfRaw4_0[c("date", "raw4_0")]
                dfRaw10_0 <- dfRaw10_0[c("date", "raw10_0")]
                
                dfTmpData<-dfRaw0_5 %>% left_join(dfRaw1_0, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw2_5, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw4_0, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw10_0, by = c("date" = "date"))
                #              dfTmpData$sensorId<-dfTmpOne$sensorId[1]
                #              dfTmpData$sensorType<-dfTmpOne$sensorType[1]
                #              dfTmpData$dateObserved<-dfTmpOne$dateObserved
                dfTmpOne<- dfTmpData
                #              dfTmpOne<-dfTmpOne %>% left_join(dfTmpData, by = c("date" = "date"))
                #print(str(dfTmpOne))
              }
              if (sensorIds$sensorType[i]=="nextpm") {
                print('### merge raw data nextpm')
                dfTmpData<-NULL

                
                
                
                
                
                
                dfpn02pn05<- subset(dfTmpOne, dfTmpOne$sensorType=='pn02pn05')
                dfpn02pn05$pn02pn05<-dfpn02pn05$sensorValue
                dfpn05pn1<- subset(dfTmpOne, dfTmpOne$sensorType=='pn05pn1')
                dfpn05pn1$pn05pn1<-dfpn05pn1$sensorValue
                dfpn1pn25<- subset(dfTmpOne, dfTmpOne$sensorType=='pn1pn25')
                dfpn1pn25$pn1pn25<-dfpn1pn25$sensorValue
                dfpn25pn5<- subset(dfTmpOne, dfTmpOne$sensorType=='pn25pn5')
                dfpn25pn5$pn25pn5<-dfpn25pn5$sensorValue
                dfpn5pn10<- subset(dfTmpOne, dfTmpOne$sensorType=='pn5pn10')
                dfpn5pn10$pn5pn10<-dfpn5pn10$sensorValue
                dfpn02pn05 <- dfpn02pn05[c("date", "pn02pn05","dateObserved","sensorId","sensorType")]
                dfpn05pn1 <- dfpn05pn1[c("date", "pn05pn1")]
                dfpn1pn25 <- dfpn1pn25[c("date", "pn1pn25")]
                dfpn25pn5 <- dfpn25pn5[c("date", "pn25pn5")]
                dfpn5pn10 <- dfpn5pn10[c("date", "pn5pn10")]
                
                dfTmpData<-dfpn02pn05 %>% left_join(dfpn05pn1, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfpn1pn25, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfpn25pn5, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfpn5pn10, by = c("date" = "date"))
                
#                dfRaw1_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw1_0')
#                dfRaw1_0$raw1_0<-dfRaw1_0$sensorValue
#                dfRaw2_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw2_5')
#                dfRaw2_5$raw2_5<-dfRaw2_5$sensorValue
#                dfRaw10_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw10_0')
#                dfRaw10_0$raw10_0<-dfRaw10_0$sensorValue
#                dfRaw1_0 <- dfRaw1_0[c("date", "raw1_0","dateObserved","sensorId","sensorType")]
#                dfRaw2_5 <- dfRaw2_5[c("date", "raw2_5")]
#                dfRaw10_0 <- dfRaw10_0[c("date", "raw10_0")]
#                dfTmpData<-dfRaw1_0 %>% left_join(dfRaw2_5, by = c("date" = "date"))
#                dfTmpData<-dfTmpData %>% left_join(dfRaw10_0, by = c("date" = "date"))
                dfTmpOne<- dfTmpData
              }
              if (sensorIds$sensorType[i]=="ips7100") {
                dfTmpData<-NULL
                dfRaw0_1<- subset(dfTmpOne, dfTmpOne$sensorType=='raw0_1')
                dfRaw0_1$raw0_1<-dfRaw0_1$sensorValue
                dfRaw0_3<- subset(dfTmpOne, dfTmpOne$sensorType=='raw0_3')
                dfRaw0_3$raw0_3<-dfRaw0_3$sensorValue
                dfRaw0_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw0_5')
                dfRaw0_5$raw0_5<-dfRaw0_5$sensorValue
                dfRaw1_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw1_0')
                dfRaw1_0$raw1_0<-dfRaw1_0$sensorValue
                dfRaw2_5<- subset(dfTmpOne, dfTmpOne$sensorType=='raw2_5')
                dfRaw2_5$raw2_5<-dfRaw2_5$sensorValue
                dfRaw5_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw5_0')
                dfRaw5_0$raw5_0<-dfRaw5_0$sensorValue
                dfRaw10_0<- subset(dfTmpOne, dfTmpOne$sensorType=='raw10_0')
                dfRaw10_0$raw10_0<-dfRaw10_0$sensorValue
                #              keepsMlr2 <- c("date", "raw0_1", "raw0_3","raw0_5", "raw1_0", "raw2_5", "raw5_0", "raw10_0")
                dfRaw0_1 <- dfRaw0_1[c("date", "raw0_1","dateObserved","sensorId","sensorType")]
                dfRaw0_3 <- dfRaw0_3[c("date", "raw0_3")]
                dfRaw0_5 <- dfRaw0_5[c("date", "raw0_5")]
                dfRaw1_0 <- dfRaw1_0[c("date", "raw1_0")]
                dfRaw2_5 <- dfRaw2_5[c("date", "raw2_5")]
                dfRaw5_0 <- dfRaw5_0[c("date", "raw5_0")]
                dfRaw10_0 <- dfRaw10_0[c("date", "raw10_0")]
                
                dfTmpData<-dfRaw0_1 %>% left_join(dfRaw0_3, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw0_5, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw1_0, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw2_5, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw5_0, by = c("date" = "date"))
                dfTmpData<-dfTmpData %>% left_join(dfRaw10_0, by = c("date" = "date"))
                #              dfTmpData$sensorId<-dfTmpOne$sensorId[1]
                #              dfTmpData$sensorType<-dfTmpOne$sensorType[1]
                #              dfTmpData$dateObserved<-dfTmpOne$dateObserved
                dfTmpOne<- dfTmpData
                #              dfTmpOne<-dfTmpOne %>% left_join(dfTmpData, by = c("date" = "date"))
              }
            }
            dfTmpMlr2<-dfTmpOne %>% left_join(dfTmpMlr1, by = c("date" = "date"))
            #print(str(dfTmpMlr2))
            
            if (is.null(sensorIds$mlrType[i])==FALSE && is.na(sensorIds$mlrType[i])==FALSE) {
              
              if (sensorIds$mlrType[i]=="PM") {
                if (dfTmpMlr2$sensorType[1]=="pm25" || dfTmpMlr2$sensorType[1]=="pm25_pm25") {
                  print("Calculate MLR for PM2.5")
                  
                  dfTmpMlr2$sensorValueTmp<-dfTmpMlr2$sensorValue
                  
                  print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
                  # print(sensorIds$mlrFactorsPM[i,])
                  #            print(sensorIds$mlrType[i])
                  #            print(sensorIds[i,])
                  
                  if (sensorIds$sensorType[i]=="pmsa003") {
                    if (sensorIds$mlrVersion[i]=="visibilis1") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 14.8,
                                                       pm25= 0.3834,
                                                       temperature= -0.1905,
                                                       rHum= -0.1498
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 18.4,
                                                       pm25= 0.3853438,
                                                       temperature= 0.0053451,
                                                       rHum= -0.1781074
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2regel3") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 18.2,
                                                       pm25= 0.38591650,
                                                       temperature= -0.15387010,
                                                       rHum= -0.16499600
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2regel4") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 18.4,
                                                       pm25= 0.45055050,
                                                       temperature= -0.05564480,
                                                       rHum= -0.30222530
                      )
                    }
                  }
                  if (sensorIds$sensorType[i]=="ips7100") {
                    if (sensorIds$mlrVersion[i]=="visibilis2") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 20.1,
                                                       pm25= 0.33859960,
                                                       temperature= 0.05404910,
                                                       rHum= -0.18073580
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2B") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 23.8,
                                                       pm25= 0.2381699,
                                                       temperature= -0.0182353,
                                                       rHum= -0.3337437
                      )
                    }
                  }
                  if (sensorIds$sensorType[i]=="nextpm") {
                    if (sensorIds$mlrVersion[i]=="visibilis2") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 27.7,
                                                       pm25= 0.80096440,
                                                       temperature= -0.12838190,
                                                       rHum= -0.31309600
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2B") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 24.9,
                                                       pm25= 0.8806398,
                                                       temperature= -0.1857225,
                                                       rHum= -0.3337736
                      )
                    }
                  }
                  if (sensorIds$sensorType[i]=="sps30") {
                    if (sensorIds$mlrVersion[i]=="visibilis2") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 14.8,
                                                       pm25= 1.05446529,
                                                       temperature= 0.08208380,
                                                       rHum= -0.15686450
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2B") {
                      sensorMlrFactorsPM <- data.frame(type='PM',
                                                       b0= 21.2,
                                                       pm25= 0.9978563,
                                                       temperature= -0.1255003,
                                                       rHum= -0.3121716
                      )
                    }
                  }
                  
                  #sensorMlrFactorsPM <- data.frame(type='PM', bo = sensorIds$mlrFactorsPM$b0[i], pm25 = sensorIds$mlrFactorsPM$pm25[i], temperature = sensorIds$mlrFactorsPM$temperature[i], rHum = sensorIds$mlrFactorsPM$rHum[i])
                  
                  # Visibilis1 factors (default)
                  #                  mlrB0<-14.8
                  #                  mlrPm25<-0.3834
                  #                  mlrTemperature<- -0.1905
                  #                  mlrRHum<- -0.1498
                  #                  if (length(sensorIds$mlrFactorsPM$b0[i])!=0) {
                  #                    mlrB0<-sensorIds$mlrFactorsPM$b0[i]
                  #                    mlrPm25<-sensorIds$mlrFactorsPM$pm25[i]
                  #                    mlrTemperature<- sensorIds$mlrFactorsPM$temperature[i]
                  #                    mlrRHum<- sensorIds$mlrFactorsPM$rHum[i]
                  #                    print('wel mlr object')
                  #                  } else {
                  #                    print('geen mlr object')
                  #                  }
                  print(sensorMlrFactorsPM)
#                  print(head(dfTmpMlr2))
                  
                  dfTmpMlr2<-dfTmpMlr2 %>%
                    #mutate(sensorValue = 14.8 + (0.3834*sensorValue) + (-0.1498*rHum) + (-0.1905*temperature) ) %>%
                    mutate(sensorValue = sensorMlrFactorsPM$b0 + (sensorMlrFactorsPM$pm25*sensorValue) + (sensorMlrFactorsPM$rHum*rHum) + (sensorMlrFactorsPM$temperature*temperature) ) # %>%
            #        mutate(sensorValue = ifelse(sensorValue>sensorValueTmp,
            #                                    sensorValueTmp
            #                                    , sensorValue))
                  
#                  print(head(dfTmpMlr2))

                                    #            dfTmpMlr2$sensorValue<-ifelse (dfTmpMlr2$sensorValue>=4,
                  #            dfTmpMlr2$sensorValue <- 14.8 + (0.3834*dfTmpMlr2$sensorValue) + (-0.1498*dfTmpMlr2$rHum) + (-0.1905*dfTmpMlr2$temperature)
                  #              ,dfTmpMlr2$sensorValue)
                  dfTmpMlr2$sensorId<-paste0(dfTmpMlr2$sensorId,'_mlr')
                  keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
                  dfTmpMlr2 <- dfTmpMlr2[keeps]
                }
                if (dfTmpMlr2$sensorType[1]=='pm10' || dfTmpMlr2$sensorType[1]=='pm10_pm10') {
                  print('Calculate MLR for PM10')
                  
                  dfTmpMlr2$sensorValueTmp<-dfTmpMlr2$sensorValue
                  dfTmpMlr2<-dfTmpMlr2 %>%
                    mutate(sensorValue = 14.7 + (0.3151*sensorValue) + (-0.0948*rHum) + (0.2445*temperature) ) %>%
                    mutate(sensorValue = ifelse(sensorValue>sensorValueTmp,
                                                sensorValueTmp
                                                , sensorValue))
                  
                  #              #dfTmpMlr2$sensorValue1<-dfTmpMlr2$sensorValue
                  #              dfTmpMlr2<-dfTmpMlr2 %>% mutate(sensorValue = ifelse(sensorValue>=5,
                  #                14.7 + (0.3151*sensorValue) + (-0.0948*rHum) + (0.2445*temperature)
                  #                , sensorValue))
                  
                  #            dfTmpMlr2$sensorValue<-ifelse (dfTmpMlr2$sensorValue>=4,
                  #            dfTmpMlr2$sensorValue <- 14.8 + (0.3834*dfTmpMlr2$sensorValue) + (-0.1498*dfTmpMlr2$rHum) + (-0.1905*dfTmpMlr2$temperature)
                  #              ,dfTmpMlr2$sensorValue)
                  dfTmpMlr2$sensorId<-paste0(dfTmpMlr2$sensorId,'_mlr')
                  keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
                  dfTmpMlr2 <- dfTmpMlr2[keeps]
                }
                
                dfTmpOne<-dfTmpMlr2
              } # end of mlrType PM
              
              
              
              if (sensorIds$mlrType[i]=="PN") {
                #if (dfTmpMlr2$sensorType[1]=="pm25" || dfTmpMlr2$sensorType[1]=="pm25_pm25") {
               # if (sensorIds$sensorType[i]=="pmsa003") {
                  
                  #dfTmpMlr2$sensorValueTmp<-dfTmpMlr2$sensorValue
                  
                  if (sensorIds$sensorType[i]=="pmsa003") {
                    if (sensorIds$mlrVersion[i]=="visibilis2") {
                      # print("Calculate MLR for pmsa003 PN Visibilis2 A")
                      sensorMlrFactorsPN <- data.frame(type='PN',
                                                       b0= 15.9,
                                                       pn0_3= -0.0398651000,
                                                       pn0_5= 0.1542776000,
                                                       pn1_0= -0.0640661000,
                                                       pn2_5= 0.1228056000,
                                                       pn5_0= 4.1869030000,
                                                       pn10_0= -2.6055440000,
                                                       temperature= -0.0083817000,
                                                       rHum= -0.1808905000
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2regel3") {
                      # print("Calculate MLR for pmsa003 PN Visibilis2 B")
                      sensorMlrFactorsPN <- data.frame(type='PN',
                                                       b0= 15.10,
                                                       pn0_3= -0.0757818,
                                                       pn0_5= 0.2792005,
                                                       pn1_0= -0.1072021,
                                                       pn2_5= 0.3029881,
                                                       pn5_0= 1.3001051,
                                                       pn10_0= 0.5304403,
                                                       temperature= -0.0641135,
                                                       rHum= -0.1728792
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2regel4") {
                      # print("Calculate MLR for pmsa003 PN Visibilis2 B")
                      sensorMlrFactorsPN <- data.frame(type='PN',
                                                       b0= 11.0,
                                                       pn0_3= -0.1375197,
                                                       pn0_5= 0.474207,
                                                       pn1_0= -0.1055953,
                                                       pn2_5= 0.1715291,
                                                       pn5_0= 3.502532,
                                                       pn10_0= -2.904245,
                                                       temperature= 0.0312201,
                                                       rHum= -0.1375378
                      )
                    }
                  }

                  if (sensorIds$sensorType[i]=="sps30") {
                    if (sensorIds$mlrVersion[i]=="visibilis2") {
                      #print("Calculate MLR for sps30 PN Visibilis2")
                      sensorMlrFactorsPN <- data.frame(type='PN',
                                                       b0= 14.6,
                                                       pn0_5= -1.5049130000,
                                                       pn1_0= 8.6868410000,
                                                       pn2_5= -13.5178600000,
                                                       pn4_0= -11.2471800000,
                                                       pn10_0= 14.3540250000,
                                                       temperature= 0.1227285000,
                                                       rHum= -0.1590476000
                      )
                    }
                    if (sensorIds$mlrVersion[i]=="visibilis2B") {
                      #print("Calculate MLR for sps30 PN Visibilis2")
                      sensorMlrFactorsPN <- data.frame(type='PN',
                                                       b0= 20.6,
                                                       pn0_5= 1.004932,
                                                       pn1_0= -5.807172,
                                                       pn2_5= 8.5187483,
                                                       pn4_0= 4.0393887,
                                                       pn10_0= 31.656368,
                                                       temperature= -0.1172317,
                                                       rHum= -0.3089134
                      )
                    }
                  }
                if (sensorIds$sensorType[i]=="nextpm") {
                  if (sensorIds$mlrVersion[i]=="visibilis2") {
                    #print("Calculate MLR for nextpm PN Visibilis2")
                    sensorMlrFactorsPN <- data.frame(type='PN',
                                                     b0= 6.8,
                                                     pn1_0= 0.3354533000,
                                                     pn2_5= -1.1564390000,
                                                     pn10_0= 0.8224360000,
                                                     temperature= 0.4153694000,
                                                     rHum= -0.1185348000
                    )
                  }
                  if (sensorIds$mlrVersion[i]=="visibilis2B") {
                    #print("Calculate MLR for nextpm PN Visibilis2")
                    sensorMlrFactorsPN <- data.frame(type='PN',
                                                     b0= 21.3,
                                                     pn02pn05= 0.0020498,
                                                     pn05pn1= -0.0187594,
                                                     pn1pn25= -0.073801,
                                                     pn25pn5= 0.9140044,
                                                     pn5pn10= -0.5849119,
                                                     temperature= -0.3126088,
                                                     rHum= -0.2509179
                    )
                  }
                }

                if (sensorIds$sensorType[i]=="ips7100") {
                  if (sensorIds$mlrVersion[i]=="visibilis2") {
                    # print("Calculate MLR for pmsa003 PN Visibilis2")
                    sensorMlrFactorsPN <- data.frame(type='PN',
                                                     b0= 16.2,
                                                     pn0_1= 0.0002542000,
                                                     pn0_3= -0.0004870200,
                                                     pn0_5=0.0000109130,
                                                     pn1_0= 0.0028714000,
                                                     pn2_5= -0.0177061000,
                                                     pn5_0= 3.1346226000,
                                                     pn10_0= -46.7696700000,
                                                     temperature= 0.0251139000,
                                                     rHum= -0.1436989000
                    )
                  }
                  if (sensorIds$mlrVersion[i]=="visibilis2B") {
                    # print("Calculate MLR for pmsa003 PN Visibilis2")
                    sensorMlrFactorsPN <- data.frame(type='PN',
                                                     b0= 22.3,
                                                     pn0_1= 0.00023613,
                                                     pn0_3= -0.00034519,
                                                     pn0_5=-0.00012424,
                                                     pn1_0= 0.0010241,
                                                     pn2_5= 0.0028082,
                                                     pn5_0= -0.3400202,
                                                     pn10_0= 10.643739,
                                                     temperature= -0.1078622,
                                                     rHum= -0.3414054
                    )
                  }
                }
                
                                
                  print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
                  #                  sensorMlrFactorsPN <- data.frame(type='PN', bo = sensorIds$mlrFactorsPN$b0[i], 
                  #                                                   pn0_3 = sensorIds$mlrFactorsPN$pn0_3[i],
                  #                                                   pn0_5 = sensorIds$mlrFactorsPN$pn0_5[i],
                  #                                                   pn1_0 = sensorIds$mlrFactorsPN$pn1_0[i],
                  #                                                   pn2_5 = sensorIds$mlrFactorsPN$pn2_5[i],
                  #                                                   pn5_0 = sensorIds$mlrFactorsPN$pn5_0[i],
                  #                                                   pn10_0 = sensorIds$mlrFactorsPN$pn10_0[i],
                  #                                                   temperature = sensorIds$mlrFactorsPN$temperature[i], 
                  #                                                   rHum = sensorIds$mlrFactorsPN$rHum[i]);
                  
                  # Visibilis1 factors (default)
                  #                  mlrB0<-0
                  #                  mlrPn0_3<-0
                  #                  mlrPn0_5<-0
                  #                  mlrPn1_0<-0
                  #                  mlrPn2_5<-0
                  #                  mlrPn5_0<-0
                  #                  mlrPn10_0<-0
                  #                  mlrTemperature<- 0
                  #                  mlrRHum<- 0
                  #                  if (length(sensorIds$mlrFactorsPN$b0[i])!=0) {
                  #                    mlrB0<-sensorIds$mlrFactorsPN$b0[i]
                  #                    mlrPn0_3<-sensorIds$mlrFactorsPN$pn0_3[i]
                  #                    mlrPn0_5<-sensorIds$mlrFactorsPN$pn0_5[i]
                  #                    mlrPn1_0<-sensorIds$mlrFactorsPN$pn1_0[i]
                  #                    mlrPn2_5<-sensorIds$mlrFactorsPN$pn2_5[i]
                  #                    mlrPn5_0<-sensorIds$mlrFactorsPN$pn5_0[i]
                  #                    mlrPn10_0<-sensorIds$mlrFactorsPN$pn10_0[i]
                  #                    mlrTemperature<- sensorIds$mlrFactorsPN$temperature[i]
                  #                    mlrRHum<- sensorIds$mlrFactorsPN$rHum[i]
                  #                    print('wel mlr PN object')
                  #                  } else {
                  #                    print('geen mlr PN object')
                  #                  }
                  print(sensorMlrFactorsPN)
                  
                  #       print(str(dfTmpMlr2))
                  
                  if (sensorIds$sensorType[i]=="pmsa003") {
                    print("Calculate MLR for pmsa003 PN Visibilis2")
                    
                  dfTmpMlr2<-dfTmpMlr2 %>%
                    #mutate(sensorValue = 14.8 + (0.3834*sensorValue) + (-0.1498*rHum) + (-0.1905*temperature) ) %>%
                    mutate(sensorValue = sensorMlrFactorsPN$b0 + 
                             (sensorMlrFactorsPN$pn0_3*raw0_3) + 
                             (sensorMlrFactorsPN$pn0_5*raw0_5) + 
                             (sensorMlrFactorsPN$pn1_0*raw1_0) + 
                             (sensorMlrFactorsPN$pn2_5*raw2_5) + 
                             (sensorMlrFactorsPN$pn5_0*raw5_0) + 
                             (sensorMlrFactorsPN$pn10_0*raw10_0) + 
                             (sensorMlrFactorsPN$rHum*rHum) + 
                             (sensorMlrFactorsPN$temperature*temperature) ) # %>%
                  #mutate(sensorValue = ifelse(sensorValue>sensorValueTmp,
                  #                            sensorValueTmp
                  #                            , sensorValue))
                  
                  #            dfTmpMlr2$sensorValue<-ifelse (dfTmpMlr2$sensorValue>=4,
                  #            dfTmpMlr2$sensorValue <- 14.8 + (0.3834*dfTmpMlr2$sensorValue) + (-0.1498*dfTmpMlr2$rHum) + (-0.1905*dfTmpMlr2$temperature)
                  #              ,dfTmpMlr2$sensorValue)
                  }
                  
                  if (sensorIds$sensorType[i]=="sps30") {
                    print("Calculate MLR for sps30 PN Visibilis2")
                    
                    dfTmpMlr2<-dfTmpMlr2 %>%
                      #mutate(sensorValue = 14.8 + (0.3834*sensorValue) + (-0.1498*rHum) + (-0.1905*temperature) ) %>%
                      mutate(sensorValue = sensorMlrFactorsPN$b0 + 
                               (sensorMlrFactorsPN$pn0_5*raw0_5) + 
                               (sensorMlrFactorsPN$pn1_0*raw1_0) + 
                               (sensorMlrFactorsPN$pn2_5*raw2_5) + 
                               (sensorMlrFactorsPN$pn4_0*raw4_0) + 
                               (sensorMlrFactorsPN$pn10_0*raw10_0) + 
                               (sensorMlrFactorsPN$rHum*rHum) + 
                               (sensorMlrFactorsPN$temperature*temperature) ) # %>%
                    #mutate(sensorValue = ifelse(sensorValue>sensorValueTmp,
                    #                            sensorValueTmp
                    #                            , sensorValue))
                    
                    #            dfTmpMlr2$sensorValue<-ifelse (dfTmpMlr2$sensorValue>=4,
                    #            dfTmpMlr2$sensorValue <- 14.8 + (0.3834*dfTmpMlr2$sensorValue) + (-0.1498*dfTmpMlr2$rHum) + (-0.1905*dfTmpMlr2$temperature)
                    #              ,dfTmpMlr2$sensorValue)
                  }

                  if (sensorIds$sensorType[i]=="nextpm") {
                    print("Calculate MLR for nextpm PN Visibilis2")
                    
                    dfTmpMlr2<-dfTmpMlr2 %>%
                  #    mutate(sensorValue = sensorMlrFactorsPN$b0 + 
                  #             (sensorMlrFactorsPN$pn1_0*raw1_0) + 
                  #             (sensorMlrFactorsPN$pn2_5*raw2_5) + 
                  #             (sensorMlrFactorsPN$pn10_0*raw10_0) + 
                  #             (sensorMlrFactorsPN$rHum*rHum) + 
                  #             (sensorMlrFactorsPN$temperature*temperature) ) # %>%
                       mutate(sensorValue = sensorMlrFactorsPN$b0 + 
                             (sensorMlrFactorsPN$pn02pn05*pn02pn05) + 
                             (sensorMlrFactorsPN$pn05pn1*pn05pn1) + 
                             (sensorMlrFactorsPN$pn1pn25*pn1pn25) + 
                               (sensorMlrFactorsPN$pn25pn5*pn25pn5) + 
                               (sensorMlrFactorsPN$pn5pn10*pn5pn10) + 
                               (sensorMlrFactorsPN$rHum*rHum) + 
                             (sensorMlrFactorsPN$temperature*temperature) ) # %>%
                  }

                  if (sensorIds$sensorType[i]=="ips7100") {
                    print("Calculate MLR for pmsa003 PN Visibilis2")
                    
                    dfTmpMlr2<-dfTmpMlr2 %>%
                      #mutate(sensorValue = 14.8 + (0.3834*sensorValue) + (-0.1498*rHum) + (-0.1905*temperature) ) %>%
                      mutate(sensorValue = sensorMlrFactorsPN$b0 + 
                               (sensorMlrFactorsPN$pn0_1*raw0_1) + 
                               (sensorMlrFactorsPN$pn0_3*raw0_3) + 
                               (sensorMlrFactorsPN$pn0_5*raw0_5) + 
                               (sensorMlrFactorsPN$pn1_0*raw1_0) + 
                               (sensorMlrFactorsPN$pn2_5*raw2_5) + 
                               (sensorMlrFactorsPN$pn5_0*raw5_0) + 
                               (sensorMlrFactorsPN$pn10_0*raw10_0) + 
                               (sensorMlrFactorsPN$rHum*rHum) + 
                               (sensorMlrFactorsPN$temperature*temperature) ) # %>%
                    #mutate(sensorValue = ifelse(sensorValue>sensorValueTmp,
                    #                            sensorValueTmp
                    #                            , sensorValue))
                    
                    #            dfTmpMlr2$sensorValue<-ifelse (dfTmpMlr2$sensorValue>=4,
                    #            dfTmpMlr2$sensorValue <- 14.8 + (0.3834*dfTmpMlr2$sensorValue) + (-0.1498*dfTmpMlr2$rHum) + (-0.1905*dfTmpMlr2$temperature)
                    #              ,dfTmpMlr2$sensorValue)
                  }
                  
                                    
                  dfTmpMlr2$sensorId<-paste0(dfTmpMlr2$sensorId,'_mlr')
                  dfTmpMlr2$sensorType <- 'pm25_pm25'  # pm25 on for pn_mlr 
                  
                  print(str(dfTmpMlr2))
                  #keeps <- c("sensorId",
                  #           "sensorType",
                  #           "date", "raw0_3", "raw0_5", "raw1_0", "raw2_5", "raw5_0", "raw10_0"
                  #           ,"dateObserved"
                  #           )
                  keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
                  
                  dfTmpMlr2 <- dfTmpMlr2[keeps]
                
                
                #}
                
                dfTmpOne<-dfTmpMlr2
              } # end of mlrType PN
            }
          } 
        }
      }
    }
    
    #    if (calib==FALSE) {
    if (is.null(dfTmpOne)==FALSE) {
      dfTmpStack<-dfTmpOne
      # multiply factor e.g. ips7100 1L -> 0.1L = multiply by 0.1
      if (is.null(sensorIds$multiply[i])==FALSE && is.na(sensorIds$multiply[i])==FALSE) {
        dfTmpStack$sensorValue<-dfTmpStack$sensorValue*sensorIds$multiply[i]
      }
      #    if (is.null(pm25Treshold)==FALSE) {
      #      if (observableProperties=="pm25") {
      #        pm25Treshold<-NULL
      #        dfPm25Treshold <- subset(dfTmpOne, sensorType == 'pm25')
      #        dfPm25Treshold$sensorId<-'treshold'
      #        dfPm25Treshold$sensorValue<-25  # WHO 24 hours mean
      #        dfTmpStack<-rbind(dfTmpStack,dfPm25Treshold)
      #      }
      #    }
      dfTmp<-rbind(dfTmp,dfTmpStack)
    }
  }
}

###########################################################

#dfTmp$date <- as.POSIXct(dfTmp$dateObserved, format="%Y-%m-%dT%H:%M")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
#dfTmp$minute <- sapply(format(dfTmp$date, "%M"), as.numeric)
#dfTmp$hour <- sapply(format(dfTmp$date, "%H"), as.numeric)
#dfTmp$foi <- dfTmp$sensorId
#dfTmp$date <- dfTmp$date - ( dfTmp$minute %% meanMinutes)*60  # gemiddelde per x minutes


if(is.null(dfTmp)) {
  print('No records found')
  quit()
}

#if (length(dfTmp)<=2) {
#  print("No records found, process stopped")
#  quit()
#}

#if (!is.null(reportLocal) && !is.na(reportLocal)) {
#  # Japan
#  if (reportLocal=='xxJA') {
#    dfTmp$date <- as.POSIXct(dfTmp$dateObserved, format="%Y-%m-%dT%H:%M:%S")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
#    dfTmp$date <- with_tz(dfTmp$date, tz="Asia/Tokyo")
#  } 
#} else {
dfTmp$date <- as.POSIXct(dfTmp$dateObserved, format="%Y-%m-%dT%H:%M:%S")+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
#}


dfTmp$minute <- sapply(format(dfTmp$date, "%M"), as.numeric)
dfTmp$hour <- sapply(format(dfTmp$date, "%H"), as.numeric)
dfTmp$foi <- dfTmp$sensorId 

if(nrow(dfTmp==0)) {
  print('no (new) records retrieved')
}
# else {
#   if (meanMinutes==0) {
#     print('No mean calculation')
#     #  #dfTmp$date <- dfTmp$date - ( dfTmp$minute %% meanMinutes)*60  # gemiddelde per x minutes
#   } else {
#     print(paste('xx mean calculation',dfTmp$minute,'%%',meanMinutes,' rows:',nrow(dfTmp)))
#     dfTmp$date <- dfTmp$date - ( dfTmp$minute %% meanMinutes)*60  # gemiddelde per x minutes
#   }
# }

keeps <- c("date", "sensorValue","sensorType","sensorId")
total <- dfTmp[keeps]
#total$tmp2 = as.character(total$sensorId);
total$foiLocation=factor(substr(as.character(total$sensorId),regexpr('S.*$',as.character(total$sensorId)),50));
total$type = c(0, cumsum(diff(total$date) > 99999600))  # tijdsduur in seconden als minumum waarde voor onderbrekingen van grafieklijn
total$sensorType = factor(total$sensorType, levels=names(sensorTypes))
print(paste('y-limit=',reportYLim))
yZoom<-NULL
if (is.null(reportYLim)) {
  ylim <- NULL
} else {
  if(reportYLim=="ZERO"){
    ylim <- c(0, max(total$sensorValue))
  }
  if(reportYLim=="ZERO15"){
    ylim <- c(0, 15)
  }
  if(reportYLim=="ZERO60"){
    ylim <- c(0, 60)
  }
  if(reportYLim=="ZERO100"){
    ylim <- c(0, 100)
    #yZoom <- c(0, 100)
  }
  if(reportYLim=="ZERO150"){
    ylim <- c(0, 150)
    #yZoom <- c(0, 100)
  }
  if(reportYLim=="ZERO200"){
    ylim <- c(0, 200)
  }
  if(reportYLim=="ZERO300"){
    ylim <- c(0, 300)
  }
  if(reportYLim=="ZERO500"){
    ylim <- c(0, 500)
  }
  if(reportYLim=="ZERO800"){
    ylim <- c(0, 800)
  }
  if(reportYLim=="ZERO1500"){
    ylim <- c(0, 1500)
  }
  if(reportYLim=="MINMAX"){
    ylim <- c(min(total$sensorValue), max(total$sensorValue))
  }
}

period <- range(total$date);
print(period)
localTimeZone<-'CET'
if (!is.null(reportLocal) && !is.na(reportLocal)) {
  # Japan
  if (reportLocal=='JA') {
    localTimeZone<-'Japan'
  }
}

periodetext1 <- with_tz(period[1],localTimeZone)|>format("%Y-%m-%d %H:%Mu")   # alleen jaar-maand-dag uur # strftime(with_tz(period[1],'Asia/Tokyo'), format = "%Y-%m-%d %H:%M uur %z" ,tz='JST')
periodetext2 <- with_tz(period[2],localTimeZone)|>format("%Y-%m-%d %H:%Mu")  # strftime(period[2], format = "%Y-%m-%d %H:%M uur %z",tz='JST',usetz=TRUE)
print(">ggplot")
dateBreaks<-"1 hour"
dateLabels<-"%H"
aggregateTxt<-"gemiddeld per minuut"
#if (!is.null(reportConfig$mean$text) && reportConfig$mean$text=='dag') {
#  dateBreaks<-"2 days"
#  dateLabels<-"%d"
#  aggregateTxt<-"gemiddeld per dag"
#}
#if (!is.null(reportConfig$mean$text) && reportConfig$mean$text=='10 seconden') {
#  #dateBreaks<-"1 month"
#  #dateLabels<-"%m"
#  aggregateTxt<-"gemiddeld per 10 seconden"
#}
#if (!is.null(reportConfig$mean$nr) && reportConfig$mean$nr==0) {
#  aggregateTxt<-"gemiddeld per 20 seconden"
#}
if (!is.null(reportConfig$mean$nr) && reportConfig$mean$nr==10) {
  aggregateTxt<-"gemiddeld per 10 minuten"
}

if (!is.null(reportLocal)&&!is.na(reportLocal)) {
  if(reportLocal=='JA') {
    if (aggregateTxt=='gemiddeld per minuut') aggregateTxt<-'1分あたりの平均'
    if (aggregateTxt=='gemiddeld per uur') aggregateTxt<-'1時間あたりの平均'
    if (aggregateTxt=='gemiddeld per dag') aggregateTxt<-'1日あたりの平均'
    if (aggregateTxt=='gemiddeld per 10 seconden') aggregateTxt<-'10秒ごとの平均'
    if (aggregateTxt=='gemiddeld per 20 seconden') aggregateTxt<-'20秒ごとの平均'
    if (aggregateTxt=='gemiddeld per 10 minuten') aggregateTxt<-'平均10分ごと'
  }
}

print("start apriSensorPlotSingle")
gTotal<-apriSensorPlotSingle(total,dfSensorIds,sensorTypes,reportTitle,reportSubTitle
                             ,ylim,treshold=reportTreshold
                             ,tresholdLabel=reportTresholdLabel,dateBreaks=dateBreaks,dateLabels=dateLabels
                             ,aggregateTxt=aggregateTxt,yzoom=yZoom,
                             incident=incident
                             ,reportLocal=reportLocal
                             ,reportStats=reportStats
                             ,reportColors=reportColors)


print("make imagefile")
#print(head(gTotal))
#print(reportHeight)
#print(reportWidth)
#gTotal
if (!is.null(reportHeight)) {
  if (!is.null(reportWidth)) {
    apriSensorImage(gTotal,reportFileLabel,height=reportHeight,width=reportWidth)
  } else {
    apriSensorImage(gTotal,reportFileLabel,height=reportHeight)
  }
} else {
  if (!is.null(reportWidth)) {
    apriSensorImage(gTotal,reportFileLabel,width=reportWidth)
  } else {
    apriSensorImage(gTotal,reportFileLabel)
  }
}
#if (!is.null(reportHeight)&!is.null(reportWidth)) apriSensorImage(gTotal,reportFileLabel,height=reportHeight,width=reportWidth)
#if (!is.null(reportHeight)) apriSensorImage(gTotal,reportFileLabel,height=reportHeight)
#if (!is.null(reportWidth)) apriSensorImage(gTotal,reportFileLabel,width=reportWidth)
#if (is.null(reportHeight)) apriSensorImage(gTotal,reportFileLabel)
print(paste0("Report saved as ",reportFileLabel,'.png'))

if(is.null(reportConfig$correlPlots)==FALSE) {
  reportCorrelPlots<-reportConfig$correlPlots
  if (nrow(reportCorrelPlots)>0) {
    for (i in 1:nrow(reportCorrelPlots)) {
      #  str(reportCorrelPlots)
      #  print(reportCorrelPlots$active[i])
      plotDateTime<- Sys.time() #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
      captionText<-paste0('Datum: ',format(plotDateTime,"%d-%m-%Y %H:%M"))
      if (reportCorrelPlots$active[i]!="FALSE") {
        print("plot correlPlot")
        #total <- subset(total, total$sensorType == 'pm25')
        dfX<- subset(total, (total$sensorId == reportCorrelPlots$xSensorId[i] & total$sensorType==reportCorrelPlots$xSensorType[i]))
        #   print(paste(total$sensorId,reportCorrelPlots$xSensorId[i]))
        dfXMin<-min(dfX$sensorValue)
        dfXMax<-max(dfX$sensorValue)
        dfX$mDate<-strftime(dfX$date, format = "%Y%m%d%H%M" )
        dfY<- subset(total, (total$sensorId == reportCorrelPlots$ySensorId[i] & total$sensorType==reportCorrelPlots$ySensorType[i]))
        dfYMin<-min(dfY$sensorValue)
        dfYMax<-max(dfY$sensorValue)
        dfY$mDate<-strftime(dfY$date, format = "%Y%m%d%H%M" )
        dfXRes<-(dfXMax-dfXMin)/20
        dfYRes<-(dfYMax-dfYMin)/20
        print('Correlationplot: ')
        #    str(dfX)
        #    str(dfY)
        #    plot(dfX$sensorValue, dfY$sensorValue)
        dfMerged<-merge(dfX, dfY, by= 'mDate', sort = TRUE)
        #    str(dfMerged)
        # formula <- y ~ poly(x, 3, raw = TRUE)
        b <- ggplot(dfMerged, aes(x = sensorValue.x, y = sensorValue.y)) +
          stat_cor(label.x = dfXMin+dfXRes, label.y = dfYMax-dfYRes*1,size=1.0,
                   aes(label =  paste( ..r.label.., ..rr.label.., sep = "~~~~")),) +
          stat_regline_equation(label.x = dfXMin+dfXRes, label.y = dfYMax-dfYRes*2,width=1.0) +
          #stat_regline_equation(label.x = dfXMin+dfXRes, label.y = dfYMax-dfYRes*2,linewidth=1.0) +
          #   stat_cor(label.x = dfXMin+dfXRes, label.y = dfYMax-dfYRes*3,size=0.9,formula=formula) +
          #   stat_regline_equation(label.x = dfXMin+dfXRes, label.y = dfYMax-dfYRes*4,size=0.9,formula=formula) +
          theme_bw()+
          theme(text = element_text(size = rel(1.8))
                , element_line(colour = 'green', linewidth = 0.1)
                #, plot.title = element_text(face="bold",size = rel(3.2), hjust =0,margin=margin(0,0,0,0)) # 0.5)  #lineheight=rel(1),
                , plot.title = element_text(face="bold",size = rel(1.8), hjust =0,margin=margin(0,0,0,0)) # 0.5)  #lineheight=rel(1),
                , plot.subtitle=element_text(size = rel(2.2), hjust =0,margin=margin(3,0,8,0)) # 0.5) #,face="bold")
                #, plot.caption=element_text(size = rel(1.5),hjust=0,color = "black", face="italic")
                , plot.caption=element_text(size = rel(1.1),hjust=0,color = "black", face="italic")
                #        , plot.caption.position =  "plot"
                #, axis.text=element_text(size = rel(0.9))
                , axis.text=element_text(size = rel(0.9))
                , axis.text.y.right=element_text(size = rel(0.9))
                #, axis.line = element_line(colour = "black", linewidth = 0.1)
                , axis.line = element_line(colour = "black", linewidth = 0.01)
                , axis.ticks = element_line(colour = "black", linewidth = 0.01)
                #, legend.text=element_text(size = rel(1.9))
                , legend.text=element_text(size = rel(1.5))
                #, legend.title=element_text(size = rel(2.0)) #,face="bold")
                , legend.title=element_text(size = rel(1.7)) #,face="bold")
                , legend.position="top"
                , legend.justification="right"
                , legend.margin=margin(0,0,0,0)
                , legend.box.margin=margin(-10,-10,-10,-10) # t r b l
                , panel.border = element_rect(colour = "black", fill=NA, linewidth=0.1)
                , legend.key.height=unit(0.5,"line")
                , legend.key = element_rect(color = NA, fill = NA)
                , legend.key.width=unit(0.3,"cm")
          )  +
          labs(x=paste(reportCorrelPlots$xLabel[i],'\n\nperiode: ',periodetext1,' tm ',periodetext2,'\n',sep=''),
               y=reportCorrelPlots$yLabel[i],title=paste("ApriSensor ",reportCorrelPlots$fileLabel[i])
               # , subtitle=''
               , caption=captionText) +
          geom_point(color = "#00AFBB", size = 0.001) +
          geom_smooth(method = lm, se = FALSE, size=0.1,color='#00AFBB')
        #   print(ggplot_build(b))
        #   reg<-lm(sensorValue.y ~ sensorValue.x, data = dfMerged)
        #   print(reg)
        #   coeff=coefficients(reg)
        #   print(coeff)
        # b<-b + geom_abline(intercept = coeff[1], slope = coeff[2], color="red", linetype="dashed", size=1.5)
        #  b<-b + geom_abline(intercept = -0.02, slope = 0.00087, color="red", linetype="dashed", size=0.5)
        apriSensorImage(b,paste0(reportFileLabel,'-',reportCorrelPlots$fileLabel[i]),height=1.9,width=2.1)
        if (is.null(reportCorrelPlots$save[i])==FALSE && is.na(reportCorrelPlots$save[i])==FALSE ) {
          if (reportCorrelPlots$save[i]=='TRUE') {
            timeStamp<-paste0(substr(dfMerged$mDate[1],1,11),'0')
            fileNameTimeStamp<-paste0(reportFileLabel,'-',reportCorrelPlots$fileLabel[i],'_',timeStamp)
            print(fileNameTimeStamp)
            #apriSensorImage(b,fileNameTimeStamp,height=2.4,width=3.2,subFolder='correl')
            apriSensorImage(b,fileNameTimeStamp,height=2.0,width=2.1,subFolder='correl')
          }
        }
      }
    }
  }
} # end of correlation plot loop

