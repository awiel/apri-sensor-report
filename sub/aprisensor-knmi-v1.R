##


#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggthemes")
#install.packages("readr")
#require(grid) # for unit()
#require(cowplot)
library(cowplot)
library(ggthemes)
library(ggplot2)
library(grid) # for unit()
library(methods)
#library(reshape2)
library(scales)
library(dplyr)
#library("rjson")
#library(RJSONIO)
#library("RCurl")
library(jsonlite)
library(httr)
library(RColorBrewer)
#library(tidyverse)
library(readr)
library(cli)

writeDataFiles<-function(fileName) {
  save(count, age, circumference, file = "mydata.rda") 
}

getStationSelectRecordsKnmi<-function(dfIn=NULL,station=station,dateFrom=NULL,dateTo=NULL,periodSpan=NULL) {  

  if (is.null(dateFrom)) {
    #useCache <- TRUE
    if (is.null(periodSpan)) {
      dateFrom <-
        format(Sys.time() - (24 * 60 * 60 - 60) - (as.numeric(format(
          Sys.time(), '%z'
        )) / 100) * 60 * 60
        ,
        "%Y-%m-%dT%H:%M:%S")
    } else {
      if (is.numeric(periodSpan)) {
        dateFrom <-
          format(
            Sys.time() - (periodSpan * 60 * 60 - 60) - (as.numeric(format(
              Sys.time(), '%z'
            )) / 100) * 60 * 60
            ,
            "%Y-%m-%dT%H:%M:%S"
          )
      } else {
        dateFrom <-
          format(Sys.time() - (24 * 60 * 60 - 60) - (as.numeric(format(
            Sys.time(), '%z'
          )) / 100) * 60 * 60
          ,
          "%Y-%m-%dT%H:%M:%S")
      }
    }
    dateTo <-
      format(Sys.time() - (as.numeric(format(
        Sys.time(), '%z'
      )) / 100) * 60 * 60
      , "%Y-%m-%dT%H:%M:%S")
    print("Standaard periode:")
    print(dateFrom)
    print(dateTo)
  }
  
  
  if (is.null(dateFrom)) {
    paramDate<-''
    dateFrom<-format(Sys.time()-(24*60*60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
      ,"%Y-%m-%dT%H:%M:%S")
    dateTo<-format(Sys.time() - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
      ,"%Y-%m-%dT%H:%M:%S")
    tmpDateFrom<-as.POSIXct(dateFrom, format="%Y-%m-%dT%H:%M")
    tmpDateTo<-as.POSIXct(dateTo, format="%Y-%m-%dT%H:%M")
    dateFrom<-format(tmpDateFrom - (as.numeric(format(tmpDateFrom,'%z'))/100)*60*60
      ,"%Y-%m-%dT%H:%M:%S")
    dateTo<-format(tmpDateTo - (as.numeric(format(tmpDateTo,'%z'))/100)*60*60
      ,"%Y-%m-%dT%H:%M:%S")
    paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='') 
  } else {
    paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='') 
  }
  
  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"
  url <- paste("https://aprisensor-api-v1.openiod.org/v1/knmi/"
               , station,"?"
#               ,"?observationTypes=", observationTypes
               , paramDate
               ,sep='')
  print(url)
  
  dfResult <- fromJSON(url)$features$properties
  dfResult<-dfResult[order(dfResult$dateObserved),]  

#  if("pressure" %in% colnames(dfResult))
#  {
#    dfResult$sensorId<-as.factor(dfResult$station)
#    dfResult$sensorType<-"pressure"
#    dfResult$sensorType<-as.factor(dfResult$sensorType)
#    dfResult$sensorValue<-dfResult$pressure
#    dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M") #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
#    dfResult$tmpDateObserved<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M") 
#    dfResult$dateObserved<-format(dfResult$tmpDateObserved + (as.numeric(format(dfResult$tmpDateObserved,'%z'))/100)*60*60
#      ,"%Y-%m-%dT%H:%M:%S")
#  }
 
  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}

