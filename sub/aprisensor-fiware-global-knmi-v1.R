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

fileLocation <- '/opt/SCAPE604/R/files/'

writeDataFiles<-function(fileName) {
  save(count, age, circumference, file = "mydata.rda") 
}

fiwareGetSensorSelectRecordsKnmi<-function(dfIn,fiwareService,fiwareServicePath,key,foi,ops,opPerRow='true',dateFrom=NULL,dateTo=NULL) {  
  # eg.  SCNM5CCF7F2F62F3,pm25
  # eg2. SCNM5CCF7F2F62F3:SCNM5CCF7F2F62F3_a,pm25:pm25_alias,pm10
  # https://aprisensor-in.openiod.org/apri-sensor-service/v1/getSelectionData/?fiwareService=aprisensor_in&fiwareServicePath=/pmsa003&key=sensorId&foiOps=SCNM5CCF7F2F62F3:SCNM5CCF7F2F62F3_a,pm25:pm25_alias
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
#    tmpDateFrom<-as.POSIXct(dateFrom, format="%Y-%m-%dT%H:%M") 
#    tmpDateTo<-as.POSIXct(dateTo, format="%Y-%m-%dT%H:%M") 
#    dateFrom<-format(tmpDateFrom - (as.numeric(format(tmpDateFrom,'%z'))/100)*60*60
#      ,"%Y-%m-%dT%H:%M:%S")
#    dateTo<-format(tmpDateTo - (as.numeric(format(tmpDateTo,'%z'))/100)*60*60
#      ,"%Y-%m-%dT%H:%M:%S")
    paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='') 
  }

  url <- paste("https://aprisensor-in.openiod.org/apri-sensor-service/v1/getSelectionData/"
                ,"?fiwareService=",fiwareService
                ,"&fiwareServicePath=",fiwareServicePath
                ,"&key=",key
                ,"&opPerRow=",opPerRow
                ,"&foiOps=",foi,",",ops
                , paramDate
                ,sep='')
                #,"&dateFrom=",dateFrom
                #,"&dateTo=",dateTo
  print("fiwareGetSensorSelectRecordsKnmi")
  print(url)
  #dfResult <- read.csv(url, header = TRUE, sep = ";", quote = "\"",stringsAsFactors = FALSE)
  #dfResult <- read.csv(url, header = TRUE, sep = ";", stringsAsFactors = FALSE,as.is = 1)
  if (ops=="pressure") {
    colTypes<-"cccd"
  } else {
    colTypes<-"cccnn"
  }
  dfResult <-readr::read_delim(
    url, col_types = colTypes, delim=';'
  #col_names = TRUE,
  #col_types = NULL,
  #locale = default_locale(),
  ,na = c("undefined","", "NA")
  #quoted_na = TRUE,
  ,quote = "\""
  #comment = "",
  #trim_ws = TRUE,
  ,skip = 0
  #n_max = Inf,
  #guess_max = min(1000, n_max),
#  progress = show_progress(),
  #skip_empty_rows = TRUE
)
  dfResult<-as.data.frame(dfResult)
  if("pressure" %in% colnames(dfResult))
  {
    dfResult$sensorId<-as.factor(dfResult$station)
    dfResult$sensorType<-"pressure"
    dfResult$sensorType<-as.factor(dfResult$sensorType)
    dfResult$sensorValue<-dfResult$pressure
    dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M") #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
    dfResult$tmpDateObserved<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M") 
    dfResult$dateObserved<-format(dfResult$tmpDateObserved + (as.numeric(format(dfResult$tmpDateObserved,'%z'))/100)*60*60
      ,"%Y-%m-%dT%H:%M:%S")
  }
 
  #if (ops=='pm1,pm25,pm10') {
  #  dfResultMax <- subset(dfResult, dfResult$sensorValue >= 200)
  #  if (as.numeric(nrow(dfResultMax)>=1)) {
  #    dfResult1 <- subset(dfResult, dfResult$sensorValue < 200)
  #    dfResultMax$sensorValue<-200
  #    dfResult<-rbind(dfResult1,dfResultMax)
  #  }
  #}
  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}

fiwareGetPeriodSlice<-function(fiwareStart,fiwareEnd,fiwareService,fiwareServicePath,observableProperty,sensorId) {
  fiwareUrl <- paste("https://orion.openiod.nl/v2/entities?limit=1000&options=count,keyValues"
                   ,"&attrs=",observableProperty,",","sensorId,dateObserved" 
                   ,"&q=","sensorId=='",sensorId,"'" 
                   ,";dateObserved=='",fiwareStart,"'..'",fiwareEnd,"'"
                   ,sep='')
  print(fiwareUrl)
#  print(paste('Fiware service: ',fiwareService,' / ',fiwareServicePath,sep=''))
  if (fiwareServicePath=='') {
    httpResponse <- GET(fiwareUrl, accept_json())
  } else {
    httpResponse <- GET(fiwareUrl, add_headers("Fiware-Service" = fiwareService,"Fiware-ServicePath" = fiwareServicePath), accept_json())
  }
  print(http_status(httpResponse)$reason)
  if (headers(httpResponse)$'fiware-total-count' == "0") {
    print("Geen records gevonden");
   # result<-" "
    #result <- data.frame("","",0,"","")
    #x <- c("id", "type", observableProperty,"sensorId","dateObserved")
    #colnames(result) <- x
    options(stringsAsFactors=FALSE)
    result<-data.frame(id=character(), type=character(), x=numeric(),sensorId=character(),dateObserverd=character(),stringsAsFactors=FALSE)
    result<-rbind(result,c("1","1",1,"1","1"))
    colnames(result) <- c("id", "type", observableProperty,"sensorId","dateObserved")
  } else { 
    result = fromJSON(content(httpResponse, "text",encoding ="UTF-8", null=NA))
  }
  str(result)
  return(result);
}

fiwareGetRecords<-function(dfIn,fiwareStart,fiwareEnd,fiwareService,fiwareServicePath,observableProperty,alias,sensorId,sensorIdSuffix) {
  print(fiwareStart)
  print(fiwareEnd)
  print(paste('Fiware service:',fiwareService,fiwareServicePath))
  print(paste('Foi:',sensorId,observableProperty))
  fiwareLoopStart <- as.POSIXct(fiwareStart, format="%Y-%m-%dT%H:%M:%S")  #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
  fiwareLoopEnd <-as.POSIXct(fiwareEnd, format="%Y-%m-%dT%H:%M:%S")
  resultMax<-fiwareLoopStart
  nr<-0
  rTot<-' '
  while (resultMax<fiwareLoopEnd) {
    fiwareTmpStart<-strftime(fiwareLoopStart,format = "%Y-%m-%dT%H:%M:%S")
    fiwareTmpEnd<-strftime(fiwareLoopEnd,format = "%Y-%m-%dT%H:%M:%S")
    r<-fiwareGetPeriodSlice(fiwareTmpStart,fiwareTmpEnd,fiwareService,fiwareServicePath,observableProperty,sensorId);
    print(names(r))
    print('aantal kolommen: ')
    print(ncol(r))
    #if (r[1]==" ") {
    if (ncol(r)==4) {
      print("verwerk geen records")
      resultMax<-as.POSIXct('3000-12-31T00:00:00', format="%Y-%m-%dT%H:%M:%S");
    } else {
      if (nrow(r)>0) {
        if (nr==0) {
          rTot<-r
        } else {
          rTot<-rbind(rTot,r);
        }
        nr<-nr+nrow(r)
        resultMax <- as.POSIXct(max(r$dateObserved), format="%Y-%m-%dT%H:%M:%S")+1
        if(nrow(r)<1000){  # al records retrieved, stop reading
          resultMax<-as.POSIXct('3000-12-31T00:00:00', format="%Y-%m-%dT%H:%M:%S");
        }
      } else {
        resultMax<-as.POSIXct('3000-12-31T00:00:00', format="%Y-%m-%dT%H:%M:%S");
      } 
    } 
    print(fiwareLoopStart)
    print(resultMax)
    fiwareLoopStart<-resultMax
  }
  print(observableProperty)
  print('aantal kolommen totaal: ')
  print(ncol(rTot))
  if (is.null(rTot)) {
  #if (rTot==' ') {
#    length(rTot)<-4;
#    print("------------------------")
#    print(nrow(rTot))
#    dfOut<-rTot;
  } else {
    if (nrow(rTot)>0) {
      dfOut<-rTot
      print(observableProperty)
      print(colnames(dfOut))
      print(colnames(dfOut)[colnames(dfOut)==observableProperty])
      colnames(dfOut)[colnames(dfOut)==observableProperty] <- "sensorValue"
      print(colnames(dfOut))
      dfOut<-dfOut[3:5];
      print(colnames(dfOut))
      dfOut$sensorType<-alias;
    } else {
      length(rTot)<-4;
      dfOut<-rTot;
    } 
  }
  newSensorId<- paste(dfOut$sensorId,sensorIdSuffix,sep='')
#  print('sensorId')
#  print(dfOut$sensorId)
#  print('sensorId new')
  dfOut$sensorId<-newSensorId
#  print(dfOut$sensorId)
  print(length(dfIn)) 
  if (length(dfIn)==1) {
    if (rTot==' ') {
      dfResult<-dfIn  
    } else {
      dfResult<-dfOut  
   }
  } else {
    if (rTot==' ') {
      dfResult<-dfIn  
    } else {
      dfResult<-rbind(dfIn,dfOut)
    }
  }
  return (dfResult);
}

