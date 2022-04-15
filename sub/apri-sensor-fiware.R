##

#script.dir <- dirname(sys.frame(1)$ofile)
cmdArgs = commandArgs(trailingOnly = FALSE)
print(paste('script','start Rscript: ', Sys.time(),cmdArgs[4]))


##install.packages("ggplot2")
##install.packages("scales")
##install.packages("ggthemes")
##require(grid) # for unit()
##require(cowplot)
#library(cowplot)
#library(ggthemes)
#library(ggplot2)
#library(grid) # for unit()
#library(methods)
##library(reshape2)
#library(scales)
#library(dplyr)
##library("rjson")
##library(RJSONIO)
##library("RCurl")
#library(jsonlite)
#library(httr)
#library(RColorBrewer)


useCache<-FALSE
pmTop<-5000

readCacheFile<-function(cachePath,fileName) {
  #cacheFilePath<-paste(fileLocation,fileName,sep='')
  #print(cacheFilePath)
#  setwd(fileLocation)
  readRDS(paste0(cachePath,fileName))
}
saveCacheFile<-function(cachePath,fileName,object) {
  print("Save cachefile")
  print(fileName)
#  setwd(fileLocation)
  saveRDS(object, file = paste0(cachePath,fileName))
}


getFiwareData<-function(dfIn=NULL,fiwareService=NULL,fiwareServicePath=NULL,key=NULL,foi=NULL,ops=NULL,opPerRow='true'
                        ,opsc=NULL,dateFrom=NULL,dateTo=NULL,aggregateInd=NULL,cachePath=NULL
                        ,source=NULL,sensorId=NULL,datastream=NULL,sensorType=NULL) {
  # eg2. SCNM5CCF7F2F62F3:SCNM5CCF7F2F62F3_a,pm25:pm25_alias,pm10
  # https://aprisensor-in.openiod.org/apri-sensor-service/v1/getSelectionData/?fiwareService=aprisensor_in&fiwareServicePath=/pmsa003&key=sensorId&foiOps=SCNM5CCF7F2F62F3:SCNM5CCF7F2F62F3_a,pm25:pm25_alias

  #fileName<-paste(paste(fiwareService,str_replace_all(fiwareServicePath, '/', '_'),key,foi,ops,sep='#'),'.Rda',sep='')
  fileName<-paste(paste(foi,fiwareService,gsub("/", "_", fiwareServicePath),ops,key,opPerRow,sep='#'),'.rds',sep='')
  fileName<-gsub(":","_",fileName)

  dateFromOldestInCache<-Sys.time()-(24*60*60-1) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
  if (is.null(dateFrom)) {
    useCache<-TRUE
    dateFrom<-format(Sys.time()-(24*60*60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
                     ,"%Y-%m-%dT%H:%M:%S")
    dateTo<-format(Sys.time() - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
                   ,"%Y-%m-%dT%H:%M:%S")
    print("Standaard periode:")
    print(dateFrom)
    print(dateTo)
  }
  if(useCache==TRUE) {
    cached<-FALSE
    allCache<-FALSE
    if (file.exists (paste0(cachePath,fileName))) {
      print("Load cache:")
      cacheFile <-readCacheFile(cachePath,fileName)
      # volgende regel kan eruit als alle cache is omgezet en date bevat
      #    cacheFile$date<-as.POSIXct(cacheFile$dateObserved, format="%Y-%m-%dT%H:%M")
      maxDateObservedCache<-max(cacheFile$date)
      print(paste("Max from cache is:",maxDateObservedCache))

      tmpDateObservedFrom<-as.POSIXct(dateFrom, format="%Y-%m-%dT%H:%M:%S")
      tmpDateObservedTo<-as.POSIXct(dateTo, format="%Y-%m-%dT%H:%M:%S")
      if (tmpDateObservedFrom < maxDateObservedCache) {
        if (tmpDateObservedTo > maxDateObservedCache) {
          print("Change retrieve date from into max cache date")
          dateFrom<-format(maxDateObservedCache #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60
                           ,"%Y-%m-%dT%H:%M:%S")
        }
        else {
          print("All data from cache")
          dateFrom<-dateTo
          allCache<-TRUE
        }
      }
      cached<-TRUE
      #    if (nrow(cacheFile)>0) {
      #      print("aggregate cache")
      #      cacheFile <- aggregate(sensorValue~sensorId+sensorType+date, data=cacheFile, mean, na.rm=TRUE)
      #    }
      #    # next line is temporary? (until all cache files have are p/min
      #    cacheFile$dateObserved<-format(cacheFile$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved for averaged value
    }
  }
  paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='')

  if (!is.null(fiwareService) && !is.null(fiwareServicePath)) {
    if (fiwareService == '' || fiwareServicePath=='/knmi' || fiwareServicePath=='/tsi3007' || substr(fiwareService,1,6)=='orion-' | substr(fiwareService,1,1)=='#') {
      dbSuffix <-''
    } else {
      if (dateFrom<"2021-05-05T21:12") {
        dbSuffix <-''
      } else {
        dbSuffix<-paste0('_',substr(dateTo,1,4),substr(dateTo,6,7))
        print(dbSuffix)
        #dbSuffix <-'_202111'
      }
    }
  }

  if (substr(fiwareService,1,1)=='#') {
    fiwareService<-substr(fiwareService,2,999)
  }

  if (is.null(source) || (!is.null(source) && source=='fiware')) {
    url <- paste("https://aprisensor-in.openiod.org/apri-sensor-service/v1/getSelectionData/"
                 ,"?fiwareService=",fiwareService,dbSuffix
                 ,"&fiwareServicePath=",fiwareServicePath
                 ,"&key=",key
                 ,"&opPerRow=",opPerRow
                 ,"&foiOps=",foi,",",ops
                 , paramDate
                 ,sep='')
    #,"&dateFrom=",dateFrom
    #,"&dateTo=",dateTo
    print(url)
    dfResult <- read.csv(url, header = TRUE, sep = ";", quote = "\"")
  }

  if (!is.null(source) && !is.na(source)) {
    if (source == 'samenmeten') {  # source == samenmeten api
      # /NBI_TN012/12-pm25
      splitTmp<-strsplit(ops,split=':')[[1]]
      ops<-splitTmp[1]

      url <- paste("https://samenmeten.openiod.org/api/v1/thing/"
                   ,sensorId,"/"
                   ,datastream
                   ,'?sensorType=',sensorType
                   ,paramDate
                   ,sep='')
      print(url)
      dfResult<-jsonlite::fromJSON(url,simplifyDataFrame = TRUE)
      dfResult$sensorId<-as.factor(dfResult$sensorId)
      dfResult$sensorType<-as.factor(dfResult$sensorType)
      dfResult$dateObserved<-as.factor(dfResult$dateObserved)

      if (length(dfResult)>0) {
        if (length(splitTmp)>1) {
          dfResult$sensorType<-splitTmp[2]
        } else {
          dfResult$sensorType<-dfResult$datastream
        }
      }
    }
  }

  dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M:%S")
  keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
  dfResult <- dfResult[keeps]


  if (ops=='pm1,pm25,pm10') {
    dfResultMax <- subset(dfResult, dfResult$sensorValue >= pmTop)
    if (as.numeric(nrow(dfResultMax)>=1)) {
      dfResult1 <- subset(dfResult, dfResult$sensorValue < pmTop)
      dfResultMax$sensorValue<-pmTop
      dfResult<-rbind(dfResult1,dfResultMax)
    }
  }

  if (useCache==FALSE){
    cacheFileNew <- dfResult
  }
  if (useCache==TRUE){
    if (cached==FALSE) cacheFileNew <- dfResult
    if (cached==TRUE) {
      print("Add retrieved records to cache")
      print(paste("In cache was:",nrow(cacheFile)))
      cacheFile <- subset(cacheFile, cacheFile$date > dateFromOldestInCache )
      print(paste("drop hist in cache before ",dateFromOldestInCache,", reduced to:",nrow(cacheFile)))
      cacheFile <- subset(cacheFile, cacheFile$date < maxDateObservedCache )
      print(paste("In cache reduced to:",nrow(cacheFile)))
      #cacheFile <- subset(cacheFile, select = -c(dateObservedDate) ) # drop temporary column
      cacheFileNew <- rbind(cacheFile,dfResult)
    }
    print(paste("In cache is: ",nrow(cacheFileNew)))
    if (nrow(cacheFileNew)>0) {
      saveCacheFile(cachePath,fileName,cacheFileNew)
      print("Cache saved to file")
    }
  }

#### aggregate ? (cache is not aggregated!!)
  dfResult<-cacheFileNew
  if (length(dfResult)>0 && nrow(dfResult)>0) {
    aggrTmp<-FALSE
    print(aggregateInd)
    if (is.null(aggregateInd)) {
      print("test aggregateInd")
    }
    if (is.null(aggregateInd)) aggrTmp <- FALSE
    if (!is.null(aggregateInd)) {
      if (aggregateInd!="N")  aggrTmp <- TRUE
    }
    if (aggrTmp == TRUE) {
      if (!is.null(aggregateInd)) {
        if (aggregateInd == 'D') {
          print("aggregate dfResult per day")
          dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%d")
          dfResult <- aggregate(sensorValue~sensorId+sensorType+date, data=dfResult, mean, na.rm=TRUE)
          dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved to averaged value
        } else {
          print("aggregate dfResult per minute")
          dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M")
          dfResult <- aggregate(sensorValue~sensorId+sensorType+date, data=dfResult, mean, na.rm=TRUE)
          dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved to averaged value
        }
      } else {
        print("aggregate dfResult none")
        #dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M:%S")
        #keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
        #dfResult <- dfResult[keeps]
      }
    } else {
      print("aggregate dfResult none 2")
      #print("aggregate dfResult none plus keeps")
      #dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M:%S")
      #keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
      #dfResult <- dfResult[keeps]
    }

    if (!is.null(dfResult$sensorType[1]) && dfResult$sensorType[1]=='bme680_gasResistance') {
      #dfTmpGas<-fiwareGetSensorSelectRecords(NULL,'aprisensor_in','/bme680','sensorId','SCRP00000000504b9dd5','gasResistance:bme680_gasResistance')
      dfResult$sensorValue<-(1000000 - dfResult$sensorValue)/1000
    }
  }

  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}
