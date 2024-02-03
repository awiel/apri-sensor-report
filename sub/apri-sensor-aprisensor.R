#

cmdArgs = commandArgs(trailingOnly = FALSE)
print(paste('script','start Rscript: ', Sys.time(),cmdArgs[4]))


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


getApriSensorData<-function(dfIn=NULL,dbGroup=NULL,sensorId=NULL,sensorType=NULL,observationTypes=NULL
                        ,aggregation=NULL,dateFrom=NULL,dateTo=NULL,aggregateInd=NULL,cachePath=NULL
                        ,source=NULL,datastream=NULL,periodSpan=NULL
                        ,csvPath='./data/csv/',csvFileName=NULL,csvType='2'
                        ,rdaPath='./data/Rda/',rdaFileName=NULL
  ) {

  useCache<-FALSE
  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"

  #fileName<-paste(paste(fiwareService,str_replace_all(sensorType, '/', '_'),foi,ops,sep='#'),'.Rda',sep='')
  print('test')
  print(dbGroup)
  if (is.null(dbGroup) || is.na(dbGroup)) fileName<-paste(paste('DB',sensorId,sensorType,observationTypes,aggregation,sep='#'),'.rds',sep='')
  else fileName<-paste(paste('DB',sensorId,sensorType,dbGroup,observationTypes,aggregation,sep='#'),'.rds',sep='')
  
  print('test2')
  
  fileName<-gsub(":","_",fileName)

  print(periodSpan)
  if (is.null(periodSpan)) {
    dateFromOldestInCache<-Sys.time()-(24*60*60-60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
  } else {
      if (is.numeric(periodSpan)) {
        dateFromOldestInCache<-Sys.time()-(periodSpan*60*60-60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
      } else {
        dateFromOldestInCache<-Sys.time()-(24*60*60-60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
      }
  }
  if (is.null(dateFrom)) {
    useCache<-TRUE
    if (is.null(periodSpan)) {
      dateFrom<-format(Sys.time()-(24*60*60-60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
                     ,"%Y-%m-%dT%H:%M:%S")
    } else {
      if (is.numeric(periodSpan)) {
        dateFrom<-format(Sys.time()-(periodSpan*60*60-60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
                     ,"%Y-%m-%dT%H:%M:%S")
      } else {
        dateFrom<-format(Sys.time()-(24*60*60-60) - (as.numeric(format(Sys.time(),'%z'))/100)*60*60
                     ,"%Y-%m-%dT%H:%M:%S")
      }
    }
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
  
  print(observationTypes)
  observationTypeArray<-strsplit(observationTypes,split=',')
  observationTypesUrl<-''
  for (observationTypeObject in observationTypeArray[[1]]) {
    if (observationTypesUrl=='') {
      observationTypesUrl<-paste0(observationTypesUrl,strsplit(observationTypeObject,split=':')[[1]][1])
    } else {
      observationTypesUrl<-paste0(observationTypesUrl,',',strsplit(observationTypeObject,split=':')[[1]][1])
    }
  }
  
  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"
  url <- paste("https://aprisensor-api-v1.openiod.org/v1/observations/sensor/"
               , sensorId,'/',sensorType
               ,"?observationTypes=", observationTypesUrl
               ,"&aggregation=", aggregation
                 , paramDate
                 ,sep='')
  print(url)

  myData <- fromJSON(url)
  dfResult<-myData$observations
#  if (aggregation=='minute') {
#    dfResult <-myData$observationsMinute
#    dfResult$dateObserved<-dfResult$dateObservedDate
#  } else if (aggregation=='detail') {
#    dfResult <-myData$observationsDetail
#    dfResult$dateObserved<-dfResult$dateObservedDate
#    dfResult$sensorId <- myData$observations$sensorId[1]
#  } else {
    # dfResult <-myData$observations
#    dfResult$sensorId <- myData$observations$sensorId
#  }
    
  dfMerged<-NULL
  for (observationTypeObject in observationTypeArray[[1]]) {
    tmpObservationType <- strsplit(observationTypeObject,split=':')[[1]][1]
    tmpObservationTypeAlias <- strsplit(observationTypeObject,split=':')[[1]][2]
      
    dfSubSet<- dfResult
    dfSubSet$sensorValue <- dfSubSet[c(tmpObservationType)][,1]
    if (!is.na(tmpObservationTypeAlias)) {
      dfSubSet$sensorType <- tmpObservationTypeAlias
    } else {
      dfSubSet$sensorType <- tmpObservationType
    }
      
    if (is.null(dfMerged)) {
      dfMerged<-dfSubSet
    } else {
      dfMerged<-rbind(dfMerged,dfSubSet)
    } 
  }
  
  dfResult<-dfMerged
    
  if (!is.null(source) && !is.na(source)) {
    if (source == 'csv') {  # source == csv dataset
      # /NBI_TN012/12-pm25
      print('test')
      print(dateFrom)
      print(dateTo)
      
      csvFile<-paste0(csvPath,csvFileName)
      print(paste('csv: (',getwd(),') ',csvFile,' csvType:',csvType))

      if (is.null(csvType)) {
        csvType='2'
      }
      if (csvType=='1') {
        dfResult<-read.csv(csvFile)
        if ("sensorType" %in% colnames(dfResult)==F) {
          dfResult$sensorType<-'pm25'
          dfResult$sensorValue<-dfResult$pm25mlr
        } 
      } else {
        dfResult<-read.csv2(csvFile)
      }
      dfResult<-subset(dfResult,dfResult$dateObserved>=dateFrom)
      dfResult<-subset(dfResult,dfResult$dateObserved<=dateTo)
      dfResult$sensorId<-as.factor(dfResult$sensorId)
      dfResult$sensorType<-as.factor(dfResult$sensorType)
      dfResult$dateObserved<-as.factor(dfResult$dateObserved)
    }
  }

  if (!is.null(source) && !is.na(source)) {
    if (source == 'rda') {  # source == Rda dataset

      rdaFile<-paste0(rdaPath,rdaFileName)
      print(paste('rda: (',getwd(),') ',rdaFile))
      dfResult<-readRDS(rdaFile)
      if ("sensorType" %in% colnames(dfResult)==F) {
        dfResult$sensorType<-'pm25'
        dfResult$sensorValue<-dfResult$pm25mlr
      } 
      tmpDateObservedFrom<-as.POSIXct(dateFrom, format="%Y-%m-%dT%H:%M:%S")
      tmpDateObservedTo<-as.POSIXct(dateTo, format="%Y-%m-%dT%H:%M:%S")
      
      dfResult<-subset(dfResult,dfResult$date>=tmpDateObservedFrom)
      dfResult<-subset(dfResult,dfResult$date<=tmpDateObservedTo)
      dfResult$sensorId<-as.factor(dfResult$sensorId)
      dfResult$sensorType<-as.factor(dfResult$sensorType)
      # Project Castricum
      if (sensorId=='SCRP000000009e652147' ||sensorId=='SCRP000000008b6eb7a5' || sensorId=='SCRP00000000c321df78') {
        dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S")
      }
      dfResult$dateObserved<-as.factor(dfResult$dateObserved)
    }
  }
  
  if (!is.null(source) && !is.na(source)) {
    if (source == 'samenmeten') {  # source == samenmeten api
      # /NBI_TN012/12-pm25
      print('test')
      splitTmp<-strsplit(observationTypes,split=':')[[1]]
      observationTypes<-splitTmp[1]

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


  if (observationTypes=='pm1,pm25,pm10') {
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
         # print("aggregate dfResult per minute")
        #  dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M")
        #  dfResult <- aggregate(sensorValue~sensorId+sensorType+date, data=dfResult, mean, na.rm=TRUE)
        #  dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved to averaged value
          if (aggregateInd == 'A') { # 10 minutes
            print("aggregate dfResult per 10 minute")
            dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M")
            dfResult$minute <- sapply(format(dfResult$date, "%M"), as.numeric)
            dfResult$date<-dfResult$date-(dfResult$minute%%10)*60
            dfResult <- aggregate(sensorValue~sensorId+sensorType+date, data=dfResult, mean, na.rm=TRUE)
            dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved to averaged value
          } else {
            print("aggregate dfResult per minute")
            dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M")
            dfResult <- aggregate(sensorValue~sensorId+sensorType+date, data=dfResult, mean, na.rm=TRUE)
            dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved to averaged value
          }
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

    #if (!is.null(dfResult$sensorType[1]) && dfResult$sensorType[1]=='bme680_gasResistance') {
    print(dfResult$sensorType[1])
   # if(!is.na(dfResult$sensorType[1])) {
      if (dfResult$sensorType[1] == 'bme680_gasResistance') {
        #dfTmpGas<-fiwareGetSensorSelectRecords(NULL,'aprisensor_in','/bme680','sensorId','SCRP00000000504b9dd5','gasResistance:bme680_gasResistance')
        dfResult$sensorValue<-(1000000 - dfResult$sensorValue)/1000
      }
    #}
  }

  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}
