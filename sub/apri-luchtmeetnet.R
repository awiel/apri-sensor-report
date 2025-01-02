#
library('jsonlite')

cmdArgs = commandArgs(trailingOnly = FALSE)
print(paste('script sub/apri-sensor-luchtmeetnet.R','start Rscript: ', Sys.time(),cmdArgs[4]))

getLuchtmeetnetData<-function(dfIn=NULL
                              ,sensorId=NULL
                              ,sensorIdAlias=NULL
                              ,observationTypes=NULL
                              ,dateFrom=NULL,dateTo=NULL
                              ,periodSpan = NULL
                        
  ) {

  # eg curl "https://aprisensor-api-v1.openiod.org/v1/luchtmeetnet/LUCHTMEETNETNL01496?,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"

  paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='')
  
  #observationTypeArray<-strsplit(observationTypes,split=',')
  #observationTypesUrl<-''
  #for (observationTypeObject in observationTypeArray[[1]]) {
  #  if (observationTypesUrl=='') {
  #    observationTypesUrl<-paste0(observationTypesUrl,strsplit(observationTypeObject,split=':')[[1]][1])
  #  } else {
  #    observationTypesUrl<-paste0(observationTypesUrl,',',strsplit(observationTypeObject,split=':')[[1]][1])
  #  }
  #}
  
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
  
  paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='')
  
  observationTypeArray<-strsplit(observationTypes,split=',')
  observationTypesUrl<-''
  for (observationTypeObject in observationTypeArray[[1]]) {
    if (observationTypesUrl=='') {
      observationTypesUrl<-paste0(observationTypesUrl,strsplit(observationTypeObject,split=':')[[1]][1])
    } else {
      observationTypesUrl<-paste0(observationTypesUrl,',',strsplit(observationTypeObject,split=':')[[1]][1])
    }
  }
  
  
  
  url <- paste("https://aprisensor-api-v1.openiod.org/v1/luchtmeetnet/",sensorId
               ,"?observationTypes=", observationTypesUrl
               , paramDate
                 ,sep='')
  print(url)

  myData <- fromJSON(url)
  dfResult<-myData$observations

  recordsFound <-TRUE
  dfMerged<-NULL
  for (observationTypeObject in observationTypeArray[[1]]) {
    tmpObservationType <- strsplit(observationTypeObject,split=':')[[1]][1]
    tmpObservationTypeAlias <- strsplit(observationTypeObject,split=':')[[1]][2]
    
    print(nrow(dfResult))
    print(str(dfResult))
    print(dfResult)
    print(tmpObservationType)
#    print(dfResult[c(tmpObservationType)][,1])
    
    if("pm25" %in% colnames(dfResult))
    {
      if (nrow(dfResult)>0) {
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
    } 
  }
  dfResult<-dfMerged

  dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M:%S")
  dfResult$sensorId<-sensorId
#  dfResult$sensorType<-'pm25'
#  dfResult$sensorValue<-dfResult$pm25
  keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
  dfResult <- dfResult[keeps]

  #keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
  #dfResult <- dfResult[keeps]

    if (is.null(nrow(dfResult))) {
    return(dfIn)
  }
  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}
