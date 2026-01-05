#
library('jsonlite')

cmdArgs = commandArgs(trailingOnly = FALSE)
print(paste('script sub/apri-sensor-cams.R','start Rscript: ', Sys.time(),cmdArgs[4]))

getCamsData<-function(dfIn=NULL,
                      dataType='analysis',
                      lat=NULL,lon=NULL,
                      observationTypes=NULL,
                      dateFrom=NULL,
                      dateTo=NULL,
                      periodSpan = NULL
  ) {

  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"

  paramDate<-paste("&dateFrom=",dateFrom,"&dateTo=",dateTo,sep='')
  
  print(periodSpan)
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
  
  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"
  url <- paste("https://aprisensor-api-v1.openiod.org/v1/cams/"
               ,dataType
               ,"?" 
               ,"lat=", lat
               ,"&lon=", lon
               ,"&observationType=", observationTypesUrl #observationType
               #,"&observationTypes=", observationTypes
#               ,"&aggregation=", aggregation
               , paramDate
               ,"&format=json"
               ,"&region=nl"
               ,"&aggregation=hour"
               ,sep='')
  print(url)

  myData <- fromJSON(url)
  dfResult <-myData$features$properties$observations
#  tmp <-tmp$properties$observations
#  print(head(tmp)) #$properties))
#  print(str(tmp)) #$properties))
#  dfResult<-tmp
#  print('===========')
#  print(head(dfResult))
#  print(str(dfResult))
#  print('=========== 2')
  
  dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M:%S")
  dfResult$sensorId<-"CAMS_forecast"
  dfResult$sensorType<-strsplit(observationTypeObject,split=':')[[1]][2] # observationTypeArray[[1]] #'pm25'
  dfResult$sensorValue<-dfResult$observation$pm25
#  print(head(dfResult))
#  print(str(dfResult))
  keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
  dfResult <- dfResult[keeps]
#  print(head(dfResult))
  

  #keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
  #dfResult <- dfResult[keeps]

  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}
