#
library('jsonlite')

cmdArgs = commandArgs(trailingOnly = FALSE)
print(paste('script sub/apri-sensor-cams.R','start Rscript: ', Sys.time(),cmdArgs[4]))

getCamsData<-function(dfIn=NULL,lat=NULL,lon=NULL,observationTypes=NULL
                        ,aggregation=NULL,dateFrom=NULL,dateTo=NULL,cachePath=NULL
  ) {

  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"

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
  
  # eg curl "https://aprisensor-api-v1.openiod.org/v1/observations/sensor/SCRP0000001234AB/pmsa003?aggregation=detail,dateFrom=2023-11-21T13:30:00"&dateTo=2023-11-22T13:30:00"
  url <- paste("https://aprisensor-api-v1.openiod.org/v1/cams/analysis"
               ,"?observationTypes=", observationTypes
               ,"&aggregation=", aggregation
               ,"&lat=", lat
               ,"&lon=", lon
               , paramDate
                 ,sep='')
  print(url)

  myData <- fromJSON(url)
  dfResult<-myData

  dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H:%M:%S")

  #keeps <- c("sensorId","sensorType","date", "sensorValue","dateObserved")
  #dfResult <- dfResult[keeps]

  if (is.null(dfIn)) return(dfResult)
  else return(rbind(dfIn,dfResult) )
}
