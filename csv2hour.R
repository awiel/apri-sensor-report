
# Template to aggregate measurements to values per hour

csvFile<-'test.csv' # file or url to REST-api
dfResult <- read.csv(csvFile, header = TRUE, sep = ";", quote = "\"", stringsAsFactors=FALSE)

# In this example:
#  - dateObserved contains datetime in ISO-format e.g. '2021-07-27T01:00:10'
#  - sensorValue is measured value e.g. 12.4
#  - sensorId is ID of sensor 'ABC12345'
#  - sensorType is type of sensor e.g. 'pm25' or 'temperature'
dfResult$date<-as.POSIXct(dfResult$dateObserved, format="%Y-%m-%dT%H")
dfResult <- aggregate(sensorValue~sensorId+sensorType+date, data=dfResult, mean, na.rm=TRUE)
dfResult$dateObserved<-format(dfResult$date,"%Y-%m-%dT%H:%M:%S") # restore dateObserved to averaged value

write.csv(dfResult,paste0('hour_',csvFile), row.names = FALSE)

