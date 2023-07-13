
library(scales)
library(tidyr)

# Rscript apri-sensor-generic-plot.R AFF4-pm25-csv-locatie-2

apriSensorPlotSingle<-function(dfTotal,dfFois,sensorTypes,foiLabel,foiText,ylim,
    treshold=NULL,tresholdLabel=NULL,
    dateBreaks="1 hour",dateLabels="%H",aggregateTxt='gemiddeld per minuut',
    yzoom=NULL,
    incident=F) {
  plotDateTime<- Sys.time() #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
  captionText<-paste0('Datum: ',format(plotDateTime,"%d-%m-%Y %H:%M"))
  statsPosX<-min(dfTotal$date, na.rm = TRUE) #+60*60
  statsPosXMax<-max(dfTotal$date, na.rm = TRUE) #+60*60
  statsXResolution<-(statsPosXMax-statsPosX)/32
  statsMin<-round(min(dfTotal$sensorValue, na.rm = TRUE),digits=1)
  statsMean<-round(mean(dfTotal$sensorValue, na.rm = TRUE),digits=1)
  statsMax<-round(max(dfTotal$sensorValue, na.rm = TRUE),digits=1)
  if(is.null(treshold)!=TRUE) {
    if(treshold>statsMax) {
      ylim<-c(0,treshold*1.1)
    }
  }
  statsResolution<-(statsMax-statsMin)/16

  # Incidents signaling
  if (incident==T) {
    
    rcMarge<- 0.7 # 0.7 # marge for rc (richtingscoefficient) Higher is steeper (up or down)
    topValueCorr<- 6 # 5 # 4 # 3 # 2 # marge for top value. Hoger is minder incidenten
    maxTimePerIncident<-9 #8 # assume x minutes per cigarette incident
    rollMedianParam <-121  # 121
    
    print('use rolling median')
    #    dfTotalMa<-rollmean(dfTotal$sensorValue,k=5, fill = if (na.pad) NA, na.pad = FALSE)
    # dfTotal$ma<-rollmean(dfTotal$sensorValue,61, fill = NA)
    dfTotal$sensorValueMa<-rollmedian(dfTotal$sensorValue,3, na.pad=T)
    dfTotal$diffDateLag<-as.numeric(dfTotal$date-lag(dfTotal$date,2))
    dfTotal$rcLag<-round((dfTotal$sensorValueMa-lag(dfTotal$sensorValueMa,2)) / dfTotal$diffDateLag, digits=3)
    dfTotal$diffDateLead<-as.numeric(lead(dfTotal$date,2)-dfTotal$date)
    dfTotal$rcLead<-round((lead(dfTotal$sensorValueMa,2)-dfTotal$sensorValueMa) / dfTotal$diffDateLead, digits=3)
    
    #print('############ 1')
    # Determine rolling median not using ('extreme') high and low values 
    dfTotal$sensorValue2<-dfTotal$sensorValueMa
    dfTotal$sensorValue2[which(dfTotal$rcLag<0 & dfTotal$rcLag< (-1*rcMarge) )]<-NA
    dfTotal$sensorValue2[which(dfTotal$rcLag>0 & dfTotal$rcLag> rcMarge)]<-NA
    dfTotal$sensorValue2[which(dfTotal$rcLead<0 & dfTotal$rcLead< (-1*rcMarge) )]<-NA
    dfTotal$sensorValue2[which(dfTotal$rcLead>0 & dfTotal$rcLead> rcMarge)]<-NA
    # (re)fill NA records
    dfTotal<-dfTotal %>% tidyr::fill(sensorValue2, .direction = 'downup')
    dfTotal$ma<-rollmedian(dfTotal$sensorValue2,rollMedianParam, na.pad=T)  # 121 # +1 # extra marge for rolling median
    # fill NA with previous/upcoming value
    dfTotal<-tidyr::fill(dfTotal, ma, .direction = 'downup')
    # determine value (topValue) above rolling mean  correction margin)
    dfTotal$topValue<-dfTotal$sensorValueMa-dfTotal$ma-topValueCorr
    dfTotal$topValue[which(dfTotal$topValue <= 0 & (
                (  (dfTotal$rcLag > 0 & dfTotal$rcLag <rcMarge) | 
                   (dfTotal$rcLag < 0 & dfTotal$rcLag >rcMarge*-1) ) 
                  &
                (  (dfTotal$rcLead > 0 & dfTotal$rcLead < rcMarge) | 
                   (dfTotal$rcLead < 0 & dfTotal$rcLead >rcMarge*-1) ) 
    ) ) ] <-NA 
    dfTotal$topValue[which(dfTotal$topValue < 0 )] <-0 
    dfTotal$topValue[which(dfTotal$topValue == 0 & (is.na(lag(dfTotal$topValue))|lead(dfTotal$topValue)==0)  & (is.na(lead(dfTotal$topValue))|lead(dfTotal$topValue)==0) )] <-NA 
    dfTotal$topValue[which(dfTotal$topValue == 0 & is.na(lag(dfTotal$topValue)) & is.na(lead(dfTotal$topValue)) )] <-NA 
#    dfTotal$topValue[which(dfTotal$topValue >= 0 )] <- dfTotal$topValue[which(dfTotal$topValue >= 0 )] - maCorr #- rcMarge
    #dfTotal$piek<-dfTotal$sensorValue
    #dfTotal$piek[which(is.na(dfTotal$topValue) )]<-NA
    
    #print(split(dfTotal$topValue, is.na(dfTotal$topValue)))
    #print(split(dfTotal, cumsum(c(TRUE, diff(dfTotal$topValue >= 0) != 0))) )
    #    tmp<-split(dfTotal, cumsum(c(TRUE, diff(!is.na(dfTotal$topValue)) != 0L)),drop=T) 
    #    str(tmp)
    #    tmp$xx<-which(tmp$topValue>0)
    #    str(tmp)
    #    incidentNr=0;
    dfTotal$incidentNr<-NA;
    #    for ( i in 1:nrow(dfTotal) ) {
    #    print(head(dfTotal));
    #    dfTotal$incidentnr<-1;
    #    print(head(dfTotal));
    #    print(dfTotal[[1]])
    dfTotal$incidentNr[which(dfTotal$topValue>=0 & is.na(lag(dfTotal$topValue))
                             )] <- which(dfTotal$topValue>=0 & is.na(lag(dfTotal$topValue) ))
    #    dfTotal$incidentNr[which(dfTotal$topValue>=0 & !is.na(lag(dfTotal$incidentNr))
    #    )] <- which(dfTotal$topValue>=0 & is.na(lag(dfTotal$topValue) )
    #    print(dfTotal$incidentNr)
    print(nrow(dfTotal))
    dfTotal$incidentNr<-na.locf0(dfTotal$incidentNr )
    dfTotal$incidentNr[which(is.na(dfTotal$topValue))] <- NA
    print(dfTotal$incidentNr)
    #    print(length( na.locf(dfTotal$incidentNr )))
    #    print(dfTotal$incidentNr )
    #    dfTotal$incidentNr[which( is.na(dfTotal$topValue))] <- na.locf(dfTotal$incidentNr )
    #    print(dfTotal$incidentNr)
    #      {
    #        incidentNr<-incidentNr+1;
    #      }
    #      if (dfTotal[i]$topValue>=0 ) {
    #        dfTotal[i]$incidentNr<-incidentNr;
    #      }
    #    }
    
    dfIncidents<-dfTotal

    # slechts 1 periode met lage piek niet als incident zien
   # dfIncidents<-subset(dfIncidents,!(lag(dfIncidents$incidentNr)!=dfIncidents$incidentNr&lead(dfIncidents$incidentNr)!=dfIncidents$incidentNr&dfIncidents$topValue<5) )
    
    dfIncidents<-subset(dfIncidents,!is.na(dfIncidents$topValue)&dfIncidents$topValue>2)
    # date cycle from 4AM till 4AM
    dfIncidents$day<-as.POSIXct(format(dfIncidents$date-4*60*60,'%Y-%m-%d'), format="%Y-%m-%d")
    dfIncidents$dayHour<-as.POSIXct(format(dfIncidents$date,'%Y-%m-%dT%H'), format="%Y-%m-%dT%H")
#    print(dfIncidents$dayHour)
#    print(dfIncidents$day)
    print("head(dfIncidents)")
    print(head(dfIncidents));
    print(dfIncidents[c('date','sensorValue','diffDateLag','diffDateLead','rcLag','rcLead','sensorValueMa','ma','topValue','incidentNr')]);
    
    dfIncidentDay <- dfIncidents %>% 
      group_by(incidentNr)  %>%
      summarise(dateStart=min(date),
                dateEnd=max(date),
                day=min(day),
                dayHour=min(dayHour),
                max_value = max(topValue),
                total_count = n(),
                incidentScore = ceiling(n()/maxTimePerIncident), # assume 5 minutes per cigarette
                .groups = 'drop') %>% 
      as.data.frame()
    print(dfIncidentDay)
    
    dfIncidentDayHour <- dfIncidents %>% 
      group_by(incidentNr)  %>%
      summarise(dateStart=min(date),dateEnd=max(date),
                day=min(day),
                dayHour=min(dayHour),
                max_value = max(topValue),
                total_count = n(),
                incidentScore = ceiling(n()/maxTimePerIncident), # assume 5 minutes per cigarette
                .groups = 'drop') %>% 
      as.data.frame()
    
    dfIncidentDayHourStats <- dfIncidentDay %>% 
      group_by(dayHour)  %>%
      summarise(dayHour=min(dayHour),
                max_value = max(max_value),
                count = sum(incidentScore),
                .groups = 'drop') %>% 
      as.data.frame()
    print(dfIncidentDayHourStats)
    
    dfIncidentStats <- dfIncidentDay %>% 
      group_by(day)  %>%
      summarise(day=min(day),
                max_value = max(max_value),
                count = sum(incidentScore),
                .groups = 'drop') %>% 
      as.data.frame()
    dfIncidentStats$date<-as.POSIXct(dfIncidentStats$day, format="%Y-%m-%d")
    #dfIncidents$day<-as.POSIXct(format(dfIncidents$date-4*60*60,'%Y-%m-%d'), format="%Y-%m-%d")
    
    print(dfIncidentStats)
  }
  
  gTotal <-ggplot(data=dfTotal, aes(x=date,y=sensorValue,colour=foiLocation)
                  ,col = brewer.pal(n = 8, name = "RdYlBu")) +
    theme_bw();

  #    stat_summary(fun.y = mean, geom="line", size=0.5,color='grey') +
  #    stat_smooth(method="loess",span=0.2,size=0.1,se = FALSE,show.legend=FALSE) + #, linetype = "dashed") +
  #scale_y_continuous( sec.axis=sec_axis(~.*1),limits = ylim) + #c(0,max(dfTotal$sensorValue))) +
  if(!is.null(yzoom)) {
    print("yzoom is not NULL")
    gTotal<-gTotal+scale_y_continuous( limits = yzoom )
  }
  if(is.null(ylim)) {
    print("ylim is NULL")
    gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1))
  } else {
    print("ylim is not NULL")
    if (statsMax>50000) {
      #gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1),limits = ylim,labels = label_number(suffix = " K", scale = 1e-3))
      #gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1,breaks = seq(statsMin, statsMax, round((statsMax-statsMin)/8/10)*10),labels = label_number(suffix = " K", scale = 1e-3)),limits = ylim,labels = label_number(suffix = " K", scale = 1e-3))
      gTotal<-gTotal+scale_y_continuous(n.breaks = 8, sec.axis=sec_axis(~.*1,labels = label_number(suffix = " K", scale = 1e-3)),limits = ylim,labels = label_number(suffix = " K", scale = 1e-3))
      #gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1),limits = ylim,sec.labels = label_number(suffix = " K", scale = 1e-3),labels = label_number(suffix = " K", scale = 1e-3))
    } else {
      gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1),limits = ylim)
    }
  }

  #print(dateBreaks)
  #print(dateLabels)
  #gTotal<-gTotal+  scale_x_datetime(date_breaks = dateBreaks, date_labels=dateLabels ,timezone='CET',breaks=waiver()) +
  dt<-difftime(statsPosXMax,statsPosX,units='hours')
  print(dt)
  if (dt<30) {
    print('breaks <30 hours')
    dateBreaks<-'1 hours'
    gTotal<-gTotal+  scale_x_datetime(date_breaks = dateBreaks, date_labels=dateLabels ,timezone='CET',breaks=waiver())
  } else {
    if (dt<75) {
      print('breaks <75 hours')
      dateBreaks<-'2 hours'
      gTotal<-gTotal+  scale_x_datetime(date_breaks = dateBreaks, date_labels=dateLabels ,timezone='CET',breaks=waiver())
  #    gTotal<-gTotal+  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
  #                                               to = max(x), 
  #                                               by = "3 hours"),
  #              minor_breaks = function(x) seq.Date(from = min(x), 
  #                                                  to = max(x), 
  #                                                  by = "1 hour"))
    } else {
      if (dt<500) {
        print('breaks <500 hours')
        dateBreaks<-'1 days';dateLabels="%d"
        gTotal<-gTotal+  scale_x_datetime(date_breaks = dateBreaks, date_labels=dateLabels ,timezone='CET',breaks=waiver())
        #    gTotal<-gTotal+  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
        #                                               to = max(x), 
        #                                               by = "3 hours"),
        #              minor_breaks = function(x) seq.Date(from = min(x), 
        #                                                  to = max(x), 
        #                                                  by = "1 hour"))
      } else {
        print('breaks else')
      gTotal<-gTotal+  scale_x_datetime()
      }
    }
  }
  
  gTotal<-gTotal+
#  gTotal<-gTotal+  scale_x_datetime(date_breaks = dateBreaks, date_labels=dateLabels ,timezone='CET',breaks=waiver()) +
    theme(text = element_text(size = rel(2.0))
          , element_line(colour = 'green', size = 0.2)
          , plot.title = element_text(face="bold",size = rel(3.2), hjust =0,margin=margin(0,0,0,0)) # 0.5)  #lineheight=rel(1),
          , plot.subtitle=element_text(size = rel(2.2), hjust =0,margin=margin(3,0,8,0)) # 0.5) #,face="bold")
          , plot.caption=element_text(size = rel(1.5),hjust=0,color = "black", face="italic")
          #        , plot.caption.position =  "plot"
          , axis.text=element_text(size = rel(0.9))
          , axis.text.y.right=element_text(size = rel(0.9))
          , legend.text=element_text(size = rel(1.9))
          , legend.title=element_text(size = rel(2.0)) #,face="bold")
          , legend.position="top"
          , legend.justification="right"
          , legend.margin=margin(0,0,0,0)
          , legend.box.margin=margin(-10,-10,-10,-10) # t r b l
          , panel.border = element_rect(colour = "black", fill=NA, size=0.2)
          , legend.key.height=unit(0.5,"line")
          , legend.key = element_rect(color = NA, fill = NA)
          , legend.key.width=unit(0.3,"cm")
    )  +
    geom_line(aes(group=interaction(sensorId,sensorType,type)),size=0.15)+ #group=foi))+#
    guides(color = guide_legend(override.aes = list(size = 1.0) ) ) +
    labs(x=paste('Ruwe en of ongevalideerde meetwaarde ',aggregateTxt,'\nperiode: ',periodetext1,' tm ',periodetext2,sep=''),
         y='meetwaarde',title=paste("ApriSensor ",foiLabel), subtitle=foiText, caption=captionText) +
    facet_grid( sensorType ~ . , labeller=labeller(sensorType = unlist(sensorTypes[dfTotal$sensorType],use.names=T)), scales = "free") +
    #annotate("text", x = statsPosX, y = statsMax-statsResolution*1, label = paste0("Max: ",statsMax),size=1,hjust=0) +
    #annotate("text", x = statsPosX, y = statsMax-statsResolution*2, label = paste0("Gem: ",statsMean),size=1,hjust=0) +
    #annotate("text", x = statsPosX, y = statsMax-statsResolution*3, label = paste0("Min: ",statsMin),size=1,hjust=0) +
    theme(
      strip.text.y = element_text(size = rel(2.6)),
      strip.background = element_rect(colour="black", fill="grey", size=0.3)
    )
  #print('annotation')

  if(is.null(ylim)!=TRUE) {
    gTotal<-gTotal +
      annotate("text", x = statsPosX, y = statsMax-statsResolution*1, label = paste0("Max: ",statsMax),size=1,hjust=0) +
      annotate("text", x = statsPosX, y = statsMax-statsResolution*2, label = paste0("Gem: ",statsMean),size=1,hjust=0) +
      annotate("text", x = statsPosX, y = statsMax-statsResolution*3, label = paste0("Min: ",statsMin),size=1,hjust=0)
  }

  if(is.null(treshold)!=TRUE) {
    #print('geom_line treshold')
    #if(treshold<statsMax) {
      gTotal<-gTotal +
        geom_hline(yintercept = treshold,size=0.10,color='darkgreen') +
        #annotate("text", x = statsPosX, y = treshold-statsResolution*0.5, label = tresholdLabel,size=1.1,hjust=0) +
        #annotate("text", x = statsPosX+statsXResolution*7, y = treshold-statsResolution*0.5, label = tresholdLabel,size=1.1,hjust=0,color='darkgreen') +
        annotate("text", x = statsPosX+statsXResolution*15, y = treshold-statsResolution*0.5, label = tresholdLabel,size=1.1,hjust=0,color='darkgreen')
    #}
  }

  if (incident==T) {
            gTotal<-gTotal + geom_line(data=dfTotal,aes(x=date,y=sensorValueMa),colour='green',size=0.2)
            gTotal<-gTotal + geom_line(data=dfTotal,aes(x=date,y=ma),colour='yellow',size=0.2)
        gTotal<-gTotal + geom_line(data=dfTotal,aes(x=date,y=topValue),colour='black',size=0.1) 
        gTotal<-gTotal + geom_point(data=dfTotal,aes(x=date,y=topValue),colour='black',size=0.1) 
    #    gTotal<-gTotal + geom_point(data=dfIncidentDayHourStats,aes(x=dayHour,y=count*2),colour='blue',size=0.1) 
    #    gTotal<-gTotal + geom_point(data=dfIncidentStats,aes(x=day,y=count*2),colour='blue',size=0.1) 
      
    
    dfIncidentStats$foiLocation<-'loc.'
    minDate<-min(dfIncidentStats$date)
    if (minDate>=min(dfIncidentStats$date)) {
      graphStart<-minDate
    } else {
      graphStart<-min(dfIncidentStats$date)
    }
    print(head(dfIncidentStats))
    statsPosX<-min(dfIncidentStats$date, na.rm = TRUE) #+60*60
    statsPosXMax<-max(dfIncidentStats$date, na.rm = TRUE) #+60*60
    statsXResolution<-(statsPosXMax-statsPosX)/32
    #gTotal<-gTotal + geom_point(data=dfIncidentStats,aes(x=graphStart,y=count*2),colour='blue',size=0.1) 
    #gTotal<-gTotal + geom_text(data=dfIncidentStats,aes(x=graphStart+statsXResolution,y=statsMax-statsResolution*2.5,label=paste0(count)),colour='black' ,size=1.4,hjust=0,vjust=0)
    #gTotal<-gTotal + geom_text(data=dfIncidentStats,aes(x=day+statsXResolution*4,y=statsMax-statsResolution*2.5,label=paste0(count)),colour='black' ,size=1.4,hjust=0,vjust=0)
    #gTotal<-gTotal + geom_text(data=dfIncidentStats,aes(x=date+statsXResolution,y=statsMax-statsResolution*2.5,label=paste0(count)),colour='black' ,size=1.4,hjust=0,vjust=0)
    gTotal<-gTotal + geom_text(data=dfIncidentStats,aes(x=date+60*60*17,y=statsMax+statsResolution*1,label=paste0(count)),colour='black' ,size=1.4,hjust=0,vjust=0)
    #gTotal<-gTotal + geom_text(data=dfIncidentStats,aes(x=date+60*60*14,y=statsMax-statsResolution*2.5,label=paste0(count)),colour='black' ,size=1.4,hjust=0,vjust=0)
    
    gTotal<-gTotal +
     #annotate("text", x = statsPosX+60*60*14, y = statsMax-statsResolution*1, label = paste0("Incident index: "),size=1.4,hjust=0)
     annotate("text", x = statsPosX+60*60*14, y = statsMax+statsResolution*2.5, label = paste0("Incident index: "),size=1.4,hjust=0)
    #    if (nrow(dfIncidentStats) >=1) {
#      gTotal<-gTotal +      annotate("text", x = statsPosX, y = statsMax-statsResolution*2, label = paste0("  ",dfIncidentStats$day[[1]],': ',dfIncidentStats$count[[1]],'x'),size=1.2,hjust=0)
#    }
#    if (nrow(dfIncidentStats) >=2) {
#      gTotal<-gTotal +  annotate("text", x = statsPosX, y = statsMax-statsResolution*3, label = paste0("  ",dfIncidentStats$day[[2]],': ',dfIncidentStats$count[[2]],'x'),size=1.2,hjust=0)
#    } 
#    if (nrow(dfIncidentStats) >=3) {
#      gTotal<-gTotal +     annotate("text", x = statsPosX, y = statsMax-statsResolution*4, label = paste0("  ",dfIncidentStats$day[[3]],': ',dfIncidentStats$count[[3]],'x'),size=1.2,hjust=0)
#    } 
#    if (nrow(dfIncidentStats) >=4) {
#      gTotal<-gTotal + annotate("text", x = statsPosX, y = statsMax-statsResolution*5, label = paste0("  ",dfIncidentStats$day[[4]],': ',dfIncidentStats$count[[4]],'x'),size=1.2,hjust=0)
#    } 
#    if (nrow(dfIncidentStats) >=5) {
#      gTotal<-gTotal + annotate("text", x = statsPosX, y = statsMax-statsResolution*6, label = paste0("  ",dfIncidentStats$day[[5]],': ',dfIncidentStats$count[[5]],'x'),size=1.2,hjust=0)
#    } 
#    if (nrow(dfIncidentStats) >=6) {
#      gTotal<-gTotal + annotate("text", x = statsPosX, y = statsMax-statsResolution*7, label = paste0("  ",dfIncidentStats$day[[6]],': ',dfIncidentStats$count[[6]],'x'),size=1.2,hjust=0)
#    } 
#    if (nrow(dfIncidentStats) >=7) {
#      gTotal<-gTotal + annotate("text", x = statsPosX, y = statsMax-statsResolution*8, label = paste0("  ",dfIncidentStats$day[[7]],': ',dfIncidentStats$count[[7]],'x'),size=1.2,hjust=0)
#    } 
  }
  # geom_line(data=as.data.frame(bb),aes(x=b,y=a))
  
  return (gTotal +
            #    scale_colour_manual(values = c(
            #    "red"
            #   ,"blue"
            #   ,"green"
            #   ,"orange"
            #   ,"light blue"
            #   ,"light green"
            #   ,"yellow"
            #   ,"dark red"
            #   ,"dark blue"
            #   ,"dark green"
          #   ,"black"
          #   ,"grey"
          #   ,"black"
          #   ),
          scale_colour_discrete(
            name = "",
            breaks= dfFois$sensorId,
            labels= dfFois$label
          ))
}

apriSensorImage<-function(apriSensorPlot,fileLabel,fileSuffix=NULL,fileDate=NULL,width=3.8,height=2.28,dpi="print",units='in',subFolder='') {
  #sprintf("%s is best", "1")
  if (!is.null(fileSuffix)) fileSuffix<-paste0('-',fileSuffix)
  if (!is.null(fileDate)) fileDate<-paste0('-',fileDate)
  fileName <- paste('aprisensor','_',fileLabel,fileSuffix,'.png',sep='')

  ggsave(fileName, path=plotPath, width=width, height=height, plot = apriSensorPlot,units=units)
  #ggsave(fileName, path=plotPath, width=3.8, height=height, plot = apriSensorPlot,dpi=dpi,units=units)

  plotImg <- image_read(paste(plotPath,'/',fileName,sep=''))
  logo <- logo %>%
    image_annotate("", color = "black", size = 30,
                   location = "+16-3", gravity = "southeast")
  final_plot <- image_composite(plotImg, image_scale(logo,"220"), gravity = "southeast", offset = "+40+20")
  if (subFolder != '') {
    subFolder<-paste0(subFolder,'/')
  }
  print(plotPath)
  print(fileName)
#  image_write(final_plot, paste0(plotPath,'/',fileName))
  image_write(final_plot, paste0(plotPath,'/',subFolder,fileName))
  print(paste0(plotPath,'/',subFolder,fileName))

}
