library(scales)

apriSensorPlotSingle<-function(dfTotal,dfFois,sensorTypes,foiLabel,foiText,ylim,treshold=NULL,tresholdLabel=NULL,dateBreaks="1 hour",dateLabels="%H",aggregateTxt='gemiddeld per minuut') {
  plotDateTime<- Sys.time() #+ (as.numeric(format(Sys.time(),'%z'))/100)*60*60;
  captionText<-paste0('Datum: ',format(plotDateTime,"%d-%m-%Y %H:%M"))
  statsPosX<-min(dfTotal$date) #+60*60
  statsPosXMax<-max(dfTotal$date) #+60*60
  statsXResolution<-(statsPosXMax-statsPosX)/32
  statsMin<-round(min(dfTotal$sensorValue),digits=1)
  statsMean<-round(mean(dfTotal$sensorValue),digits=1)
  statsMax<-round(max(dfTotal$sensorValue),digits=1)
  statsResolution<-(statsMax-statsMin)/16
  gTotal <-ggplot(data=dfTotal, aes(x=date,y=sensorValue,colour=foiLocation)
                  ,col = brewer.pal(n = 8, name = "RdYlBu")) +
    theme_bw()
  #    stat_summary(fun.y = mean, geom="line", size=0.5,color='grey') +
  #    stat_smooth(method="loess",span=0.2,size=0.1,se = FALSE,show.legend=FALSE) + #, linetype = "dashed") +
  #scale_y_continuous( sec.axis=sec_axis(~.*1),limits = ylim) + #c(0,max(dfTotal$sensorValue))) +
  if(is.null(ylim)) {
    print("ylim is NULL")
    gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1))
  } else {
    print("ylim is not NULL")
    gTotal<-gTotal+scale_y_continuous( sec.axis=sec_axis(~.*1),limits = ylim)
  }
  print(dateBreaks)
  print(dateLabels)
  
  gTotal<-gTotal+  scale_x_datetime(date_breaks = dateBreaks, date_labels=dateLabels ,timezone='CET',breaks=waiver()) +
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
  print('annotation')

  if(is.null(ylim)!=TRUE) {
    print("test1")
    gTotal<-gTotal +
      annotate("text", x = statsPosX, y = statsMax-statsResolution*1, label = paste0("Max: ",statsMax),size=1,hjust=0) +
      annotate("text", x = statsPosX, y = statsMax-statsResolution*2, label = paste0("Gem: ",statsMean),size=1,hjust=0) +
      annotate("text", x = statsPosX, y = statsMax-statsResolution*3, label = paste0("Min: ",statsMin),size=1,hjust=0)
  }
  if(is.null(treshold)!=TRUE) {
    print("test2")
    gTotal<-gTotal +
      geom_hline(yintercept = treshold,size=0.10,color='darkgreen') +
      annotate("text", x = statsPosX, y = treshold+statsResolution*0.5, label = tresholdLabel,size=1.1,hjust=0) +
      annotate("text", x = statsPosX+statsXResolution*14, y = treshold+statsResolution*0.5, label = tresholdLabel,size=1.1,hjust=0) +
      annotate("text", x = statsPosX+statsXResolution*28, y = treshold+statsResolution*0.5, label = tresholdLabel,size=1.1,hjust=0)
  }
  
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
  sprintf("%s is best", "1")
  filePath<-plotPath
  if (!is.null(fileSuffix)) fileSuffix<-paste0('-',fileSuffix)
  if (!is.null(fileDate)) fileDate<-paste0('-',fileDate)
  fileName <- paste('aprisensor','_',fileLabel,fileSuffix,'.png',sep='')
  
  ggsave(fileName, path=filePath, width=width, height=height, plot = apriSensorPlot,units=units)
  #ggsave(fileName, path=filePath, width=3.8, height=height, plot = apriSensorPlot,dpi=dpi,units=units)
  
  plotImg <- image_read(paste(filePath,'/',fileName,sep=''))
  logo <- logo %>%
    image_annotate("Powered By FIWARE", color = "black", size = 30,
                   location = "+16-3", gravity = "southeast")
  final_plot <- image_composite(plotImg, image_scale(logo,"140"), gravity = "southeast", offset = "+40+40")
  if (subFolder != '') {
    subFolder<-paste0(subFolder,'/')
  }
  print(plotPath)
  print(fileName)
  image_write(final_plot, paste0(plotPath,fileName))
}

