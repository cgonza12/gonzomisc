require(ggplot2)
getPlot.simple <-  function(mydata,
                            xVar='varname',yVar='estimate',
                            LL ="LL",UL = "UL",
                            yloc = .05,
                            textSize = 16,
                            coordFlip = T,
                            asPct = T,
                            addDataLabs = T,
                            addErrorBar=T, ebWidth = .01,
                            xlabname="",ylabname="% respondents"){
  mydata <- as.data.frame(mydata)
  mydata$yVar.pct <- makePercent(mydata[,yVar])
  
  p <- ggplot(mydata,aes_string(x=xVar,y=yVar,ymin=LL,ymax=UL))+
    geom_bar(stat='identity',position = position_dodge(.5),width=.5,
             fill='lightblue',color='grey50')+
    geom_point(position = position_dodge(.5),
               color='grey10')+
    ylab(ylabname)+
    xlab(xlabname)+
    guides(fill= F)+
    theme_bw()
   
  
  if(addErrorBar){p <- p+geom_errorbar(position = position_dodge(.5),width=ebWidth,color='grey10')
  }
  
  if(addDataLabs){p <- p+geom_text(aes_string(y='yloc',label='yVar.pct'),position = position_dodge(.5))
  }
  
  if(asPct){p <- p+scale_y_continuous(ylabname,labels = percent)}
  
  if(coordFlip){p <- p+coord_flip()+
    theme(text=element_text(size=textSize),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())
  
  }else{p <- p+theme(text=element_text(size=textSize),
                     panel.grid.major.x=element_blank(),
                     panel.grid.minor.x=element_blank())}
  
  return(p)
}

getPlot.group <- function(mydata, xVar='aggregateVar',yVar='estimate',
                          LL ="LL",UL = "UL",
                          groupVar='groupVar',fill='groupVar',
                          yloc = .05,textSize = 16,
                          coordFlip = T,
                          asPct = T,
                          addDataLabs = T,
                          addErrorBar=T, ebWidth = .01
                          ,xlabname="",ylabname="% respondents"){
  mydata <- as.data.frame(mydata)
  mydata$yVar.pct <- makePercent(mydata[,yVar])
  
  p <- ggplot(mydata,aes_string(x=xVar,y=yVar,ymin=LL,ymax=UL,group=groupVar,fill=fill))+
    geom_bar(stat='identity',position = position_dodge(.5),width=.5,color='grey50')+
    geom_point(position = position_dodge(.5),
               color='grey10')+
    
    ylab(ylabname)+
    xlab(xlabname)+
    theme_bw()

  if(addErrorBar){p <- p+geom_errorbar(position = position_dodge(.5),width=ebWidth,color='grey10')
  }
  
  if(addDataLabs){p <- p+geom_text(aes_string(y='yloc',label='yVar.pct'),position = position_dodge(.5))
  }

  if(asPct){p <- p+scale_y_continuous(ylabname,labels = percent)}
  
  if(coordFlip){p <- p+coord_flip()+
    theme(text=element_text(size=textSize),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())
  
  }else{p <- p+theme(text=element_text(size=textSize),
                     panel.grid.major.x=element_blank(),
                     panel.grid.minor.x=element_blank())}
  
  
  return(p)
}

