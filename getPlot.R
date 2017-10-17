require(ggplot2)
getPlot.simple <-  function(mydata,yloc = .05,xlabname="",ylabname="% respondents"){
  ggplot(mydata,aes(x=varname,y=estimate,ymin=LL,ymax=UL))+
    geom_bar(stat='identity',position = position_dodge(.5),width=.5,
             fill='lightblue',color='grey50')+
    geom_point(position = position_dodge(.5),
               color='grey10')+
    geom_errorbar(position = position_dodge(.5),width=.2,color='grey10')+
    geom_text(aes(y=yloc,label=makePercent(estimate)),position = position_dodge(.5))+
    scale_y_continuous(ylabname,labels = percent)+
    xlab(xlabname)+
    coord_flip()+
    guides(fill= F)+
    theme_bw()+
    theme(text=element_text(size=16),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank()
    )
}

getPlot.group <- function(mydata,yloc = .05,xlabname="",ylabname="% respondents"){
  ggplot(mydata,aes(x=aggregateVar,y=estimate,ymin=LL,ymax=UL,group=groupVar,fill=groupVar))+
    geom_bar(stat='identity',position = position_dodge(.5),width=.5,color='grey50')+
    geom_point(position = position_dodge(.5),
               color='grey10')+
    geom_errorbar(position = position_dodge(.5),width=.2,color='grey10')+
    geom_text(aes(y=yloc,label=makePercent(estimate)),position = position_dodge(.5))+
    scale_y_continuous(ylabname,labels = percent)+
    xlab(xlabname)+
    coord_flip()+
    theme_bw()+
    theme(text=element_text(size=16),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank()
    )
}

