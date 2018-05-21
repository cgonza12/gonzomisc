getOverlaps <- function(data,idToSpread,repdVar,LL='LL',UL='UL',mergeWithData=T){

  #only works when there's 2 vars

  
  keep <- c(LL,UL,repdVar,idToSpread)
  tmp <- data[,keep]
  
  names(tmp) <- c('LL','UL','repdVar','idToSpread')
  tmp <- data.frame(tmp %>% group_by(idToSpread,repdVar) %>% 
                     mutate(id=1:n()) %>%
                     melt(id=c("idToSpread", "id", "repdVar")) %>%
                     dcast(... ~ idToSpread + variable, value.var="value"))
  
  tmp$CI.overlap = ifelse((tmp[,4] - tmp[,5])*(tmp[,3] - tmp[,6])>0,'noOverlap','overLap')
  tmp
  if(mergeWithData){
    merged.tmp <- merge(data,tmp[c('CI.overlap','repdVar')],by.x='varname',by.y = 'repdVar')
    return(merged.tmp)  
  }
  return(tmp)
  
}

