
getBinomCI <- function(f,reorder=T,forVert = F, dropLevels = T,digits= 2,conf.level = .95){
  
  if(!is.factor(f)){f= as.factor(f)}
  if(dropLevels){f = droplevels(f)}
  
  if(length(levels(f))<2){print('only one level');return(NULL)}else{
  tmp = data.table(model.matrix(~f-1))
  setnames(tmp,levels(f))
  
  tmp = data.table(t(apply(tmp,2,function(i){
    c(
    round(prop.test(sum(i),length(i),correct = F,conf.level = conf.level)$conf.int,digits),
    round(prop.test(sum(i),length(i),correct = F,conf.level = conf.level)$estimate[[1]],digits),
    sum(i)
    )
  })))
  
  tmp$var = as.factor(levels(f))
  setnames(tmp,c('LL','UL','estimate','N','varname'))
  if(reorder){tmp$varname = reorder(tmp$varname,tmp$estimate*-1) } 
  if(forVert){tmp$varname = reorder(tmp$varname,tmp$estimate) } 
  return(tmp)
  }
}

getBinomCI.group <- function(data,aggregateVar,groupVar,reorder=T,forVert = F, dropLevels = T,digits= 2,conf.level = .95){
  
data.l = split(data,f = as.factor(data[,groupVar]))
tmp = bind_rows(lapply(data.l,FUN = function(x){getBinomCI(x[,aggregateVar],
                                                           reorder,forVert, dropLevels,digits
                                                           )}),.id = groupVar)
return(tmp)

}


getNormCI <- function(data,reorder=T,forVert = F,digits= 2,conf.level = .95){
  data = as.data.frame(data)
  if(!any(apply(data,2,is.numeric))){print('non-numerics in array')}
  nums <- sapply(data, is.numeric)
  tmp = data.table(t(apply(data[nums],2,function(i){
    c(
      round(t.test(i,conf.level = conf.level)$conf.int,digits),
      round(t.test(i,conf.level = conf.level)$estimate[[1]],digits),
      length(i)
    )
  })))
  
  tmp$var = as.factor(names(data[nums]))
  setnames(tmp,c('LL','UL','estimate','N','varname'))
  if(reorder){tmp$varname = reorder(tmp$varname,tmp$estimate*-1) } 
  if(forVert){tmp$varname = reorder(tmp$varname,tmp$estimate) } 
  return(tmp)
}


getNormCI.group <- function(data,aggregateVar,groupVar,reorder=T,forVert = F, dropLevels = T,digits= 2,conf.level = .95){
  
  tmp = bind_rows(lapply(split(data,data[groupVar]),function(x){getNormCI(as.data.frame(x[aggregateVar])
                                                                    )}),.id=groupVar)
  
  
  return(tmp)
  
}




getBinomCI.group <- function(data,aggregateVar,groupVar,reorder=T,forVert = F, dropLevels = T,digits= 2,conf.level = .95){
  data = as.data.frame(data)
  if(!any(apply(data,2,is.numeric))){print('non-numerics in array')}
  data <- sapply(data, is.numeric)
  
  data.l = split(data,f = as.factor(data[,groupVar]))
  tmp = bind_rows(lapply(data.l,FUN = function(x){getBinomCI(x[,aggregateVar],
                                                             reorder,forVert, dropLevels,digits
  )}),.id = groupVar)
  setnames(tmp,c('groupVar','LL','UL','estimate','N','aggregateVar'))
  return(tmp)
  
}

getBinomCI.multi <- function(mydata,reorder=T,forVert = F, dropLevels = T,digits= 2,conf.level = .95){
  tmp = data.table(t(apply(mydata,2,function(i){
    if(sum(i)==0){print('no data, skipping');return(c(0,0,0,0))}else{
    c(
      round(prop.test(sum(i),length(i),correct = F,conf.level = conf.level)$conf.int,digits),
      round(prop.test(sum(i),length(i),correct = F,conf.level = conf.level)$estimate[[1]],digits),
      sum(i)
    )
    }
  })))
  
  tmp$var = as.factor(names(mydata))
  setnames(tmp,c('LL','UL','estimate','N','varname'))
  if(reorder){tmp$varname = reorder(tmp$varname,tmp$estimate*-1) } 
  if(forVert){tmp$varname = reorder(tmp$varname,tmp$estimate) } 
  return(tmp)
  
}

getBinomCI.multi.group <- function(data,groupVar,reorder=T,forVert = F, dropLevels = T,digits= 2,conf.level = .95){
  
  data.l = split(data,f = as.factor(data[,groupVar]))
  tmp = bind_rows(lapply(data.l,FUN = function(x){getBinomCI.multi(x[names(x)!=groupVar],
                                                             reorder,forVert, dropLevels,digits
  )}),.id = groupVar)
  return(tmp)
  
}