
getBinomCI <- function(f,reorder=T,forVert = F, dropLevels = T,digits= 2){
  if(!is.factor(f)){print('convert to a factor')}
  if(dropLevels){f = droplevels(f)}
  tmp = data.table(model.matrix(~f-1))
  setnames(tmp,levels(f))
  
  tmp = data.table(t(apply(tmp,2,function(i){
    c(
    round(prop.test(sum(i),length(i),correct = F)$conf.int,digits),
    round(prop.test(sum(i),length(i),correct = F)$estimate[[1]],digits),
    sum(i)
    )
  })))
  
  tmp$var = as.factor(levels(f))
  setnames(tmp,c('LL','UL','estimate','N','varname'))
  if(reorder){tmp$varname = reorder(tmp$varname,tmp$estimate*-1) } 
  if(forVert){tmp$varname = reorder(tmp$varname,tmp$estimate) } 
  return(tmp)
}

