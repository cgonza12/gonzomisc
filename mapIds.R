mapIds <- function(newIds,corpus = idList){
  
  
  result =  unlist(lapply(newIds,function(x){names(corpus)[grep(x,corpus,ignore.case = T)]}))
  if(length(result)==0){return(NA)}else{
    return(result)
  }
    
    
}
