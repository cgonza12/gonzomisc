writeLoadings <- function(fafit, cutoff=.3,filename = 'loadingout.csv'){
  
  faLoadings = data.frame(round(loadings(fafit,sort=T)[1:length(fafit$values), 1:fafit$factors],2))
  
  faLoadings[abs(faLoadings)<.3] <- ''
  
  write.csv(faLoadings,filename)
  
}