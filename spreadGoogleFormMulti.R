spreadGoogleFormMulti <- function(x,prefix = '',other_string='Other',fillVal=1,cutOff=NULL){

# take a single column of multi select data, then split it into named columns of the options with 1's or 0's to indicat selection
  
# remove any commas that are not delimeters

tmp = gsub(","," ", x)
  
# remove trailing spaces and split each response into a list of items
tmp = trim(strsplit(x,split = ','))

#combine all responses
tmp.l = do.call(c,tmp)
# get the names of items by taking names from a table
tmp.names = names(sort(table(tmp.l),decreasing = T))

#remove any chars that won't vibe with Rs data frame names
tmp.names = gsub("[^A-Za-z0-9 ]","", tmp.names)
tmp.names = gsub(" ","_", tmp.names)

#do the same to the items so you can do string matching
tmp <- lapply(tmp,function(x){
  cleanx = gsub("[^A-Za-z0-9 ]","", x)
  cleanx = gsub(" ","_", cleanx)
})

#fill up a list of length n 
targetList <- list()
targetList <- lapply(1:length(tmp.names),FUN = function(n){
  # cut to the first item name
  item <- tmp.names[n]
  
  # search through each response if item is in that reponse add a one to that column if its in there
  targetList[[n]] <- sapply(tmp,function(j){
    jj = ifelse(item %in% j,fillVal,0)
    return(jj)})
  
  
})
# stick eveything together then add the right names
tmp.df = as.data.frame(do.call(cbind,targetList))
names(tmp.df) <- paste0(prefix,tmp.names)

if(!is.null(cutOff)){
  if(cutOff>0){
  tmp.df$other <- rowSums(tmp.df[colSums(tmp.df)<cutOff])
  tmp.df$other <- ifelse(tmp.df$other>0,fillVal,0)
  
  tmp.names <- c(names(tmp.df)[-ncol(tmp.df)],paste0(prefix,other_string))
  names(tmp.df) <- tmp.names
  
  tmp.df <- tmp.df[colSums(tmp.df)>cutOff]
  }else{
    tmp.df$other <- rowSums(tmp.df[colSums(tmp.df)>cutOff])
    tmp.df$other <- ifelse(tmp.df$other>0,fillVal,0)
    
    tmp.names <- c(names(tmp.df)[-ncol(tmp.df)],paste0(prefix,other_string))
    names(tmp.df) <- tmp.names
    
    tmp.df <- tmp.df[colSums(tmp.df)<cutOff]
    
  }
  
}



return(tmp.df)}