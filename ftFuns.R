# Extracts single click or click time

ft.extract <- function(
  #accepts flowtrics string
  
  clickString = NULL,
  clickSplitChar = ';', #level 1 split char
  elementSplitChar = ':', #level 2 split char
  clickNum = 2,
  dimension = 2){
  #1 for time, 2 for element
  require(dplyr)
  sapply(clickString,function(x){strsplit(as.character(x),split=clickSplitChar)}) %>%
    sapply(.,function(x){strsplit(as.character(x),split=elementSplitChar)}) %>%
    sapply(.,function(x){x[clickNum][[1]][[dimension]]}) %>% 
    as.character() %>% 
    gsub('NULL',NA,.) 
  
}


#Extracts n # of clicks returns a data frame
ft.extract_all <- function(
  
  #accepts flowtrics string
  clickString = NULL,
  clickSplitChar = ';', #level 1 split char
  elementSplitChar = ':', #level 2 split char
  nClicks = 1,
  dimension = 2, #1 for time, 2 for element
  clickPrefix = '' 
  
){
  require(dplyr)
  tmp = sapply(1:nClicks,FUN = function(i){
    
    ft.extract(clickString = old.getLink$t2_clickString,clickNum = i)
    
  }) %>% data.frame(stringsAsFactors = F) %>% setnames(paste0(clickPrefix,paste0('click_',1:nClicks))) %>% 
    map_if(is.factor, as.character) %>% 
    as_data_frame 
    
    
  return(tmp)
}
