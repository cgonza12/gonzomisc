cleanNames <- function(x,char='_',replacement=' ',prefix=T){
  x <- as.factor(x)
  select1or2 <- function(i){ifelse(length(i)==1,i[1],i[2])}  
  
  if(prefix){
    gsub(char,replacement,
         sapply(strsplit(levels(x),'[.]'),select1or2))  
  }else{
    
    gsub(char,replacement,levels(x))
  }
  }

