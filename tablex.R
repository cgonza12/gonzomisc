tablex <- function(tableRow = NULL, tableCol = NULL, sorted=T, props = 1, digits = 2, writeIt=F,testIt=F){
  
  if(!is.null(tableCol)){
  M1 <- table(tableRow,tableCol)
  M2 <- t(as.matrix(M1))
  J <- rbind(round(prop.table(M2,props),digits),colSums(M2))
  M3 <- rbind(M2,colSums(M2))
  result <- cbind(J,rowSums(M3))
  colnames(result)[length(colnames(result))] <- 'rowCount'
  rownames(result)[length(rownames(result))] <- 'colCount'
  result <- as.data.frame(result)
  }else{
    M1 <- table(tableRow)
    if(sorted){M1 <- sort(table(tableRow),decreasing = T)}
    M2 <- t(as.matrix(M1))
    J <- rbind(round(prop.table(M2,props),digits),colSums(M2))
    M3 <- rbind(M2,colSums(M2))
    result <- cbind(J,rowSums(M3))  
    colnames(result)[length(colnames(result))] <- 'rowCount'
    result <- as.data.frame(result)
    
  }
  
  
  
  if(writeIt){write.csv(result)}
  if(testIt){chisq.test(M1)}
  
  return(result)
}
