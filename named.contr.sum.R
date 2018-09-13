named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else {x <- levels(factor(x))}
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    paste(names(x[x>0]), 'GrandMean', sep="-")
  )
  x
}