named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    paste(names(x[x>0]), 'GrandMean', sep="-")
  )
  x
}