#clean zscore and log transfrom RT from GCS

cleanScale.gcs <- function(x){scale(log(as.numeric(gsub(pattern = 'ms',replacement = '',x = x))))}