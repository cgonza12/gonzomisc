getConfInt <- function(x,round=2){
  tmp = t.test(x)
  abs(round(tmp$conf.int-tmp$estimate,round)[1])}

