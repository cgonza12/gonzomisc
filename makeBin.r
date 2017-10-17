makeBin = function(x,splitNum = 0){
  ifelse(x>splitNum,'1+',0)
  
}

makeBin.num = function(x,splitNum = 0){
  ifelse(x>splitNum,1,0)
  
}

makeBin.multi = function(mydata){apply(mydata,2,function(x){ifelse(x=="",0,1)})}
