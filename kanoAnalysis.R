kano.convert2numeric <- function(x,questionType = NULL,
                         expectString = 'I expect it',
                         likeString = 'I like it',
                         neutralString = "I'm neutral",
                         dealwithString = "I'll deal with it",
                         dislikeString = "I dislike it"){
  ### Convert Kano response strings to numeric responses, user must provide single character vector and question type (either functional or dysfunctional)
  
  x[x==expectString] = 'expect'
  x[x==likeString] = 'like'
  x[x==neutralString] = 'neutral'
  x[x==dealwithString] = 'deal'
  x[x==dislikeString] = 'dislike'
  
  if(questionType == 1){#1 = functional question
  x[x=='like'] = 4
  x[x=='expect'] = 2
  x[x=="neutral"] = 0
  x[x=="deal"]  =1
  x[x=="dislike"] = -2
  }
  if(questionType == 2){#2 = dysfunctional question
    x[x=='like'] = -2
    x[x=='expect'] = -1
    x[x=="neutral"] = 0
    x[x=="deal"] = 2
    x[x=="dislike"] = 4
    }
return(as.numeric(x))
}



kano.getClass <- function(x,order = 1,
                                 expectString = 'I expect it',
                                 likeString = 'I like it',
                                 neutralString = "I'm neutral",
                                 dealwithString = "I'll deal with it",
                                 dislikeString = "I dislike it"){
### Compute classification for feature based on functional and dysfucntional questions, order 1 assumes first col is functional, order 2 assumes dysfuctional is first
  
#Function accepts two column data.frame

if(ncol(x)!=2){print('incorrect number of cols')}
     
kanoKey = data.frame(
  like = c("Q","A","A","A","P"),
  expect = c("R","Q","I","I","M"),
  neutral = c("R","I","I","I","M"),
  deal = c("R","I","I","Q","M"),
  dislike = c("R","R","R","R","Q"))
row.names(kanoKey) <- c('like','expect','neutral','deal','dislike')

x[x==expectString] = 'expect'
x[x==likeString] = 'like'
x[x==neutralString] = 'neutral'
x[x==dealwithString] = 'deal'
x[x==dislikeString] = 'dislike'

if(order == 1){
kanoClass <- sapply(1:nrow(x),function(i){
  as.character(kanoKey[as.character(x[i,2]),
          as.character(x[i,1])])})}

if(order == 2){
  kanoClass <- sapply(1:nrow(x),function(i){
    as.character(kanoKey[as.character(x[i,1]),
                         as.character(x[i,2])])})}

return(kanoClass)
}

kano.getClassNumeric <- function(func,dys){ifelse((func <= 2 & dys <= -1) | (func <= -1 & dys <= 2), "R",
                                                  ifelse((func <= 2 & func >= -1 & dys <= 2& dys >= -1), "I",
                                                         ifelse((func >= 2 & dys >= -1 & dys <= 2), "A",
                                                                ifelse((func <= 2 & dys <= 4 & dys >= 2), "M",
                                                                       ifelse((func >= 2 & dys >= 2), "P","Q")))))}
