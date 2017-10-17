### Convert Kano response strings to numeric responses, user must provide single character vector and question type (either functional or dysfunctional)
kano.convert2numeric <- function(x,questionType = NULL,
                         expectString = 'I expect it',
                         likeString = 'I like it',
                         neutralString = "I'm neutral",
                         dealwithString = "I'll deal with it",
                         dislikeString = "I dislike it"){
 
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



### Compute classification for feature based on functional and dysfucntional,  
kano.getClass <- function(x,order = 1,
                                 expectString = 'I expect it',
                                 likeString = 'I like it',
                                 neutralString = "I'm neutral",
                                 dealwithString = "I'll deal with it",
                                 dislikeString = "I dislike it"){

#Function accepts two column data.frame

if(ncol(x)!=2){print('incorrect number of cols')}
     
kanoKey = data.frame(
  like = c("Q","A","A","A","P"),
  expect = c("R","Q","I","I","M"),
  neutral = c("R","I","I","I","M"),
  deal = c("R","I","I","Q","M"),
  dislike = c("R","R","R","R","Q"))
row.names(kanoKey) <- c('like','expect','neutral','deal','dislike')

expectString = 'I expect it'
likeString = 'I like it'
neutralString = "I'm neutral"
dealwithString = "I'll deal with it"
dislikeString = "I dislike it"

x[x==expectString] = 'expect'
x[x==likeString] = 'like'
x[x==neutralString] = 'neutral'
x[x==dealwithString] = 'deal'
x[x==dislikeString] = 'dislike'

if(order == 1){
kanoClass <- sapply(1:nrow(x),function(i){
  as.character(kanoKey[as.character(x[i,1]),
          as.character(x[i,2])])})}

if(order == 2){
  kanoClass <- sapply(1:nrow(x),function(i){
    as.character(kanoKey[as.character(x[i,2]),
                         as.character(x[i,1])])})}

return(kanoClass)
}

