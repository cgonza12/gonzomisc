
kano.convert2numeric <- function(x,questionType = NULL,funcLove=T,na.char = "notDone",
                                 responseStrings.func = list(),
                                 responseStrings.dys = list()){
  ### Convert Kano response strings to numeric responses, user must provide single character vector and question type (either functional or dysfunctional)
  
  if(questionType == 1){#1 = functional question
   
    func = x
    func[func==responseStrings.func$expectString] = 'expect'
    func[func==responseStrings.func$likeString] = 'like'
    if(funcLove){func[func==responseStrings.func$loveString] = 'love'}
    func[func==responseStrings.func$neutralString] = 'neutral'
    func[func==responseStrings.func$dealwithString] = 'deal'
    func[func==responseStrings.func$dislikeString] = 'dislike'
    
  func[func=='love'] = 5
  func[func=='like'] = 4
  func[func=='expect'] = 2
  func[func=="neutral"] = 0
  func[func=="deal"]  = -1
  func[func=="dislike"] = -2
  func[func==na.char] = NA
  tmp = func
  }
  if(questionType == 2){#2 = dysfunctional question
    
    dys = x
    dys[dys==responseStrings.dys$expectString] = 'expect'
    dys[dys==responseStrings.dys$likeString] = 'like'
    dys[dys==responseStrings.dys$neutralString] = 'neutral'
    dys[dys==responseStrings.dys$dealwithString] = 'deal'
    dys[dys==responseStrings.dys$dislikeString] = 'dislike'
    
    dys[dys=='like'] = -2
    dys[dys=='expect'] = -1
    dys[dys=="neutral"] = 0
    dys[dys=="deal"] = 2
    dys[dys=="dislike"] = 4
    dys[dys==na.char] = NA
    tmp = dys
    }
return(as.numeric(tmp))
}



kano.getClass <- function(x,funcLove = T,
                               responseStrings.func = list(),
                              responseStrings.dys = list()){
### Compute classification for feature based on functional and dysfucntional questions, order 1 assumes first col is functional, order 2 assumes dysfuctional is first
  
#Function accepts two column data.frame

if(ncol(x)!=2){print('incorrect number of cols')}
     
kanoKey = data.frame(
  like = c("Q","Q","R","R","R","R"),
  expect = c('A','A','Q','I','I','R'),
  neutral = c("A","A","I","I","I","R"),
  deal = c("A","A","I","I","Q","R"),
  dislike = c("P","P","M","M","M","Q"))
row.names(kanoKey) <- c('love','like','expect','neutral','deal','dislike')

func = x[1]
func[func==responseStrings.func$expectString,1] = 'expect'
func[func==responseStrings.func$likeString,1] = 'like'
if(funcLove){func[func==responseStrings.func$loveString,1] = 'love'}
func[func==responseStrings.func$neutralString,1] = 'neutral'
func[func==responseStrings.func$dealwithString,1] = 'deal'
func[func==responseStrings.func$dislikeString,1] = 'dislike'


dys = x[2]
dys[dys==responseStrings.dys$expectString,1] = 'expect'
dys[dys==responseStrings.dys$likeString,1] = 'like'
dys[dys==responseStrings.dys$neutralString,1] = 'neutral'
dys[dys==responseStrings.dys$dealwithString,1] = 'deal'
dys[dys==responseStrings.dys$dislikeString,1] = 'dislike'

tmp = data.frame(func,dys)

kanoClass <- sapply(1:nrow(tmp),function(i){
 
  checkKano = as.character(kanoKey[as.character(tmp[i,1]),
                       as.character(tmp[i,2])])
   if(length(checkKano)==0){NA}else{checkKano}
  
  })
return(kanoClass)
}


kano.getClassNumeric <- function(func,dys){ifelse((func <= 2 & dys <= -1) | (func <= -1 & dys <= 2), "R",
                                                  ifelse((func <= 2 & func >= -1 & dys <= 2& dys >= -1), "I",
                                                         ifelse((func >= 2 & dys >= -1 & dys <= 2), "A",
                                                                ifelse((func <= 2 & dys <= 4 & dys >= 2), "M",
                                                                       ifelse((func >= 2 & dys >= 2), "P","Q")))))}
