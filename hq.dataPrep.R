filename = '~/Google Drive/Downloads/Home Recommendation Quality Eval - Lite_March 13, 2018_12.04.csv'
hq.dataPrep <- function(filename){

raw =  read.csv(filename,
                stringsAsFactors = F,na.strings=c("","NA"))[-c(1,2),]
raw = raw[!is.na(raw$intro.email),]
raw = raw[-c(5:6),]
raw = raw[as.numeric(raw$Progress)>25,]
raw = raw[raw$DistributionChannel=="anonymous",]

median(as.numeric(raw$Duration..in.seconds.))/60
quantile(as.numeric(raw$Duration..in.seconds.),seq(0,1,.1))/60
#raw$intro.email = substr(raw$intro.email,1,.1)
raw$intro.email2 = raw$intro.email
raw$intro.email = raw$ResponseId
names(raw)



qualNames = names(raw)[grep('.qual',names(raw))]
qualItems = raw[c('intro.email',qualNames)]

qualItems.l = gather(qualItems,'itemComb','quality',-1) %>%
  separate(itemComb,c('sec','type','item')) %>%
  mutate(type=NULL,
         quality = NULL,
         sec = gsub('X','',x = sec),
         itemQual = suppressWarnings(as.numeric(as.character(fct_recode(quality, '2' = 'Excellent','1' = 'Good', '0' = 'Average', 
                                                       '-1' = 'Poor','-2' = 'Terrible')))
  ))

secQualNames = names(raw)[grep('.overallQual',names(raw))]
secQual = raw[c('intro.email',secQualNames)]

secQual.l = gather(secQual,'itemComb','quality',-1) %>%
  separate(itemComb,c('sec','type')) %>%
  mutate(type=NULL,
         quality = NULL,
         sec = gsub('X','',x = sec),
         secQual = suppressWarnings(as.numeric(as.character(fct_recode(quality, '2' = 'Excellent','1' = 'Good', '0' = 'Average', 
                                                      '-1' = 'Poor','-2' = 'Terrible')))
  ))

combdat = merge(qualItems.l,secQual.l,by=c('intro.email','sec'))


# likNames = names(raw)[grep('lik',names(raw))]
# likItems = raw[c('intro.email',likNames)]
# 
# likItems.l = gather(likItems,'itemComb','lik',-1) %>%
#   separate(itemComb,c('sec','type','item')) %>%
#   mutate(type=NULL,
#          quality = NULL,
#          sec = gsub('X','',x = sec),
#          lik = as.numeric(as.character(fct_recode(lik, '2' = 'Extremely likely','1' = 'Somewhat likely', 
#                                                         '0' = 'Neither likely nor unlikely', 
#                                                         '-1' = 'Somewhat unlikely','-2' = 'Extremely unlikely')))
#          
#   )
# 
# combdat = merge(qualDat,likItems.l,by=c('intro.email','sec','item'))

famNames = names(raw)[grep('familiar',names(raw))]
famItems = raw[c('intro.email',famNames)]

famItems.l = gather(famItems,'itemComb','familiar',-1) %>%
  separate(itemComb,c('sec','type','item')) %>%
  mutate(type=NULL,
         quality = NULL,
         sec = gsub('X','',x = sec))

combdat = merge(combdat,famItems.l,by=c('intro.email','sec','item'))


# matchNames = names(raw)[grep('match',names(raw))]
# matchItems = raw[c('intro.email',matchNames)]
# 
# matchItems.l = gather(matchItems,'itemComb','match',-1) %>%
#   separate(itemComb,c('sec','type','item','type2')) %>%
#   mutate(type=NULL,
#          sec = gsub('X','',x = sec),
#          match = ifelse(!is.na(match),1,0),
#          type2 = fct_recode(type2, itemMatch.mood = '1',
#                             itemMatch.activity = '2', 
#                             itemMatch.taste = '3', 
#                             itemMatch.na = '4'),
#          id=1:n()) %>%
#   spread(type2,match, fill = NA) %>%
#   mutate(id=NULL) 
# 
# matchItems.l[is.na(matchItems.l)]  <- 0
# 
# matchItems.l <- group_by(matchItems.l,intro.email,sec, item) %>%
#   summarise(itemMatch.mood = sum(itemMatch.mood),
#             itemMatch.activity = sum(itemMatch.activity),
#             itemMatch.taste = sum(itemMatch.taste)
#             )
# 
# combdat = merge(combdat,matchItems.l,by=c('intro.email','sec','item'))

typeNames = names(raw)[grep('musicItems',names(raw))]
typeItems = raw[c('intro.email',typeNames)]

typeItems.l = suppressWarnings(gather(typeItems,'itemComb','val',-1) %>%
  separate(itemComb,c('sec','type','item','type2')) %>%
  mutate(type=NULL,
         type2 = ifelse(is.na(type2),'itemType','itemName'),
         sec = gsub('X','',x = sec)) %>%
  spread(type2,val, fill = NA) %>%
  mutate(id=NULL))

combdat = merge(combdat,typeItems.l,by=c('intro.email','sec','item'))

secNames = names(raw)[grep('sectionTitles',names(raw))]
secItems = raw[c('intro.email',secNames)]

secItems.l = gather(secItems,'itemComb','sectTitle',-1) %>%
  separate(itemComb,c('block','type','sec')) %>%
  mutate(type=NULL,
         block=NULL) 

combdat = merge(combdat,secItems.l,by=c('intro.email','sec'))

combdat = combdat[order(combdat$intro.email,combdat$sec,combdat$item),]

lowNames = names(raw)[grep('whyLow',names(raw))]
lowItems = raw[c('intro.email',lowNames)]

lowItems.l = suppressWarnings(gather(lowItems,'itemComb','whyLow',-1) %>%
  separate(itemComb,c('sec','type','item','type2')) %>%
  mutate(type=NULL,
         sec = gsub('X','',x = sec),
         whyLow = ifelse(!is.na(whyLow),1,0),
         type2 = fct_recode(type2, whyLow.tired = '1',
                            whyLow.tooMany = '2', 
                            whyLow.dontLike = '3', 
                            whyLow.dontThinkLike = '4',
                            whyLow.dontLikeVids = '5',
                            whyLow.notConf = '6',
                            whyLow.notNow = '7'
         ),
         id=1:n()) %>%
  spread(type2,whyLow, fill = NA) %>%
  mutate(id=NULL) %>% 
  tidyr::replace_na(list(whyLow.tired= 0,whyLow.tooMany=0,whyLow.dontLike=0,
                         whyLow.dontThinkLike=0,whyLow.dontLikeVids= 0,whyLow.notConf=0,whyLow.notNow=0
  )) %>%
  group_by(intro.email,sec, item) %>%
  summarise(whyLow.tired = sum(whyLow.tired),
            whyLow.tooMany = sum(whyLow.tooMany),
            whyLow.dontLike = sum(whyLow.dontLike),
            whyLow.dontThinkLike = sum(whyLow.dontThinkLike),
            whyLow.dontLikeVids = sum(whyLow.dontLikeVids),
            whyLow.notConf = sum(whyLow.notConf),
            whyLow.notNow = sum(whyLow.notNow)
  ))

combdat = merge(combdat,lowItems.l,by=c('intro.email','sec','item'))

combdat = combdat[order(combdat$intro.email,combdat$sec,combdat$item),]

highNames = names(raw)[grep('whyHigh',names(raw))]
highItems = raw[c('intro.email',highNames)]

highItems.l = gather(highItems,'itemComb','whyHigh',-1) %>%
  separate(itemComb,c('sec','type','item','type2')) %>%
  mutate(type=NULL,
         sec = gsub('X','',x = sec),
         whyHigh = ifelse(!is.na(whyHigh),1,0),
         type2 = fct_recode(type2, whyHigh.notHeardWhile = '1',
                            whyHigh.likeIt = '2', 
                            whyHigh.thinkLikeit = '3', 
                            whyHigh.likeVideos = '4',
                            whyHigh.heardAlot = '5',
                            whyHigh.notConf = '6'),
         id=1:n()) %>%
  spread(type2,whyHigh, fill = NA) %>%
  mutate(id=NULL) %>% 
  tidyr::replace_na(list(whyHigh.notHeardWhile= 0,whyHigh.likeIt=0,whyHigh.thinkLikeit=0,
                         whyHigh.likeVideos=0,whyHigh.heardAlot= 0,whyHigh.notConf=0
  )) %>%
  group_by(intro.email,sec, item) %>%
  summarise(whyHigh.notHeardWhile = sum(whyHigh.notHeardWhile),
            whyHigh.likeIt = sum(whyHigh.likeIt),
            whyHigh.thinkLikeit = sum(whyHigh.thinkLikeit),
            whyHigh.likeVideos = sum(whyHigh.likeVideos),
            whyHigh.heardAlot = sum(whyHigh.heardAlot),
            whyHigh.notConf = sum(whyHigh.notConf))

combdat = merge(combdat,highItems.l,by=c('intro.email','sec','item'))

combdat = combdat[order(combdat$intro.email,combdat$sec,combdat$item),]
secMatchNames = names(raw)[grep('titleMatch',names(raw))]
secMatchItems = raw[c('intro.email',secMatchNames)]


secMatchItems.l = gather(secMatchItems,'itemComb','match',-1) %>%
  separate(itemComb,c('sec','type','type2')) %>%
  mutate(type=NULL,
         sec = gsub('X','',x = sec),
         type2 = fct_recode(type2, titleMatch.mood = '1',
                            titleMatch.activity = '2', 
                            titleMatch.taste = '3'),
         match = suppressWarnings(as.numeric(as.character(fct_recode(match, 
                                                    '2' = 'Yes',
                                                    '1' = 'Somewhat',
                                                    '0'='No',
                                                    'NA'='Not sure/Not applicable')))),
         id=1:n()) %>%
  spread(type2,match, fill = NA) %>%
  mutate(id=NULL) %>% 
  group_by(intro.email,sec) 
uniques.nonna <- function(x){na.omit(unique(x))[1]}


tmp = as.data.frame(t(bind_rows(lapply(split(secMatchItems.l,f = list(secMatchItems.l$intro.email,
                                                                      secMatchItems.l$sec)),
                                       FUN = function(x){
                                         apply(x[3:5],2,uniques.nonna)}),.id='id.sec')))
tmp$id.sec = as.factor(rownames(tmp))
tmp = tmp[-1,]
tmp[1:3] <- apply(tmp[1:3],2,function(x){as.numeric(trim(as.character(x)))})

tmp = separate(tmp,id.sec,c('intro.email','sec'),sep = '[.]')
rownames(tmp) = NULL
names(tmp) <- c('titleMatch.mood','titleMatch.activity','titleMatch.taste','intro.email','sec')
secMatchItems.l = tmp

combdat = merge(combdat,secMatchItems.l,by=c('intro.email','sec'))

combdat = combdat[order(combdat$intro.email,combdat$sec,combdat$item),]

userDat = select(raw, c(18,309:313,315:316)) 

userDat[2:5] <- suppressWarnings(apply(userDat[2:5],2,function(x){
  as.numeric(fct_relevel(x,
                         'Strongly disagree','Somewhat disagree','Neither agree nor disagree',
                         'Somewhat agree' , 'Strongly agree'))
}))


combdat = merge(combdat,userDat,by=c('intro.email'))

combdat = combdat[order(combdat$intro.email,combdat$sec,combdat$item),]


initThoughtsNames = names(raw)[grep('initialThoughts',names(raw))]
initThoughtsItems = raw[c('intro.email',initThoughtsNames)]

initThoughtsItems.l = gather(initThoughtsItems,'itemComb','initialThoughts',-1) %>%
  separate(itemComb,c('sec','type')) %>%
  mutate(type=NULL,
         sec = gsub('X','',x = sec))

openEndedNames = names(raw)[grep('openEnded',names(raw))]
openEndedItems = raw[c('intro.email',openEndedNames)]

openEndedItems.l = gather(openEndedItems,'itemComb','closingThoughts',-1) %>%
  separate(itemComb,c('sec','type')) %>%
  mutate(type=NULL,
         sec = gsub('X','',x = sec))

textItems.l <- merge(openEndedItems.l,initThoughtsItems.l,by=c('intro.email','sec'))

combdat = merge(combdat,textItems.l,by=c('intro.email','sec'))

combdat = combdat[order(combdat$intro.email,combdat$sec,combdat$item),]

rm(list=setdiff(ls(), "combdat"))


combdat.secSum <- combdat %>% group_by(intro.email,sec) %>%
  summarise(sectTitle = sectTitle[1],
            items = paste(itemName[1],itemName[2],itemName[2],sep=';'),
            avgitemQual = mean(itemQual,na.rm=T),
            secQual = mean(secQual,na.rm=T),
            netitemQual = sum(itemQual,na.rm=T),
            netSecQual = sum(secQual,na.rm=T),
            nItems = length(which(!is.na(itemQual))),
            nPlaylist = sum(itemType=='Playlist'),
            nVids = sum(itemType=='Video'),
            nAlbums = sum(itemType=='Album'),
            nSingle = sum(itemType=='Single'),
            nRadio = sum(itemType=='Radio'),
            nArtistShuffle = sum(itemType=='Artist Shuffle'),
            
            titleMatch.mood = as.numeric(as.character(titleMatch.mood[1])),
            titleMatch.activity= as.numeric(as.character(titleMatch.activity[1])),
            titleMatch.taste = as.numeric(as.character(titleMatch.taste[1])),
            
            final.taste=final.taste[1],
            final.context=final.context[1],
            final.discover=final.discover[1],
            final.valueProp=final.valueProp[1],
            closingThoughts = closingThoughts[1],
            initialThoughts = initialThoughts[1]
  )

source('~/Google Drive/R/genericHeader.R')


#### Sect RegEx

combdat.secSum$secCat.yourFavs <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                    function(x){grepl('*your fav*',x,ignore.case = T,fixed = F)}))

#Video
#*video* *live perf*

toMatch <- c('vid*','live*','concert')
combdat.secSum$secCat.vid <- as.numeric(sapply(combdat.secSum$sectTitle,
                                               function(x){grepl(paste(toMatch,collapse="|"),
                                                                 x,ignore.case = T,fixed = F)}))



#new releases

combdat.secSum$secCat.newRelease <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                      function(x){grepl('*new releas*',
                                                                        x,ignore.case = T,fixed = F)}))

#Listen again
combdat.secSum$secCat.listenAgain <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                       function(x){grepl('*listen again*',
                                                                         x,ignore.case = T,fixed = F)}))

#Similar to
combdat.secSum$secCat.similarTo <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                     function(x){grepl('*Similar to*',
                                                                       x,ignore.case = T,fixed = F)}))


#More from
combdat.secSum$secCat.moreFrom <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                    function(x){grepl('*more from*',
                                                                      x,ignore.case = T,fixed = F)}))
#throwback
# toMatch <- c('throwback', 'TBT')
# combdat.secSum$secCat.throwback <- as.numeric(sapply(combdat.secSum$sectTitle,
#                                                     function(x){grepl(paste(toMatch,collapse="|"),
#                                                                       x,ignore.case = T,fixed = F)}))

#Contextual
#*ing, work, home, morning, afternoon, evening, workout, cardio family

toMatch <- c('*ing', '*work', 'home', 'morning', 'afternoon', 'evening', 
             '*workout*', 'cardio*','family*','energy','monday','hump','TGIF','rain')
combdat.secSum$secCat.contextual <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                      function(x){grepl(paste(toMatch,collapse="|"),
                                                                        x,ignore.case = T,fixed = F)}))
combdat.secSum$secCat.contextual <- ifelse(grepl('trending',
                                                 combdat.secSum$sectTitle,ignore.case = T,fixed = F) &
                                             combdat.secSum$secCat.contextual==1,0,combdat.secSum$secCat.contextual)

#Contextual:time

toMatch <- c('morning', 'afternoon', 'evening','hump','monday','TGIF')
combdat.secSum$secCat.contextual.time <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                           function(x){grepl(paste(toMatch,collapse="|"),
                                                                             x,ignore.case = T,fixed = F)}))
combdat.secSum$secCat.contextual.time <- ifelse(grepl('trending',
                                                      combdat.secSum$sectTitle,ignore.case = T,fixed = F) &
                                                  combdat.secSum$secCat.contextual.time==1,0,combdat.secSum$secCat.contextual)
#Contextual:activity
toMatch <- c('*ing', '*workout*', 'cardio*','family*','energy','feel')
combdat.secSum$secCat.contextual.activity <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                               function(x){grepl(paste(toMatch,collapse="|"),
                                                                                 x,ignore.case = T,fixed = F)}))
#Contextual:location
# toMatch <- c('*home', 'work ',' work')
# combdat.secSum$secCat.contextual.loc <- as.numeric(sapply(combdat.secSum$sectTitle,
#                                                         function(x){grepl(paste(toMatch,collapse="|"),
#                                                                           x,ignore.case = T,
#                                                                           fixed = F)}))

#Trending
toMatch <- c('trending', 'new','hot','today','top','watch','fresh')
combdat.secSum$secCat.trending <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                    function(x){grepl(paste(toMatch,collapse="|"),
                                                                      x,ignore.case = T,
                                                                      fixed = F)}))
combdat.secSum$secCat.trending <- ifelse(grepl('video',
                                               combdat.secSum$sectTitle,ignore.case = T,fixed = F) &
                                           combdat.secSum$secCat.trending==1,0,combdat.secSum$secCat.trending)

#Genre
toMatch <- c('Country', 'rock','hip','pop','indi','instrum','00','90','80','70')

combdat.secSum$secCat.genre <- as.numeric(sapply(combdat.secSum$sectTitle,
                                                 function(x){grepl(paste(toMatch,collapse="|"),
                                                                   x,ignore.case = T,
                                                                   fixed = F)}))
combdat.secSum$catMatch <- rowSums(combdat.secSum[grep('secCat',names(combdat.secSum))])
table(combdat.secSum$catMatch)

# View(unique(combdat.secSum[c('catMatch','sectTitle')]))

combdat.pidSum <- combdat %>% group_by(intro.email) %>%
  summarise(avgitemQual = mean(itemQual,na.rm=T),
            avgSecQual = mean(secQual,na.rm=T),
            nGoodItems = sum(itemQual %in% c(1,2)),
            nBadItems = sum(itemQual %in% c(-1,-2)),
            netitemQual = sum(itemQual,na.rm=T),
            netSecQual = sum(secQual,na.rm=T),
            nAlbums = sum(itemType=='Album',na.rm=T),
            nVideos = sum(itemType=='Video',na.rm=T),
            nPlaylists = sum(itemType=='Playlist',na.rm=T),
            nItems = length(which(!is.na(itemQual))),
            final.taste=final.taste[1],
            final.context=final.context[1],
            final.discover=final.discover[1],
            final.valueProp=final.valueProp[1]
  )

tmp <- combdat.secSum %>% group_by(intro.email) %>%
  summarise(
    nGoodSecs = sum(secQual %in% c(1,2)),
    nBadSecs = sum(secQual %in% c(-1,-2)),
    nsecCat.contextual = sum(secCat.contextual),
    nsecCat.contextual.activity = sum(secCat.contextual.activity),
    nsecCat.contextual.time = sum(secCat.contextual.time),
    nsecCat.yourFavs = sum(secCat.yourFavs),
    nsecCat.vid = sum(secCat.vid),
    nsecCat.newRelease = sum(secCat.newRelease),
    nsecCat.listenAgain = sum(secCat.listenAgain),
    nsecCat.similarTo = sum(secCat.similarTo),
    nsecCat.moreFrom = sum(secCat.moreFrom),
    nsecCat.trending = sum(secCat.trending),
    nsecCat.genre = sum(secCat.genre))

combdat.pidSum <- merge(combdat.pidSum,tmp,by='intro.email')

dataList <- list(combdat,
combdat.pidSum,
combdat.secSum)

return(dataList)
}

