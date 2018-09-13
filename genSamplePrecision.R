
simLikertData <- function(popSize = 1e4,simMin = 1, simMax = 5, simMean = 3.5, simSD = .5,summariseSim = F,plotSim=F){
  x <- rnorm(popSize,mean=simMean,sd=simSD)
  x <- ifelse(x<simMin,simMin,x)
  x <- ifelse(x>simMax,simMax,x)
  x <- round(x)
  
  
  if(summariseSim) {
    describe(x)
    
    
  }
  
  if(plotSim) {
    hist(x,breaks = seq(simMin,simMax,1),xlim = c(simMin,simMax),
         main = 'Distribution of \nSimulated Likert Data',
         xlab = 'simVals'
    )
    
    
  }
  
  return(x)
}

genSamplePrecision <- function(
scaleMin = 1,
scaleMax = 5,
simMean = 3,
simSD = .5,
plotSim = F,
confLevel = .95,
R = 1e3,
nSeq = c(seq(5,100,5),seq(120,500,20),seq(600,3000,100))){

simDat <- simLikertData(simMin = scaleMin,
                        simMax = scaleMax,
                        simMean = simMean,simSD = simSD,plotSim = plotSim)

# For a given population mean and sd, how precise can we expect sample CIs to be for different N's

reps.l <- list()
for(i in 1:length(nSeq)){
  
  n.i <- nSeq[i]
  
  boot.t.i <- lapply(1:R,function(i){
    
    sample.i <- sample(simDat,n.i)
    
    if(sd(sample.i)==0){sample.i <- sample(simDat,n.i)+rnorm(n.i,sd = .01)}
    
    t.result.i <- t.test(sample.i,conf.level = confLevel)
    est <- t.result.i$estimate
    LL <- t.result.i$conf.int[1]
    UL <- t.result.i$conf.int[2]
    
    LL <- ifelse(LL<min(simDat),min(simDat),LL)
    UL <- ifelse(UL>max(simDat),max(simDat),UL)
    return(data.frame(est,sampleSD = sd(sample.i),LL,UL))
    
  }) %>%bind_rows()%>%summarise_all(mean)
  
  half.ciWidth <- (boot.t.i$UL - boot.t.i$LL)
  precision <- 1/(half.ciWidth)
  print(i)
  reps.l[[i]] <- data.frame(popMean = round(mean(simDat),2),popSD = round(sd(simDat),2),
                            N = n.i,boot.t.i,half.ciWidth,precision)
  
  
}

samplePrecision <- bind_rows(reps.l)
return(samplePrecision)}


gen2samplePower <- function(
  scaleMin = 1,
  scaleMax = 5,
  
  simMean1 = 3,
  simSD1 = 1.5,
  
  simMean2 = 3.5,
  simSD2 = 1.5,
  
  plotSim = T,
  confLevel = .95,
  R = 1e3,
  
  nSeq = c(seq(5,100,5),seq(120,500,20),seq(600,3000,100))){
    
    
    
    simDat1 <- simLikertData(simMin = scaleMin,
                             simMax = scaleMax,
                             simMean = simMean1,simSD = simSD1,plotSim = plotSim)
    
    simDat2 <- simLikertData(simMin = scaleMin,
                             simMax = scaleMax,
                             simMean = simMean2,simSD = simSD2,plotSim = plotSim)
    
    
    # For 2 given population means and sds, what is the likelihood we can detect the difference for different sample sizes
    
    reps.l <- list()
    
    for(i in 1:length(nSeq)){
      
      n.i <- nSeq[i]
      
      boot.t.i <- lapply(1:R,function(i){
        
        sample.i1 <- sample(simDat1,n.i)
        sample.i2 <- sample(simDat2,n.i)
        
        if(sd(sample.i1)==0){sample.i1 <- sample(simDat1,n.i)+rnorm(n.i,sd = .01)}
        if(sd(sample.i2)==0){sample.i2 <- sample(simDat2,n.i)+rnorm(n.i,sd = .01)}
        
        t.result.i1 <- t.test(sample.i1,conf.level = confLevel)
        est1 <- t.result.i1$estimate
        LL1 <- t.result.i1$conf.int[1]
        UL1 <- t.result.i1$conf.int[2]
        
        LL1 <- ifelse(LL1<scaleMin,scaleMin,LL1)
        UL1 <- ifelse(UL1>scaleMax,scaleMax,UL1)
        
        t.result.i2 <- t.test(sample.i2,conf.level = confLevel)
        est2 <- t.result.i2$estimate
        LL2 <- t.result.i2$conf.int[1]
        UL2 <- t.result.i2$conf.int[2]
        
        LL2 <- ifelse(LL2<scaleMin,scaleMin,LL2)
        UL2 <- ifelse(UL2>scaleMax,scaleMax,UL2)
        
        power <- ifelse((UL1 - LL2)*(LL1 - UL2)>0,1,0)
        power.alt <- ifelse(t.test(sample.i2,sample.i1)$p.value<(1-confLevel),1,0)
        meanDiff <- est1-est2
        cohensD <- meanDiff/sqrt((sd(sample.i1)^2+sd(sample.i1)^2)/2)
        
        return(data.frame(est1,LL1,UL1,
                          est2,LL2,UL2,
                          power,power.alt,
                          meanDiff,cohensD
        ))
        
      }) %>%bind_rows()%>%summarise_all(mean)
      
      # half.ciWidth <- (boot.t.i$UL - boot.t.i$LL)
      # precision <- 1/(half.ciWidth)
      print(i)
      reps.l[[i]] <- data.frame(popMean1 = round(mean(simDat1),2),
                                popSD1 = round(sd(simDat1),2),
                                popMean2 = round(mean(simDat2),2),
                                popSD2 = round(sd(simDat2),2),
                                
                                N = n.i,boot.t.i)
      
      
    }
    
    samplePrecision <- bind_rows(reps.l)
    return(samplePrecision)}
  

genSamplePrecision.bin <- function(
  popProp = .8,
  confLevel = .95,
  R = 1e3,
  nSeq = c(seq(5,100,5),seq(120,500,20),seq(600,3000,100))){
  
  simDat <- rbinom(1e4,1,prob = popProp)
  
  # For a given population mean and sd, how precise can we expect sample CIs to be for different N's
  
  reps.l <- list()
  for(i in 1:length(nSeq)){
    
    n.i <- nSeq[i]
    
    boot.t.i <- lapply(1:R,function(i){
      
      sample.i <- sample(simDat,n.i)
      
      if(sd(sample.i)==0){sample.i <- c(sample(simDat,n.i),1,0)}
      
      t.result.i <- binom.test(c(sum(sample.i),length(sample.i)-sum(sample.i)),conf.level = confLevel)
      est <- t.result.i$estimate
      LL <- t.result.i$conf.int[1]
      UL <- t.result.i$conf.int[2]
      
      return(data.frame(est,sampleSD = sd(sample.i),LL,UL))
      
    }) %>%bind_rows()%>%summarise_all(mean)
    
    half.ciWidth <- (boot.t.i$UL - boot.t.i$LL)
    precision <- 1/(half.ciWidth)
    print(i)
    reps.l[[i]] <- data.frame(popProp = round(mean(simDat),2),
                              N = n.i,boot.t.i,half.ciWidth,precision)
    
    
  }
  
  samplePrecision <- bind_rows(reps.l)
  return(samplePrecision)}

gen2samplePower.bin <- function(
  popProp1 = .8,
  popProp2 = .5,
  
  confLevel = .95,
  R = 1e3,
  
  nSeq = c(seq(5,100,5),seq(120,500,20),seq(600,3000,100))){
  
  
  
  simDat1 <- rbinom(1e4,1,prob = popProp1)
  
  simDat2 <- rbinom(1e4,1,prob = popProp2)
  
  
  # For 2 given population means and sds, what is the likelihood we can detect the difference for different sample sizes
  
  reps.l <- list()
  
  for(i in 1:length(nSeq)){
    
    n.i <- nSeq[i]
    
    boot.t.i <- lapply(1:R,function(i){
      
      sample.i1 <- sample(simDat1,n.i)
      sample.i2 <- sample(simDat2,n.i)
      
      if(sd(sample.i1)==0){sample.i1 <- c(sample(simDat1,n.i),1,0)}
      if(sd(sample.i2)==0){sample.i2 <- c(sample(simDat2,n.i),1,0)}
      
      t.result.i1 <- binom.test(c(sum(sample.i1),length(sample.i1)-sum(sample.i1)),conf.level = confLevel)
      est1 <- t.result.i1$estimate
      LL1 <- t.result.i1$conf.int[1]
      UL1 <- t.result.i1$conf.int[2]
      
      
      t.result.i2 <- binom.test(c(sum(sample.i2),length(sample.i2)-sum(sample.i2)),conf.level = confLevel)
      est2 <- t.result.i2$estimate
      LL2 <- t.result.i2$conf.int[1]
      UL2 <- t.result.i2$conf.int[2]
      
      power <- ifelse((UL1 - LL2)*(LL1 - UL2)>0,1,0)
      meanDiff <- est1-est2
      
      
      return(data.frame(est1,LL1,UL1,
                        est2,LL2,UL2,
                        power,
                        meanDiff
      ))
      
    }) %>%bind_rows()%>%summarise_all(mean)
    
    print(i)
    reps.l[[i]] <- data.frame(popMean1 = round(mean(simDat1),2),
                              popSD1 = round(sd(simDat1),2),
                              popMean2 = round(mean(simDat2),2),
                              popSD2 = round(sd(simDat2),2),
                              
                              N = n.i,boot.t.i)
    
    
  }
  
  samplePrecision <- bind_rows(reps.l)
  return(samplePrecision)}
