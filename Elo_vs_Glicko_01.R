library(tidyverse)

reps <- 75
rounds <- 1000
players <- 10
kVec <- 10*(2:7)
cVec <- 25*(0:5)
corMat <- NULL
start<- 40
gamesPerRound <- 10

init <- rep(2000,players)
vars <- rep(200,players)
perfDFElo <- expand.grid(round = 1:(rounds+1),
                      k=kVec,
                      cors=NaN,
                      RMSE=NaN)
perfDFGlicko <- data.frame(round = gamesPerRound*(1+0:ceiling(rounds/gamesPerRound)),
                         cors=NA,
                         RMSE=NA)
games <- list()
skills <- list()
for (i in 1:reps) {
  skillVec <- rnorm(players,2000,200)
  skills[[i]] <- skillVec
  games[[i]] <- MakeGames(GetBTProb(skillVec,log(10)/400),
                          MatchRandomly(m=players,n=rounds,roundLength = gamesPerRound))
}

corMatTemp <- NULL
RMSEMatTemp <- NULL
for(i in 1:reps) {
  skillVec <- skills[[i]]
  ratingsGlicko <- RateGlicko(games[[i]],
                              init,
                              vars,
                              nu=0,
                              log(10)/400)[[1]]
  cors <- apply(ratingsGlicko,1,cor,y=skillVec)
  corMatTemp <- cbind(corMatTemp,cors)
  
  RMSE <- apply(ratingsGlicko,1,GetRMSE,y=skillVec)
  RMSEMatTemp <- cbind(RMSEMatTemp,RMSE)
}
cors <- apply(corMatTemp,1,mean)
RMSE <- apply(RMSEMatTemp,1,mean)

perfDF$cor[which(perfDF$k==l)] <- cors
perfDFGlicko$RMSE <- RMSE


tic()
for(l in kVec) {
  print(l)
  #print(perfDF[which(perfDFElo$k==l),])
  perfDFElo[which(perfDFElo$k==l),] <- checkPerf(games,
                                           skills,
                                           reps,
                                           fill=perfDFElo[which(perfDFElo$k == l),],
                                           RATE=RateElo,
                                           prob=GetBTProb,init,k=l,decay=0,coeff=log(10)/400)
}
toc()

#plot(perfDFGlicko$round,perfDFGlicko$RMSE)
#plot(perfDFElo$round,perfDFElo$RMSE)
     
perfDFElo$method <- 0
perfDFGlicko$method <- 1
perfDFGlicko$k = 32

perfDF <- rbind(perfDFElo,perfDFGlicko)

ggplot(perfDF,
       aes(x=round,y=RMSE,col=k,shape=as.factor(method))) + geom_point()



