reps <- 250
rounds <- 2000
players <- 50
kVec <- 300
start<- 40

init <- rep(2000,players)
perfDF <- expand.grid(round = 1:(rounds+1),
                      k=kVec,
                      gameType=1:4,
                      cor=NA,
                      RMSE=NA)

gamesRand <- list()
gamesAllPlayers <- list()
gamesAllPairs <- list()
gamesTournament <- list()

skills <- list()

tic()
for (i in 1:reps) {
  if (i %% 100 == 0) print(i)
  
  skillVec <- rnorm(players,2000,200)
  skills[[i]] <- skillVec
  gamesRand[[i]] <- MakeGames(GetBTProb(skillVec,log(10)/400),
                          MatchRandomly(m=players,n=rounds)[1:rounds,])
  gamesAllPlayers[[i]] <- MakeGames(GetBTProb(skillVec,log(10)/400),
                                    MatchAllPlayers(m=players,n=ceiling(2*rounds/players))[1:rounds,])
  gamesAllPairs[[i]] <- MakeGames(GetBTProb(skillVec,log(10)/400),
                                    MatchAllPairs(m=players,n=ceiling(2*rounds/(players*(players-1))),randomize=TRUE)[1:rounds,])
  gamesTournament[[i]] <- MakeTournament(GetBTProb(skillVec,log(10)/400),
                                         ceiling(rounds/ceiling(log2(players))),
                                         FALSE)[1:rounds,]
}
toc()

games <- list(gamesRand,gamesAllPlayers,gamesAllPairs,gamesTournament)


tic()
for(l in 1:length(games)) {
  print(l)
  perfDF[which(perfDF$gameType==l),] <- checkPerf(games[[l]],
                                           skills,
                                           reps,
                                           fill=perfDF[which(perfDF$gameType == l),],
                                           RATE=RateElo,
                                           prob=GetBTProb,init,k=32,decay=0.5,coeff=log(10)/400)
}
toc()

ggplot(perfDF[which(perfDF$round>=start),],aes(x=round,y=RMSE,color=as.factor(gameType)))+geom_point()

mean(perfDF$RMSE[which(perfDF$gameType == 4)]-perfDF$RMSE[which(perfDF$gameType == 1)])
mean(perfDF$RMSE[which(perfDF$gameType == 4)]-perfDF$RMSE[which(perfDF$gameType == 2)])
mean(perfDF$RMSE[which(perfDF$gameType == 4)]-perfDF$RMSE[which(perfDF$gameType == 3)])

