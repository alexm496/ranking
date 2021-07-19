# Initialize simulation parameters
reps <- 175
repsTune <- 30
rounds <- 2000
players <- 20
kVec <- 3*(5:35)
hVec <- 15*0:20
decayVec <- c(0,0.5)
evalRounds <- 1900:2000
start<- 50
gamesPerRound <- players*3

# Initialize simulation variables and performance dataframes
init <- rep(2000,players)
vars <- rep(200^2,players)

tuneGames <- list()
tuneSkills <- list()
tuneRounds <- max(evalRounds)

for (i in 1:repsTune) {
  skillVec <- rnorm(players,2000,200)
  tuneSkills[[i]] <- skillVec
  tuneGames[[i]] <- MakeGames(GetBTProb(skillVec,log(10)/400),
                              MatchRandomly(m=players,n=tuneRounds,roundLength = gamesPerRound))
}

tunePerfDF <- expand.grid(round = 1:(tuneRounds+1),
                          k=kVec,
                          LR=NaN,
                          decay=decayVec,
                          cors=NaN,
                          RMSE=NaN)
tunePerfDF$LR <- tunePerfDF$k*(36^tunePerfDF$decay) + 17*players*tunePerfDF$decay

for (i in kVec) {
  print(paste("k=",i))
  for (d in decayVec) {
    current <- which(tunePerfDF$k == i & tunePerfDF$decay == d)
    #print("Hello")
    #print(summary(current))
    tunePerfDF[current,] <- CheckPerf(tuneGames,
                                      tuneSkills,
                                      tunePerfDF[current,],
                                      RateElo,
                                      prob=GetBTProb,
                                      k=tunePerfDF$LR[current[1]],
                                      decay=d,
                                      init,
                                      coeff=log(10)/400,
                                      verbose = TRUE)
  }
}
bestKDF <- tunePerfDF[which(tunePerfDF$round %in% evalRounds),]
bestKDF <- bestKDF %>% 
  group_by(k,decay) %>% 
  summarise(LR=mean(LR),avg=mean(RMSE))


ggplot(data=bestKDF,aes(x=LR,y=avg,col=as.factor(decay)))+geom_point()

bestKDF <- bestKDF %>% ungroup() %>%
  group_by(decay) %>% summarise(bestK=LR[which.min(avg)])

bestKValues <- bestKDF$bestK

# Initialize perfDF
perfDFElo <- expand.grid(round = 1:(rounds+1),
                         h=hVec,
                         cors=NaN,
                         RMSE=NaN,
                         method=1:length(decayVec))

perfDFGlicko <- expand.grid(round = 1:(rounds+1),
                            h=hVec,
                            cors=NaN,
                            RMSE=NaN,
                            method=3)

perfDFTM <- expand.grid(round = 1:(rounds+1),
                        h=hVec,
                        cors=NaN,
                        RMSE=NaN,
                        method=4)

perfDFTrueskill <- expand.grid(round = 1:(rounds+1),
                               h=hVec,
                               cors=NaN,
                               RMSE=NaN,
                               method=5)

# Generate games and skills
games <- list()
skills <- list()
schedules <- list()

for (i in 1:reps) {
  schedules[[i]] <- MatchRandomly(m=players,
                                  n=rounds,
                                  roundLength = gamesPerRound)
  skills[[i]] <- rnorm(players,2000,200)
}

for(i in 1:length(hVec)) {
  #print(i)
  games[[i]] <- list()
  for (j in 1:reps) {
    games[[i]][[j]] <- MakeGames(GetBTProb(skills[[j]],log(10)/400,h=hVec[i]),
                                     schedules[[j]])
  }
  #print("Hello")
}

# Rate games
for(l in 1:length(decayVec)) {
  print(l)
  for (i in 1:length(hVec)) {
    current <- which(perfDFElo$method == l & perfDFElo$h == hVec[i])
    perfDFElo[current,] <- CheckPerf(games[[i]],
                                     skills,
                                     fill=perfDFElo[current,],
                                     RATE=RateElo,
                                     prob=GetBTProb,
                                     init,
                                     k=bestKValues[l],
                                     decay=decayVec[l],
                                     coeff=log(10)/400)
  }
}

ggplot(perfDFElo[which(perfDFElo$round==900),],aes(x=h,y=RMSE,col=method))+geom_point()

for (i in 1:length(hVec)) {
  current <- which(perfDFGlicko$h==hVec[i])
  perfDFGlicko[current,] <- CheckPerf(games[[i]],
                          skills,
                          fill=perfDFGlicko[current,],
                          RATE=RateGlicko,
                          priorMeans=init,priorVar=vars,q=log(10)/400)
}

tempDF <- perfDFElo[which(perfDFElo$round>900 & perfDFElo$decay==1),] %>%
  group_by(h) %>% summarise(avg=mean(RMSE))

ggplot(tempDF, aes(x=h,y=avg))+geom_point()
  
ggplot(perfDFGlicko[which(perfDFGlicko$round==900),],
       aes(x=h,y=RMSE)) +geom_point()

for (i in 1:length(hVec)) {
  current <- which(perfDFTM$h == hVec[i])
  perfDFTM[current,] <- CheckPerf(games[[i]],
                          skills,
                          fill=perfDFTM[current,],
                          RATE=RateTMExact,
                          init,vars,beta2=(400/log(10))^2*(pi^2/6))
}

for (i in 1:length(hVec)) {
  current <- which(perfDFTrueskill$h == hVec[i])
  perfDFTrueskill[current,] <- CheckPerf(games[[i]],
                             skills,
                             fill=perfDFTrueskill[current,],
                             RATE=RateTrueskill,
                             data.frame(init,sqrt(vars)),
                             beta=(400/log(10))*(pi/sqrt(6)))
}

ggplot(perfDFTrueskill[which(perfDFTrueskill$round==900),],
       aes(x=h,y=RMSE))+geom_point()

perfDF <- rbind(perfDFElo,perfDFGlicko,perfDFTM,perfDFTrueskill)

perfDF$method <- as.factor(perfDF$method)
levels(perfDF$method) <- c("Elo d=0","Elo d=0.5","Glicko","TM-Exact","Trueskill")

summaryDF <- perfDF[which(perfDF$round >= 1800),] %>%
  group_by(h,method) %>% summarise(RMSE=mean(RMSE),cor=mean(cors))

ggplot(summaryDF,aes(x=h,y=RMSE,col=method,shape=method))+
  geom_point(size=2)+
  scale_color_manual(values=c("gold","orange","#77DDFF","#5599FF","navy"))+
  scale_shape_manual(values=c(15,12,17,16,18))+
  labs(col="Method",shape="Method",y="Mean RMSE, rounds 1800-2000",title="Robustness to Order Effect")+
  theme_classic()+theme(text=element_text(size=24,  family="EB Garamond"))


bestKDF
