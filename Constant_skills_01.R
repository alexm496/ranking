# Initialize simulation parameters
reps <- 150
repsTune <- 50
rounds <- 2000
players <- 20
kVec <- 3*(3:35)
decayVec <- c(0,0.5)
evalRounds <- 1900:2000
start<- 50
gamesPerRound <- 2.5*players

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
                      cors=NaN,
                      RMSE=NaN,
                      method=1:length(decayVec))

perfDFGlicko <- expand.grid(round = 1:(rounds+1),
                         cors=NaN,
                         RMSE=NaN,
                         method=3)

perfDFTM <- expand.grid(round = 1:(rounds+1),
                            cors=NaN,
                            RMSE=NaN,
                            method=4)

perfDFTrueskill <- expand.grid(round = 1:(rounds+1),
                        cors=NaN,
                        RMSE=NaN,
                        method=5)

# Generate games and skills
games <- list()
skills <- list()
for (i in 1:reps) {
  skillVec <- rnorm(players,2000,200)
  skills[[i]] <- skillVec
  games[[i]] <- MakeGames(GetBTProb(skillVec,log(10)/400),
                          MatchRandomly(m=players,n=rounds))
}

# Rate games
for(l in 1:length(decayVec)) {
  print(l)
  current <- which(perfDFElo$method == l)
  perfDFElo[current,] <- CheckPerf(games,
                                   skills,
                                   fill=perfDFElo[current,],
                                   RATE=RateElo,
                                   prob=GetBTProb,
                                   init,
                                   k=bestKValues[l],
                                   decay=decayVec[l],
                                   coeff=log(10)/400)
}

perfDFGlicko <- CheckPerf(games,
                          skills,
                          fill=perfDFGlicko,
                          RATE=RateGlicko,
                          priorMeans=init,priorVar=vars,q=log(10)/400)

perfDFTM <- CheckPerf(games,
                      skills,
                      fill=perfDFTM,
                      RATE=RateTMExact,
                      init,vars,beta2=(400/log(10))^2*(pi^2/6))

perfDFTrueskill <- CheckPerf(games,
                             skills,
                             fill=perfDFTrueskill,
                             RATE=RateTrueskill,
                             data.frame(init,sqrt(vars)),
                             beta=(400/log(10))*(pi/sqrt(6)))

perfDF <- rbind(perfDFElo,perfDFGlicko,perfDFTM,perfDFTrueskill)

perfDF$method <- as.factor(perfDF$method)
levels(perfDF$method) <- c("Elo, d=0","Elo, d=0.5","Glicko","TM-Exact","Trueskill")

modVec <- (perfDF$round%%20==0 & 
             (perfDF$method=="Elo, d=0" | 
                perfDF$method == "Elo, d=0.5" | 
                perfDF$method=="Trueskill")) |
  (perfDF$round%%40==0 & (perfDF$method=="Glicko")) | 
  (perfDF$round%%40==20 & (perfDF$method=="TM-Exact"))

ggplot(perfDF[which(perfDF$round>100 & modVec),],aes(x=round,y=RMSE,col=method,shape=method))+
  geom_point(size=2)+
  scale_color_manual(values=c("gold","orange","#77DDFF","#5599FF","navy"))+
  scale_shape_manual(values=c(15,12,17,16,18))+
  labs(col="Method",shape="Method",x="Round",title="Method Comparison with Constant Skills")+
  theme_classic()+theme(text=element_text(size=24,  family="EB Garamond"))

write.csv(perfDF,"Constant_skills_final_02.csv")

bestKDF
