showtext_auto()

# Initialize simulation parameters
reps <- 200
repsTune <- 30
rounds <- 2000
players <- 20
kVec <- 2*(1:25)
evalRounds <- 1900:2000
start<- 50
nu2 <- 400
gamesPerRound <- 50

roundsTune <- max(evalRounds)

# Initialize simulation variables and performance dataframes
init <- rep(2000,players)
vars <- rep(200^2,players)

tuneGames <- list()
tuneSkills <- list()

tic("Make tuning games")
for (i in 1:repsTune) {
  skillVec <- rnorm(players,2000,200)
  temp <- MakeGlickmanGames(skill=skillVec,
                            skillVar = rep(nu2,players),
                            tau=0,
                            games=MatchRandomly(m=players,
                                                n=roundsTune,
                                                roundLength = gamesPerRound),
                            prob=GetBTProb,
                            scaling=log(10)/400)
  tuneGames[[i]] <- temp[[1]]
  tuneSkills[[i]] <- temp[[2]]
}
toc()

tunePerfDF <- expand.grid(round = 1:(roundsTune+1),
                          k=kVec,
                          cors=NaN,
                          RMSE=NaN)

tic("Rate tuning games")
for (i in kVec) {
  print(paste("k=",i))
  current <- which(tunePerfDF$k == i)
  tunePerfDF[current,] <- CheckPerf(tuneGames,
                                      tuneSkills,
                                      tunePerfDF[current,],
                                      RateElo,
                                      prob=GetBTProb,
                                      k=i,
                                      init,
                                      coeff=log(10)/400,
                                      verbose = TRUE)
}
toc()
bestKDF <- tunePerfDF[which(tunePerfDF$round %in% evalRounds),]
bestKDF <- bestKDF %>% 
  group_by(k) %>% 
  summarise(avg=mean(RMSE))


ggplot(data=bestKDF,aes(x=k,y=avg))+geom_point()

bestKValue <- bestKDF$k[which.min(bestKDF$avg)]


# Initialize perfDF
perfDFElo <- expand.grid(round = 1:(rounds+1),
                         cors=NaN,
                         RMSE=NaN,
                         method=0)

perfDFGlicko <- expand.grid(round = 1:(rounds+1),
                            cors=NaN,
                            RMSE=NaN,
                            method=2)

perfDFTM <- expand.grid(round = 1:(rounds+1),
                        cors=NaN,
                        RMSE=NaN,
                        method=3)

perfDFTrueskill <- expand.grid(round = 1:(rounds+1),
                               cors=NaN,
                               RMSE=NaN,
                               method=5)
tic()
# Generate games and skills
games <- list()
skills <- list()
for (i in 1:reps) {
  skillVec <- rnorm(players,2000,200)
  temp <- MakeGlickmanGames(skill=skillVec,
                            skillVar = rep(nu2,players),
                            tau=0,
                            games=MatchRandomly(m=players,
                                                n=rounds,
                                                roundLength = gamesPerRound),
                            prob=GetBTProb,
                            scaling=log(10)/400)
  games[[i]] <- temp[[1]]
  skills[[i]] <- temp[[2]]
}
toc()

# Rate games
tic("Rate games")
perfDFElo <- CheckPerf(games,
                       skills,
                       fill=perfDFElo,
                       RATE=RateElo,
                       prob=GetBTProb,
                       init,
                       k=bestKValue,
                       coeff=log(10)/400)

perfDFGlicko <- CheckPerf(games,
                          skills,
                          fill=perfDFGlicko,
                          RATE=RateGlicko,
                          center=TRUE,
                          priorMeans=init,priorVar=vars,q=log(10)/400,nu=20)

perfDFTM <- CheckPerf(games,
                      skills,
                      fill=perfDFTM,
                      RATE=RateTMExact,
                      center=TRUE,
                      init,
                      vars,
                      beta2=(400/log(10))^2*(pi^2/6),
                      nu=20)

perfDFTrueskill <- CheckPerf(games,
                             skills,
                             fill=perfDFTrueskill,
                             RATE=RateTrueskill,
                             center=TRUE,
                             data.frame(init,sqrt(vars)),
                             beta=(400/log(10))*(pi/sqrt(6)),
                             nu=20)
toc()

perfDF <- rbind(perfDFElo,perfDFGlicko,perfDFTM,perfDFTrueskill)
perfDF$method <- as.factor(perfDF$method)
levels(perfDF$method) <- c("Elo, d=0","Glicko","TM-Exact","Trueskill")


ggplot(perfDF[which(perfDF$round>100 & perfDF$round%%50==0),],
       aes(x=round,y=RMSE,col=as.factor(method),shape=as.factor(method)))+
  geom_point(size=2)+
  scale_color_manual(values=c("gold","#77DDFF","#5599FF","navy"))+
  scale_shape_manual(values=c(15,17,16,18))+
  labs(col="Method",shape="Method",x="Round",title="Method Comparison with Changing Skills")+
  theme_classic()+theme(text=element_text(size=24,  family="EB Garamond"))

  
write.csv(perfDF,"Changing_skills_final_02.csv")  

bestKDF
