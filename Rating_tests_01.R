library(tictoc)

skillVec <- runif(20,1500,2500)
init <- rep(2000,20)
ratingsElo <- RateElo(MakeGames(GetBTProb(skillVec,log(10)/400),MatchRandomly(20,800)),
                   GetBTProb,
                   init,
                   32,
                   log(10)/400)
cor(ratingsElo[801,],skillVec)
sqrt(mean((skillVec-ratingsElo[801,])^2))
cors <- apply(ratingsElo,1,cor,y=skillVec)
RMSE <- apply(ratingsElo,1,GetRMSE,y=skillVec)
plot(1:801,cors)
plot(1:801,RMSE)

skillVec <- rnorm(20,1500,200)
init <- rep(1500,20)
ratingsElo <- RateElo(MakeGames(GetBTProb(skillVec,log(10)/400),MatchRandomly(m=20,n=800,roundLength = 100)),
                   GetBTProb,
                   init,
                   32,
                   log(10)/400)
cor(ratingsElo[201,],skillVec)
cors <- apply(ratingsElo,1,cor,y=skillVec)
RMSE <- apply(ratingsElo,1,GetRMSE,y=skillVec)
ProbRMSE <- apply(ratingsElo,1,GetProbRMSE,
                  y=skillVec,
                  prob=GetBTProb,
                  coeff=log(10)/400)
plot(1:801,cors)
plot(1:801,RMSE)
plot(1:801,ProbRMSE)

init <- rep(1500,20)
initVar <- rep(200^2,20)
ratings <- RateGlicko(MakeGames(GetBTProb(skillVec,log(10)/400),MatchRandomly(m=20,n=800,roundLength = 100)),
                      priorMeans = init,
                      priorVar=initVar,
                      nu=10,
                      q=log(10)/400)
means <- ratings[[1]]
variances <- ratings[[2]]
variances[9,]
means[9,]

cor(means[9,],skillVec)

sqrt(mean((skillVec-means[9,])^2))

tic()
for(i in 1:100000000) {
  1/(i^1)
}
toc()

tic()
for(i in 1:100000000) {
  1/(i^0)
}
toc()

tic()
for(i in 1:100000000) {
  1/1
}
toc()

tic()
for(i in 1:100000000) {
  1
}
toc()

skills <- c(1,2,3,4,5,6)
tic()
for(i in 1:1000) games <- MakeGames(GetBTProb(skills),MatchRandomly(6,100))
toc()

tic()
games <- MakeGames(GetBTProb(skills),MatchRandomly(6,100000))
toc()


tic()
myMat <- matrix(rnorm(100000,0,1),nrow=1000,byrow=TRUE)
toc()

tic()
for(i in 1:1000) rnorm(100,0,1)
toc()


