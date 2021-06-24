###################################
###    Deprecated functions     ###
###################################

# Old method for computing Bradley-Terry probabilities.
# Less vectorized (ergo probably less efficient) than
# GetBTProb.
GetBTProbScalar <- function(skills,coeff=1,h=0) {
  n <- length(skills)
  probMat <- matrix(0,n,n)
  for(i in 1:n) {
    for(j in 1:n) {
      probMat[i,j] <- 1/(1+exp(-coeff*(skills[i]-skills[j]+h))) 
    }
  }
  return(probMat)
}

# Deprecated -- replaced by MatchBySkill
#
# Schedules players based on skills. If closest=TRUE,
# tries to schedule players with similar skills; o.w.
# tries to schedule players with dissimilar skills.
#
# For games under competitive matchmaking, can use
# current skill estimates for the skills argument
#
# Scheduling is deterministic.
MatchBySkillDet <- function(skills,closest=TRUE,n=1) {
  m <- length(skills)
  r <- ceiling(m/2)
  matchups <- data.frame("i"=c(),"j"=c())
  for (k in 1:n) {
    temp <- data.frame("i"=rep(0,r),
                       "j"=rep(0,r))
    options <- 1:m
    for (k in 1:floor(m/2)) {
      temp$i[k] <- options[which.max(skills[options])]
      options <- (1:m)[-c(temp$i,temp$j)]
      if (closest) temp$j[k] <- options[which.max(skills[options])]
      else temp$j[k] <- options[which.min(skills[options])]
      options <- (1:m)[-c(temp$i[1:k],temp$j)]
    }
    if (m %% 2 == 1) {
      temp$i[r] <- options[1]
      temp$j[r] <- ceiling(runif(1,0,m))
    }
    matchups <- rbind(matchups,temp)
  }
  matchups$t <- rep(1:n,each=r)
  return(matchups)
}

OldMatchAllPlayers <- function(m,n=1) {
  c <- ceiling(m/2)
  f <- floor(m/2)
  matchups <- data.frame("t" = rep(1:n,each=c))
  iTeams <- rep(0,c)
  jTeams <- rep(0,c)
  for(k in 0:(n-1)) {
    iTeams <- sample(1:m,c,replace=FALSE)
    jTeams[1:f] <- sample((1:m)[-iTeams],f,replace=FALSE)
    if(f<c) jTeams[c] <- sample((1:m)[-iTeams[c]],1,replace=FALSE)
    matchups$i[(k*c+1):(k*c+c)] <- iTeams
    matchups$j[(k*c+1):(k*c+c)] <- jTeams
  }
  return(matchups)
}


###########################################
###    Functions for running tests      ###
###########################################
TestSchedulingFunction <- function(FUN,
                                   mGrid=2:100,
                                   nGrid=1:15,
                                   times=50,
                                   ...) {
  for (i in mGrid) for (j in nGrid) for (k in times) {
    temp <- FUN(m=i,n=j,...)
    if (sum(temp$i == temp$j) != 0) {
      print(temp)
      return(FALSE)
    }
    if (sum(temp$i > i)+sum(temp$i<1) != 0) {
      print(temp)
      return(FALSE)
    }
    if (sum(temp$j > i)+sum(temp$j<1) != 0) {
      print(temp)
      return(FALSE)
    }
  }
  return(TRUE)
}
TestSchedulingFunction(OldMatchAllPlayers)


###################################
###    Tests for functions      ###
###################################
skills <- runif(10,min=1500,max=2500)
skills2 <- c(1,1,2,3,4,4,0)
q=log(10)/400

### Probability calculation tests

# Confirm that the vectorized BTProb function produces the
# same output as the non-vectorized one
GetBTProb(skills,q)-GetBTProbScalar(skills,q)

# Test functions
GetTMProb(c(0,1,1.5,1.96))
GetBTProb(c(0,1,1.5,1.96))

GetTMProb(c(0,1,1.5,1.96),h=0.5)
GetBTProb(c(0,1,1.5,1.96),h=0.5)

GetTMProb(skills,q)
GetTMProb(skills,q,h=1)
GetTMProb(skills,q,h=35)
GetTMProb(skills,q,h=1000)

GetStepProb(c(0,1,2,3,4,0))
GetStepProb(c(0,1,2,3,4,0),h=1)

### Scheduling tests

  
MatchRandomly(10,20)
MatchRandomly(10,20,roundLength = 2)
MatchRandomly(7,20,roundLength=3)
MatchRandomly(5,12)
MatchRandomly(4)
TestSchedulingFunction(MatchRandomly,nGrid=1:1000)

MatchAllPlayers(6,5)
MatchAllPlayers(5,3)
MatchAllPlayers(10)
TestSchedulingFunction(MatchAllPlayers)

for(i in 1:10) print(MatchAllPlayers(2))
MatchAllPlayers(2,50)


MatchAllPairs(4,2)
MatchAllPairs(4,2,randomize=TRUE)
MatchAllPairs(5)
TestSchedulingFunction(MatchAllPairs)

MatchBySkill(c(1,6,2,5,3,4),n=2,closest=FALSE)
MatchBySkill(c(1,6,2,5,3,4),n=2)
MatchBySkill(c(1,6,2,5,3,4),n=4)
MatchBySkill(c(1,6,2,5,3,4),n=4,df=1)
MatchBySkill(c(1,6,2,5,3,4),n=4,df=1,randomize=TRUE)

MatchBySkill(1:10)
MatchBySkill(1:11)
MatchBySkill(1:20,df=1)
MatchBySkill(1:20,df=1.75)
MatchBySkill(1:20,df=4)
MatchBySkill(1:20,df=30)

MatchBySkill(1:20,randomize = TRUE)
MatchBySkill(1:21,randomize = TRUE)

MatchBySkill(1:21,df=2)

MatchBySkill(c(5,1,1,2,3,5,0))
MatchBySkill(c(5,1,1,2,3,5,0),df=2)
MatchBySkill(c(5,1,1,2,3,5,0),df=3)

MatchBySkill(c(5,1,1,2,3,5,0),randomize=TRUE)

MatchBySkill(c(5,1,1,2,3,5,0),closest=FALSE)


### Game-making tests
MakeGames(prob=GetBTProb(skills,q),
          games = MatchRandomly(length(skills),25))

MakeGames(prob=GetTMProb(skills,q),
          games = MatchAllPairs(length(skills),1))
MakeGames(prob=GetTMProb(skills,q),
          games = MatchAllPairs(length(skills),1))

games <- MakeGames(prob = GetBTProb(skills,q),
                   games = MatchRandomly(length(skills),10000))
sum(games$p*games$results)/sum(games$results)
sum((1-games$p)*games$results)/sum(1-games$results)
for(i in 0:19) {
  print(mean(games$results[which((games$p>i*0.05) & (games$p < (i+1)*0.05))]))
}

MakeGames(GetBTProb(skills2),MatchRandomly(length(skills2),100))

MakeGlickmanGames(c(1,2,3,4),c(0,0,0,0),0,MatchAllPlayers(4,4))
MakeGlickmanGames(c(1,2,3,4),c(0.25,0.25,0.25,0),0.125,MatchAllPlayers(4,4))


MakeTournament(GetBTProb(c(0,100,200,300,400,500,600)))
MakeTournament(GetBTProb(c(0,0,50,100,200,300,400,500,600,600)))

MakeTournament(GetBTProb(c(0,100,200,300,400,500,600)),byTournament = FALSE,n=3)

MakeTournament(GetBTProb(c(0,100,200,300,400,500,600)),byTournament = TRUE,n=3)


#Benchmarking Bradley-Terry prob functions
x <- matrix(0,nrow=4000,ncol=4000)

tic()
x <- GetBTProb(1:4000,q)
toc()

tic()
x <- GetBTProbScalar(1:4000,q)
toc()

#Mild difference observed. Can't be bothered to confirm
#at this juncture through more rigorous benchmarking
