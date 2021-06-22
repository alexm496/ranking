
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

MatchAllPlayers(6,5)
MatchAllPlayers(5,3)
MatchAllPlayers(10)

MatchAllPairs(4,2)
MatchAllPairs(4,2,randomize=TRUE)
MatchAllPairs(5)

MatchBySkill(c(1,6,2,5,3,4),n=2,closest=FALSE)
MatchBySkill(c(1,6,2,5,3,4),n=2)

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
