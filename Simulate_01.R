library(tictoc)

# This .R file is laid out as follows: the first set of 
# functions take in a vector of skills and output a matrix
# of pairwise win-loss probabilities; the second set of 
# functions take in a number of players (and sometimes
# other parameters) and output a schedule of games (in the
# form of a dataframe with columns for which player is player
# i and which is player j and a row for each game); then the
# function MakeGames takes in a matrix of pairwise win-loss 
# probabilities and a schedule of games and outputs a 
# dataframe containing the results.


###################################
###   Functions for computing   ###
###         probabilities       ###
###################################

# Old method for computing Bradley-Terry probabilities.
# Less vectorized (ergo probably less efficient) than
# GetBTProb.
GetBTProbScalar <- function(skills,coeff=1) {
  n <- length(skills)
  probMat <- matrix(0,n,n)
  for(i in 1:n) {
    for(j in 1:n) {
      probMat[i,j] <- 1/(1+exp(-coeff*(skills[i]-skills[j]))) 
    }
  }
  return(probMat)
}

# New method for computing Bradley-Terry probabilities.
#
# To get Glickman's Bradley-Terry model, set coeff=ln10/400.
GetBTProb <- function(skills, coeff=1) {
  n <- length(skills)
  skillMat <- matrix(rep(skills,n),nrow=n,byrow=TRUE)
  probMat <- (1+exp(coeff*(skillMat-t(skillMat))))^(-1)
  return(probMat)
}

# Function for computing Thurstone-Mosteller probabilities.
GetTMProb <- function(skills, coeff=1) {
  n <- length(skills)
  probMat <- matrix(0,n,n)
  for(j in 1:n) probMat[,j] <- pnorm(skills,
                                     mean = skills[j],
                                     sd=1/coeff)
  probMat
}


####################################
###   Functions for scheduling   ###
###            games             ###
####################################


#Chooses a player for each game unif. at random, then
#chooses another play for them to play against unif.
#at random.
MatchRandomly <- function(m,n=1000) {
  matchups <- data.frame("i" = rep(0,n))
  matchups$i <- ceiling(runif(n,min=0,max=m))
  matchups$j <- ((matchups$i-1 + ceiling(runif(n,min=0,max=m-1))) %% m)+1
  return(matchups)
}


#Assigns players to matches an opponents randomly, but
#each player plays exactly one game in the first m/2.
#If m is odd, a randomly-selected player plays 2 games.
#Param n is number of rounds, not number of games
MatchAllPlayers <- function(m,n=1) {
  c <- ceiling(m/2)
  f <- floor(m/2)
  matchups <- data.frame("i" = rep(0,c*n))
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


# Every possible pair of players, n times.
# If randomize=TRUE, return in a random order (within
# rounds).
#
# I suspect there's a one-line way to do this, but
# I couldn't find it.
MatchAllPairs <- function(m,n=1,randomize=FALSE) {
  r <- m*(m-1)/2
  matchups <- data.frame("i"=c(),"j"=c())
  for(k in 1:n) {
    for(l in 1:(m-1)) {
      matchups <- rbind(matchups,
            data.frame("i"=rep(l,m-l),
                       "j"=(l+1):m))
    }
    if(randomize) matchups[(1+((k-1)*r)):(k*r),] <- 
        (matchups[(1+((k-1)*r)):(k*r),])[sample(1:r,r),]
  }
  print(r)
  return(matchups)
}


# UNFINISHED
#
# Schedules players based on skills. If closest=TRUE,
# tries to schedule players with similar skills; o.w.
# tries to schedule players with dissimilar skills.
#
# For games under competitive matchmaking, can use
# current skill estimates for the skills argument
MatchBySkill <- function(skills,closest=TRUE,n) {
  temp <- data.frame("i"=c(),
                     "j"=c())
  for(k in 1:n) {
    temp <- data.frame("i"=c(),
                       "j"=c())
    while(length(temp$i) < floor(m/2)) {
      options <- (1:m)[-c(temp$i,temp$j)]
      temp$i[length(temp$i)+1] <- options[which.max(skills[options])]
      options <- (1:m)[-c(temp$i,temp$j)]
      temp$j[length(temp$j)+1] <- options[which.max(skills[options])]
    }
    #UNFINISHED
  }
}


####################################
###   Functions for simulating   ###
###            games             ###
####################################


# Simulates the games scheduled in dataframe games between
# players with pairwise win/loss probabilities taken from
# matrix prob.
#
# I tried to vectorize this function, but performance
# got worse. (In fact, the performance of this function
# was very strange -- I'm at a loss for why the code I
# have ran faster than other code I tried.)
MakeGames <- function(prob,games) {
  n <- length(games$i)
  
  #tic("Assign probabilities")
  games$p <- rep(0,length(games$i))
  for(k in 1:n) games$p[k] <- prob[games$i[k],games$j[k]]
  #toc()
  
  #tic("Play the game")
  for(k in 1:n) {
    games$results[k] <- rbinom(1,1,prob[games$i[k],games$j[k]])
  }
  #toc()
  
  return(games)
}

# Makes n tournaments between m players with matrix of
# win-loss probabilities prob. Each tournament works as
# follows: players are randomly assigned to games; the
# winners of one round progress to the next.
MakeTournament <- function(prob,n) {
  #UNFINISHED
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

GetTMProb(skills,q)


### Scheduling tests
MatchRandomly(10,20)
MatchRandomly(5,12)
MatchRandomly(4)

MatchAllPlayers(6,5)
MatchAllPlayers(5,3)
MatchAllPlayers(10)

MatchAllPairs(4,2)
MatchAllPairs(4,2,randomize=TRUE)
MatchAllPairs(5)


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
