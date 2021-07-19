library(tictoc)
library(tidyverse)
library(showtext)
library(showtextdb)

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

# Function for computing Bradley-Terry probabilities.
#
# To get Glickman's Bradley-Terry model, set coeff=ln10/400.
GetBTProb <- function(skills, coeff=1,h=0) {
  n <- length(skills)
  skillMat <- matrix(rep(skills,n),nrow=n,byrow=TRUE)
  probMat <- (1+exp(coeff*(skillMat-t(skillMat)-h)))^(-1)
  return(probMat)
}

# Function for computing Thurstone-Mosteller probabilities.
GetTMProb <- function(skills, coeff=1,h=0) {
  n <- length(skills)
  probMat <- matrix(0,n,n)
  for(j in 1:n) probMat[,j] <- pnorm(skills,
                                     mean = skills[j]-h,
                                     sd=1/coeff)
  return(probMat)
}

# Function for computing "probabilities" by a step function
GetStepProb <- function(skills,h=0) {
  skillMat <- matrix(rep(skills,length(skills)),nrow=length(skills),byrow=TRUE)
  probMat <- (skillMat<t(skillMat)+h) + 0.5*(skillMat == t(skillMat)+h)
  return(probMat)
}


####################################
###   Functions for scheduling   ###
###            games             ###
####################################


# Chooses a player for each game unif. at random, then
# chooses another play for them to play against unif.
# at random.
#
# Param roundLength controls time, with each round of length
# roundLength (except the last round, which may be shorter). 
MatchRandomly <- function(m,n=1000,roundLength = 1) {
  times <- rep(1:ceiling(n/roundLength),each=roundLength)
  matchups <- data.frame("t" = times[1:n])
  matchups$i <- ceiling(runif(n,min=0,max=m))
  matchups$j <- ((matchups$i-1 + ceiling(runif(n,min=0,max=m-1))) %% m)+1
  return(matchups)
}


# Assigns players to matches an opponents randomly, but
# each player plays exactly one game in the first m/2.
# If m is odd, a randomly-selected player plays 2 games.
# Param n is number of rounds, not number of games
MatchAllPlayers <- function(m,n=1) {
  c <- ceiling(m/2)
  f <- floor(m/2)
  matchups <- data.frame("t" = rep(1:n,each=c))
  iTeams <- rep(0,c)
  jTeams <- rep(0,c)
  for(k in 0:(n-1)) {
    iTeams <- sample(1:m,c,replace=FALSE)
    jTeams[1:f] <- ((1:m)[-iTeams])[sample(1:f,f,replace=FALSE)]
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
  matchups$t=rep(1:n,each=r)
  return(matchups)
}

# Matches players by skill, matching similar players together if
# closest==TRUE, dissimilar players otherwise.
#
# If param randomize is used, randomizes order within rounds.
#
# Uses a chi-square distribution with df degrees of freedom to
# introduce randomness. If df=0, matchmaking is deterministic.
# Because of the way truncation is done, sufficiently high df
# leads to almost-deterministic matchmaking with opposite
# closest value.
MatchBySkill <- function(skills,df=0,closest=TRUE,n=1,randomize=FALSE) {
  m <- length(skills)
  r <- ceiling(m/2)
  matchups <- data.frame("i"=c(),"j"=c())
  
  for (l in 1:n) {
    temp <- data.frame("i"=c(rep(0,r)),
                       "j"=rep(0,r))
    options <- 1:m
    
    for (k in 1:floor(m/2)) {
      #print(k)
      temp$i[k] <- options[1]
      options <- options[2:length(options)]
      
      #floor(rchisq(1,df))+1 is used rather than ceiling(rchisq(1,df))
      #so that df=0 produces deterministic matchmaking
      if (closest) temp$j[k] <- options[min(floor(rchisq(1,df))+1,
                                            length(options))]
      else temp$j[k] <- options[length(options)-min(floor(rchisq(1,df)),
                                                    length(options)+1)]
      #print(c(temp$i,temp$j))
      options <- (1:m)[-c(temp$i,temp$j)]
    }
    
    if (m %% 2 == 1) {
      temp$i[r] <- options[1]
      temp$j[r] <- ((1:m)[-temp$i[r]])[max(m-1,
                                           min(1,
                                               floor(rnorm(1,
                                                           temp$i[r],
                                                           df))))]
    }
    
    temp$i <- order(skills)[temp$i]
    temp$j <- order(skills)[temp$j]
    
    if (randomize) temp <- temp[sample(1:r,r),]
    
    matchups <- rbind(matchups,temp)
    
  }
  
  matchups$t <- rep(1:n,each=r)
  return(matchups)
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


# Simulates games according to the model described in Glickman, 2001,
# Dynamic Paired-Comparison Models with Stochastic Variances.
#
# Returns a list containing a matrix of the skills over time (each round
# is a column and each player is a row) and a dataframe of the games
# 
# To get Glickman's model, set skill=runif(m,0,omega) for some reasonable
# omega, and pass q as the scaling parameter.
# 
# To get Glickman's model from the 1995 paper, set skillVar to
# be nu-squared repeated m times and tau to be zero.
MakeGlickmanGames <- function(skill,
                              skillVar,
                              tau,
                              games,
                              prob=GetBTProb,
                              orderEffect=0,
                              scaling=1,
                              ...) {
  m <- length(skill)
  nRounds <- max(games$t)
  skillMat <- NULL
  games$results <- rep(NA,length(games$i))
  #print(games)
  #print("Entering for loop")
  for(i in 1:nRounds) {
    #print(i)
    skillMat <- rbind(skillMat,
                      matrix(rep(skill,sum(games$t==i)),
                             ncol=length(skill),
                             byrow=TRUE))
    
    temp <- MakeGames(prob(skill,h=orderEffect,coeff=scaling,...),
                      games[which(games$t==i),])
    games$results[which(games$t==i)] <- temp$results
    
    skillVar <- rlnorm(m,log(skillVar),tau)
    skill <- rnorm(m,skill,sqrt(skillVar))
  }
  
  skillMat <- rbind(skillMat,skillMat[length(skillMat[,1]),])
  
  return(list(games,skillMat))
}


# Makes n tournaments between m players with matrix of
# win-loss probabilities prob. Each tournament works as
# follows: players are randomly assigned to games; the
# winners of one round progress to the next.
# 
# Be aware: currently no attempt is made to control which player is
# allowed to skip a round when the number of players is odd,
# meaning that by chance a player could skip every round except
# the final one (though with many players this is of course unlikely).
MakeTournament <- function(prob,n=1,byTournament = FALSE) {
  mTotal <- length(prob[1,])
  games <- NULL
  count <- 1
  
  for (k in 1:n) {
    players <- 1:mTotal
    
    while(length(players)>1) {
      m <- length(players)
      temp <- MakeGames(prob[players,players],
                        MatchAllPlayers(m,1)[1:floor(m/2),])
      tempPlayers <- players[-c(temp$results*temp$j,
                                (1-temp$results)*temp$i)]
      temp$i <- players[temp$i]
      temp$j <- players[temp$j]
      temp$t <- count

      players <- tempPlayers
      games<- rbind(games,temp)
      
      if (!byTournament) count <- count+1
    }
    
    if (byTournament) count <- count+1
  }
  return(games)
}

