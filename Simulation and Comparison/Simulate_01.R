library(dplyr)

# This .R file is laid out as follows: the first set of 
# functions take in a vector of skills and output a matrix
# of pairwise win-loss probabilities; the second set of 
# functions take in a number of players (and sometimes
# other parameters) and output a schedule of games (in the
# form of a dataframe with columns for which player is player
# i and which is player j and a row for each game); then the
# third set take a schedule of games (and usually a matrix of
# pairwise win/loss probabilities) and simulate the games.


###################################
###   Functions for computing   ###
###         probabilities       ###
###################################

#' Function for computing Bradley-Terry probabilities.
#'
#' @param skills Vector of skills
#' @param coeff c variable. Setting coeff=ln10/400 yields 
#' the model generally used in chess, which is also the
#' model used by Glickman.
#' @param h Home team advantage or order effect
GetBTProb <- function(skills, coeff=1,h=0) {
  n <- length(skills)
  skillMat <- matrix(rep(skills,n),nrow=n,byrow=TRUE)
  probMat <- (1+exp(coeff*(skillMat-t(skillMat)-h)))^(-1)
  return(probMat)
}

#' Get player i's Bradley-Terry win probability when player i
#' competes against player j.
#' 
#' @param skill1 Player i's skill
#' @param skill2 Player j's skill
#' @param coeff c variable. Setting coeff=ln10/400 yields 
#' the model generally used in chess, which is also the
#' model used by Glickman.
#' @param h Home team advantage or order effect
GetBTProbSingle <- function(skill1,skill2,coeff=1,h=0) {
  return ((1+exp(coeff*(skill2-skill1-h)))^(-1))
}

#' Function for computing Thurstone-Mosteller probabilities.
#' @param skills Vector of skills
#' @param coeff Variance of the normal cdf used to compute
#' win/loss probabilities
#' @param h Home team advantage or order effect
GetTMProb <- function(skills, coeff=1,h=0) {
  n <- length(skills)
  probMat <- matrix(0,n,n)
  for(j in 1:n) probMat[,j] <- pnorm(skills,
                                     mean = skills[j]-h,
                                     sd=1/coeff)
  return(probMat)
}

#' Function for computing "probabilities" by a step function
#' 
#' @param skills Vector of skills
#' @param h Order effect
GetStepProb <- function(skills,h=0) {
  skillMat <- matrix(rep(skills,length(skills)),nrow=length(skills),byrow=TRUE)
  probMat <- (skillMat<t(skillMat)+h) + 0.5*(skillMat == t(skillMat)+h)
  return(probMat)
}


####################################
###   Functions for scheduling   ###
###            games             ###
####################################

#' Chooses a player for each game unif. at random, then
#' chooses another play for them to play against unif.
#' at random.
#'
#' @param m Number of players
#' @param n Number of games
#' @param roundLength controls time, with each round of length
#' roundLength (except the last round, which may be shorter). 
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


#' Every possible pair of players, n times.
#' 
#' @param m Number of players
#' @param n Number of games
#' @param randomize If TRUE, randomize the order of pairs within
#' a round.
#'
MatchAllPairs <- function(m,n=1,randomize=FALSE) {
  # I suspect there's a one-line way to do this, but
  # I couldn't find it.
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

#' Matches players by skill.
#' 
#' @skills Vector of player skills
#' @param df Degrees of freedom for the chisq random variable
#' used to determine the number of ranks separating a player
#' and the player they're matched with and thus introduce 
#' randomness. Setting to 0 yields deterministic matchmaking
#' @param closest If TRUE, matches players with similar skill;
#' otherwise, matches dissimilar ones
#' @param n Number of "rounds." (Each "round," every player 
#' plays one game.)
#' @param randomize If TRUE, orders players randomly within rounds
#' 
#' @note Because of the way truncation is done, sufficiently high df
#' leads to almost-deterministic matchmaking with opposite
#' closest value.
MatchBySkill <- function(skills,df=0,closest=TRUE,n=1,randomize=FALSE) {
  m <- length(skills)
  r <- ceiling(m/2)
  matchups <- data.frame("i"=c(),"j"=c())
  
  for (l in 1:n) {
    temp <- data.frame("i"=c(rep(0,r)),
                       "j"=rep(0,r))
    options <- 1:m
    
    for (k in 1:floor(m/2)) {
      temp$i[k] <- options[1]
      options <- options[2:length(options)]
      
      # floor(rchisq(1,df))+1 is used rather than 
      # ceiling(rchisq(1,df)) so that df=0 produces 
      # deterministic matchmaking
      if (closest) temp$j[k] <- options[min(floor(rchisq(1,df))+1,
                                            length(options))]
      else temp$j[k] <- options[length(options)-min(floor(rchisq(1,df)),
                                                    length(options)+1)]
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


#' Simulates games
#' 
#' @param prob Matrix of pairwise win/loss probabilities,
#' such as output from GetBTProb, GetTMProb, etc
#' @param games Dataframe containing schedule of games, with a 
#' row for each game and columns for player i, player j, and t
#
# I tried to vectorize this function, but performance
# got worse. (In fact, the performance of this function
# was very strange -- I'm at a loss for why the code I
# have ran faster than other code I tried.)
MakeGames <- function(prob,games) {
  n <- length(games$i)
  games$p <- rep(0,length(games$i))
  for(k in 1:n) games$p[k] <- prob[games$i[k],games$j[k]]
  
  for(k in 1:n) {
    games$results[k] <- rbinom(1,1,prob[games$i[k],games$j[k]])
  }
  
  return(games)
}


#' Simulates games according to the model described in Glickman, 2001,
#' Dynamic Paired-Comparison Models with Stochastic Variances.
#'
#' Returns a list containing a matrix of the skills over time (each round
#' is a column and each player is a row) and a dataframe of the games
#' 
#' @param skill Vector of initial skills
#' @param skillVar Vector of initial skill variances
#' @param tau Defined as in Glickman's paper
#' @param games Dataframe containing schedule of games, with
#' one row for each game and columns for player i, player j, and t
#' @prob Function for computing probability matrices from skills.
#' Defaults to GetBTProb.
#' @param orderEffect h parameter, to be passed to PROB
#' @param scaling Scale parameter, to be passed to PROB
#' @param ... Additional parameters, to be passed to PROB
#' 
#' @note 
#' To get Glickman's model, set skill=runif(m,0,omega) for some reasonable
#' omega, and pass q as the scaling parameter.
#' 
#' To get Glickman's model from the 1995 paper, set skillVar to
#' be nu-squared repeated m times and tau to be zero.
MakeGlickmanGames <- function(skill,
                              skillVar,
                              tau,
                              games,
                              PROB=GetBTProb,
                              orderEffect=0,
                              scaling=1,
                              ...) {
  m <- length(skill)
  nRounds <- max(games$t)
  skillMat <- NULL
  games$results <- rep(NA,length(games$i))
  for(i in 1:nRounds) {
    skillMat <- rbind(skillMat,
                      matrix(rep(skill,sum(games$t==i)),
                             ncol=length(skill),
                             byrow=TRUE))
    
    temp <- MakeGames(PROB(skill,h=orderEffect,coeff=scaling,...),
                      games[which(games$t==i),])
    games$results[which(games$t==i)] <- temp$results
    
    skillVar <- rlnorm(m,log(skillVar),tau)
    skill <- rnorm(m,skill,sqrt(skillVar))
  }
  
  skillMat <- rbind(skillMat,skillMat[length(skillMat[,1]),])
  
  return(list(games,skillMat))
}


#' Makes n tournaments between m players with matrix of
#' win-loss probabilities prob. Each tournament works as
#' follows: players are randomly assigned to games; the
#' winners of one round progress to the next.
#'
#' @param prob The pairwise win probability matrix of the 
#' players
#' @param n The number of tournaments to simulate
#' @param byTournament If FALSE, t counts the number of games; 
#' if TRUE, t counts the number of tournaments
#' 
#' @return A dataframe containing one row for each player, with columns for player i, player j, t, and who won
#' 
#' Be aware: currently no attempt is made to control which player is
#' allowed to skip a round when the number of players is odd,
#' meaning that by chance a player could skip every round except
#' the final one (though with many players this is of course unlikely).
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

