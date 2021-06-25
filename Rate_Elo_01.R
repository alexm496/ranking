

RateElo <- function(games,prob,init,k) {
  skills <- init
  for(l in 1:length(games$i)) {
#    print(games[l,])
    skills[games$i[l]] <- skills[games$i[l]]+k*(games$results[l]-prob(skills)[games$i[l],games$j[l]])
#    print(skills)
    skills[games$j[l]] <- skills[games$j[l]]+k*(-games$results[l]+prob(skills)[games$i[l],games$j[l]])
#    print(skills)
  }
  return(skills)
}

skillVec <- runif(25,0,15)
init <- rep(1,25)
ratings <- RateElo(MakeGames(GetBTProb(skillVec),MatchRandomly(25,10000)),
        GetBTProb,
        init,
        0.5)
cor(ratings,skillVec)

