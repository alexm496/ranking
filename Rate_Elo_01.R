#' Elo rating function.
#' 
#' @param games Dataframe containing games (1 row each) with
#' columns for players i and j and a column for the results
#' @param prob Function to compute the probabilities.
#' @param init Initial ratings
#' @param k Learning rate
#' @param coeff Coefficient. Passed to prob.
#' @param decay k coefficient decays with time by a factor
#' of t^decay
#' 
#' @return Matrix, with each row containing ratings as of a
#' given game.
RateElo <- function(games,prob,init,k,coeff,decay=0) {
  skills <- init
  skillMat <- matrix(skills,nrow=1,byrow = TRUE)
  for(l in 1:length(games$i)) {
    kDecayed <- k/(l^decay)
    skills[games$i[l]] <- skills[games$i[l]]+
      kDecayed*(games$results[l]-prob(skills,coeff)[games$i[l],games$j[l]])
    skills[games$j[l]] <- skills[games$j[l]]+
      kDecayed*(-games$results[l]+prob(skills,coeff)[games$i[l],games$j[l]])
    skillMat <- rbind(skillMat,skills)
  }
  return(skillMat)
}


