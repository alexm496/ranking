#' Elo rating function.
#' 
#' @param games Dataframe containing games (1 row each) with
#' columns for players i and j and a column for the results
#' @param PROB Function to compute the probabilities. Should
#' take in two skills and optionally other parameters and return
#' a pairwise win/loss probability.
#' @param init Initial ratings
#' @param k Learning rate
#' @param coeff Coefficient. Passed to prob.
#' @param decay k coefficient decays with time by a factor
#' of 1/(t^decay)
#' @param ... Additional parameters to be passed to PROB.
#' 
#' @return Matrix, with each row containing ratings as of a
#' given game.
RateElo <- function(games,PROB,init,k,coeff,decay=0,...) {
  skills <- init
  skillMat <- matrix(skills,nrow=1,byrow = TRUE)
  for(l in 1:length(games$i)) {
    kDecayed <- k/(l^decay)
    iSkill <- skills[games$i[l]]
    jSkill <- skills[games$j[l]]
    p <- PROB(iSkill,jSkill,coeff,...)
    skills[games$i[l]] <- iSkill+kDecayed*(games$results[l]-p)
    skills[games$j[l]] <- jSkill+kDecayed*(p-games$results[l])
    skillMat <- rbind(skillMat,skills)
  }
  return(skillMat)
}

