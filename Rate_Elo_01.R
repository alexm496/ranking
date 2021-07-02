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


