# RateTrueskill and associated functions. Written by Alex Mangiapane
# and slightly modified.

vfunc <- function(a, b){
  return(dnorm(a-b)/pnorm(a-b))
}


#No draws allowed w function
wfunc <- function(a, b){
  return(vfunc(a, b)*(vfunc(a, b) + a - b))
}


RateTrueskillDF <- function(games, p_data, eps = 0, beta = 300){
  games[games$results == 0, c("i", "j", "results")] <- games[games$results == 0, c("j", "i", "results")]
  games$results <- NULL
  #print(summary(games))
  #print(summary(p_data))
  for(n in 1:nrow(games)) {
    c = sqrt(2*beta^2 + p_data[games$i[n], 2]^2 + p_data[games$j[n], 2]^2)
    
    p_data[games$i[n], 1] = p_data[games$i[n], 1] + ((p_data[games$i[n], 2]^2)/c)*vfunc(((p_data[games$i[n], 1]) - (p_data[games$j[n], 1]))/c, eps/c)
    
    p_data[games$j[n], 1] = p_data[games$j[n], 1] - ((p_data[games$j[n], 2]^2)/c)*vfunc(((p_data[games$i[n], 1]) - (p_data[games$j[n], 1]))/c, eps/c)
    
    p_data[games$i[n], 2] = p_data[games$i[n], 2]*(1-(((p_data[games$i[n], 2])^2)/(c^2))*wfunc((p_data[games$i[n], 1] - p_data[games$j[n], 1])/c, eps/c))
    
    p_data[games$j[n], 2] = p_data[games$j[n], 2]*(1-(((p_data[games$j[n], 2])^2)/(c^2))*wfunc((p_data[games$i[n], 1] - p_data[games$j[n], 1])/c, eps/c))
    
    #euclid_n[n] = sum((p_data[, 1] - rating_means)^2)
    #print(sum((p_data[,1]-rating_means)^2))
  }
  return(p_data)
}



RateTrueskill <- function(games, p_data, eps = 0, beta = 300,nu=0){
  games[games$results == 0, c("i", "j", "results")] <- games[games$results == 0, c("j", "i", "results")]
  games$results <- NULL
  skillMat <- NULL
  varMat <- NULL
  #print(games$t)
  #print(summary(games))
  #print(summary(p_data))
  for(n in 1:nrow(games)) {
    #print(skillMat)
    skillMat <- rbind(skillMat, p_data[,1])
    varMat <- rbind(varMat, p_data[,2])
    if (n>1) {
      if (games$t[n-1]<games$t[n]) {
        #print(p_data[,2])
        p_data[,2] <- sqrt(p_data[,2]^2+nu^2)
        #print(p_data[,2])
      }
    }
    
    c = sqrt(2*beta^2 + p_data[games$i[n], 2]^2 + p_data[games$j[n], 2]^2)
    
    p_data[games$i[n], 1] = p_data[games$i[n], 1] + ((p_data[games$i[n], 2]^2)/c)*vfunc(((p_data[games$i[n], 1]) - (p_data[games$j[n], 1]))/c, eps/c)
    
    p_data[games$j[n], 1] = p_data[games$j[n], 1] - ((p_data[games$j[n], 2]^2)/c)*vfunc(((p_data[games$i[n], 1]) - (p_data[games$j[n], 1]))/c, eps/c)
    
    p_data[games$i[n], 2] = p_data[games$i[n], 2]*sqrt(1-(((p_data[games$i[n], 2])^2)/(c^2))*wfunc((p_data[games$i[n], 1] - p_data[games$j[n], 1])/c, eps/c))
    
    p_data[games$j[n], 2] = p_data[games$j[n], 2]*sqrt(1-(((p_data[games$j[n], 2])^2)/(c^2))*wfunc((p_data[games$i[n], 1] - p_data[games$j[n], 1])/c, eps/c))
    
    #euclid_n[n] = sum((p_data[, 1] - rating_means)^2)
    #print(sum((p_data[,1]-rating_means)^2))
  }
  skillMat <- rbind(skillMat, p_data[,1])
  varMat <- rbind(varMat, p_data[,2])
  
  return(list(skillMat,varMat))
}

