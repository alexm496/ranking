#' RateTMExact function. Computes exact Thurstone-Mosteller updates
#' 
#' @param games Dataframe containing one row for each game
#' @param priorMeans
#' @param priorVar
#' @param beta2 When players i & j play, i's win probability is 
#' assumed to be the cdf of the normal RV w/ mean 0 and variance
#' beta2 evaluated at the difference in skills
RateTMExact <- function(games,priorMeans,priorVar,beta2,nu=0) {
  n <- length(games$i)
  
  skillMat <- matrix(rep(priorMeans,n+1),nrow=n+1,byrow = TRUE)
  varMat <- matrix(rep(priorVar,n+1),nrow=n+1,byrow=TRUE)

  for (l in 1:length(games[,1])) {
    if (games$results[l] == 1)  {
      winner <- games$i[l]
      loser <- games$j[l]
    } else {
      winner <- games$j[l]
      loser <- games$i [l]
    }
    
    sigmaT2 <- 2*beta2+varMat[l,winner]+varMat[l,loser]
    sigmaT <- sqrt(sigmaT2)
    phi <- dnorm((skillMat[l,winner]-skillMat[l,loser])/sqrt(sigmaT2))
    Phi <- pnorm((skillMat[l,winner]-skillMat[l,loser])/sqrt(sigmaT2))
    
    skillMat[l+1,] <- skillMat[l,]
    varMat[l+1,] <- varMat[l,]
    
    if (l>1) {
      if (games$t[l-1]<games$t[l]) varMat[l+1,] <- varMat[l+1,]+nu^2
    }
    
    skillMat[l+1,winner] <- skillMat[l,winner]+(varMat[l,winner]*phi)/(sigmaT*Phi)
    skillMat[l+1,loser] <- skillMat[l,loser]-(varMat[l,loser]*phi)/(sigmaT*Phi)

    varMat[l+1,winner] <- varMat[l,winner]+skillMat[l,winner]^2-skillMat[l+1,winner]^2+((phi*varMat[l,winner])/(Phi*sigmaT2^(3/2)))*
      (varMat[l,winner]*(skillMat[l,winner]+skillMat[l,loser])+2*(2*beta2+varMat[l,loser])*skillMat[l,winner])
    varMat[l+1,loser] <-  varMat[l,loser] +skillMat[l,loser]^2 -skillMat[l+1,loser]^2 -((phi*varMat[l,loser])/(Phi*sigmaT2^(3/2)))*(2*(varMat[l,winner]+2*beta2)*skillMat[l,loser]+
       varMat[l,loser]*(skillMat[l,loser]+skillMat[l,winner]))
  }
  
  return(list(skillMat,varMat))
  
}

