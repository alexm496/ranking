#' g function. Defined as in Glickman 1995
g <- function(sigmaSquare,q) {
  return(1/sqrt(1+(3*sigmaSquare*q^2)/(pi^2)))
}

#' E function. Defined as in Glickman 1995
E <- function(mu,mu_j,sigmaSquare,q) {
  return((1+exp(-q*g(sigmaSquare,q)*(mu-mu_j)))^(-1))
}

#' RateGlicko function. 
#' @param games Dataframe containing games, with columns for 
#' player i, player j, and result
#' @param priorMeans Priors for means
#' @param priorVar Priors for variances
#' @param nu nu
#' @param q 
#' 
#' @return A list. The first entry is a matrix containing a 
#' row for the skill as of each round; the second is a matrix
#' containing a row for the variance.
RateGlicko <- function(games,priorMeans,priorVar,nu=0,q) {
  skillMat <- NULL
  varMat <- NULL
  
  means <- priorMeans
  variances <- priorVar
  
  for (t in 1:max(games$t)) {
    roundLength <- sum(games$t == t)
    
    newSkill <- matrix(rep(means,roundLength),
                       nrow=roundLength,
                       byrow=TRUE)
    newVar <- matrix(rep(variances,roundLength),
                     nrow=roundLength,
                     byrow=TRUE)
    
    varMat <- rbind(varMat,newVar)
    skillMat <- rbind(skillMat,newSkill)
    
    deltaSum <- rep(0,length(priorVar))
    updateSum <- rep(0,length(priorMeans))
    
    
    current <- games[which(games$t == t),]
    for (l in 1:length(current$i)) {
      i <- current$i[l]
      j <- current$j[l]

      updateSum[i] <- updateSum[i] + (g(variances[j],q)*
        (current$results[l]-E(means[i],means[j],variances[j],q)))
      deltaSum[i] <- deltaSum[i]+(g(variances[j],q))^2*E(means[i],means[j],variances[j],q)*
        (1-E(means[i],means[j],variances[j],q))
      
      updateSum[j] <- updateSum[j] + (g(variances[i],q)*
        (1-current$results[l]-E(means[j],means[i],variances[i],q)))
      deltaSum[j] <- deltaSum[j]+(g(variances[i],q))^2*E(means[j],means[i],variances[i],q)*
        (1-E(means[j],means[i],variances[i],q))
    }
    
    invDelta2 <- q^2 * deltaSum
    
    variances <- 1/(1/variances+invDelta2)
    means <- means+q*variances*updateSum
    
    variances <- variances + nu^2
    
  }
  
  varMat <- rbind(varMat,variances)
  skillMat <- rbind(skillMat,means)
  
    
  return(list(skillMat,varMat))
}
