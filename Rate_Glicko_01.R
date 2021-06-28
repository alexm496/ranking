g <- function(sigmaSquare,q) {
  return(1/sqrt(1+(3*sigmaSquare*q^2)/(pi^2)))
}

E <- function(mu,mu_j,sigmaSquare,q) {
  return((1+exp(-q*g(sigmaSquare,q)*(mu-mu_j)))^(-1))
}


RateGlicko <- function(games,priorMeans,priorVar,nu,q) {
  skillMat <- matrix(priorMeans,nrow=1)
  #print(skillMat)
  varMat <- matrix(priorVar,nrow=1)
  #print(varMat)
  
  for (t in 1:max(games$t)) {
    deltaSum <- rep(0,length(priorVar))
    updateSum <- rep(0,length(priorMeans))
    
    current <- games[which(games$t == t),]
    #print(current)
    for (l in 1:length(current$i)) {
      i <- current$i[l]
      j <- current$j[l]

      updateSum[i] <- updateSum[i] + (g(varMat[t,j],q)*
        (current$results[l]-E(skillMat[t,i],skillMat[t,j],varMat[t,j],q)))
      deltaSum[i] <- deltaSum[i]+(g(varMat[t,j],q))^2*E(skillMat[t,i],skillMat[t,j],varMat[t,j],q)*
        (1-E(skillMat[t,i],skillMat[t,j],varMat[t,j],q))
      
      updateSum[j] <- updateSum[j] + (g(varMat[t,i],q)*
        (1-current$results[l]-E(skillMat[t,j],skillMat[t,i],varMat[t,i],q)))
      deltaSum[j] <- deltaSum[j]+(g(varMat[t,i],q))^2*E(skillMat[t,j],skillMat[t,i],varMat[t,i],q)*
        (1-E(skillMat[t,j],skillMat[t,i],varMat[t,i],q))
      
      
    }
    
    #print(updateSum)
    #print(deltaSum)
    
    invDelta2 <- q^2 * deltaSum
    
    variances <- 1/(1/varMat[t,]+invDelta2)
    means <- skillMat[t,]+q*variances*updateSum
    
    variances <- variances + nu^2
    
    varMat <- rbind(varMat,variances)
    skillMat <- rbind(skillMat,means)
  }
  
  return(list(skillMat,varMat))
}

skillVec <- rnorm(20,1500,200)
init <- rep(1500,20)
initVar <- rep(200^2,20)
ratings <- RateGlicko(MakeGames(GetBTProb(skillVec,log(10)/400),MatchRandomly(m=20,n=800,roundLength = 100)),
                   priorMeans = init,
                   priorVar=initVar,
                   nu=10,
                   q=log(10)/400)
means <- ratings[[1]]
variances <- ratings[[2]]
variances[9,]
means[9,]

cor(means[9,],skillVec)
