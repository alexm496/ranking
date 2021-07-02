checkPerf <- function(games,skills,reps,fill,RATE,...) {
  corMatTemp <- NULL
  RMSEMatTemp <- NULL
  for(i in 1:reps) {
    skillVec <- skills[[i]]
    ratings <- RATE(games[[i]],...)
    cors <- apply(ratings,1,cor,y=skillVec)
    corMatTemp <- cbind(corMatTemp,cors)

    RMSE <- apply(ratings,1,GetRMSE,y=skillVec)
    RMSEMatTemp <- cbind(RMSEMatTemp,RMSE)
  }
  fill$cors <- apply(corMatTemp,1,mean)
  fill$RMSE <- apply(RMSEMatTemp,1,mean)
  return(fill)
}

GetRMSE <- function(x,y) return(sqrt(mean((x-y)^2)))

GetProbRMSE <- function(x,y,prob=GetBTProb,...) {
  return(GetRMSE(prob(x,...),prob(y,...)))
}
