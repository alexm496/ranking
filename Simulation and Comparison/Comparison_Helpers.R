#' CheckPerf function -- takes in games, skills, a rating function,
#' and a dataframe to fill in and fills in the dataframe with 
#' data on the average performance of the function at each round
#'
#' @param games List containing dataframes, each representing a
#' number of games
#' @param skills List containing skills. (Will accept either 
#' a vector containing the skills or a matrix, where each row is
#' the skill at a given round)
#' @param fill Dataframe to be filled. Should have as many rows
#' as each entry of games.
#' @param RATE The function whose performance is to be evaluate
#' @param ... Extra arguments to be passed to RATE
#' @param verbose Boolean. If true, the function prints progress
#' to the console every fifty iterations.
#' 
#' @return fill, with performance metrics added.
CheckPerf <- function(games,skills,fill,RATE,...,center=TRUE,verbose=TRUE) {
  if (length(games)==0) print("BAD PROBLEMS")
  #print(summary(skills))
  
  corMatTemp <- NULL
  RMSEMatTemp <- NULL
  rounds <- length(fill[,1])
  cors <- rep(0,rounds)
  
  #print("Hi")
  #print(skills[[1]])
  for (i in 1:length(skills)) {
    if (is.null(games[[i]])) print(paste("Game",i,"was null."))
    if (verbose & i%%20==1) print(i)
    
    if (is.matrix(skills[[i]])) skillMat <- skills[[i]]
    else {
      skillMat <- matrix(rep(skills[[i]],rounds),
                            nrow=rounds,
                            byrow=TRUE)
      #print("hi")
    }
    
    #print(dim(skillMat))
    
    temp <- RATE(games[[i]],...)
    
    if (!is.list(temp)) ratings <- temp
    else ratings <- temp[[1]]
    
    if (center) {
      players <- length(ratings[1,])
      #print(players)
      #print(length(ratings))
      #print(summary(rowMeans(ratings)))
      ratings <- ratings - matrix(rep(rowMeans(ratings),players),
                                  ncol =players,byrow = FALSE)
      skillMat <- skillMat - matrix(rep(rowMeans(skillMat),players),
                                  ncol=players,byrow = FALSE)
    }
    
    #print(dim(ratings))

    for (i in 1:rounds) cors[i] <- cor(ratings[i,],skillMat[i,])
    corMatTemp <- cbind(corMatTemp,cors)

    RMSE <- sqrt(rowMeans((ratings-skillMat)^2))
    RMSEMatTemp <- cbind(RMSEMatTemp,RMSE)
  }
  #print("Hello")
  #print(dim(fill))
  #print(dim(corMatTemp))
  fill$cors <- rowMeans(corMatTemp)
  fill$RMSE <- rowMeans(RMSEMatTemp)
  return(fill)
}

#' RMSE function.
GetRMSE <- function(x,y) return(sqrt(mean((x-y)^2)))
