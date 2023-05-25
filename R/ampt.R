library("dplyr")
library("locfit")


amp <- function(df){
  
  if (ncol(df) == 3){
    pat <- c(0, 1, 0)
    wei <- c(0, 1, 0)
  } else {
    pat <- c(0, 1, 1, 0)
    wei <- c(0, 1, 0, 0)
  }
  
  X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = wei, patterns = pat)$amp
  
  return(X_miss)
}

amp_45 <- function(df){
  
  if (ncol(df) == 3){
    pat <- c(0, 1, 0)
    wei <- c(0, 1, 0)
  } else {
    pat <- c(0, 1, 1, 0)
    wei <- c(0, 1, 0, 0)
  }
  
  X_miss = ampute(df, prop = 0.45, mech = "MAR", type = "RIGHT", weights = wei, patterns = pat)$amp
  
  return(X_miss)
}


amp_MNAR <- function(df){
  
  if (ncol(df) == 3){
    pat <- c(0, 1, 0) #("X", "Y", "X2")
    wei <- c(1, 0, 1)
  } else {
    pat <- c(0, 1, 1, 0) #("X", "Y", "Z", "XZ")
    wei <- c(1, 0, 0, 1)
  }
  
  X_miss = ampute(df, prop = 0.25, mech = "MAR", type = "RIGHT", weights = wei, patterns = pat)$amp
  
  return(X_miss)
}
  
