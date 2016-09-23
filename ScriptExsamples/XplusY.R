XplusY <- function(X=1,Y=2){
  R <- X + Y
  R
}

XdividY <- function(X=1,Y=2){
  R <- X/Y
  R
}

New1 <- function(X1=1,Y1=2){
  Result <- XplusY(X=X1,Y=Y1) * XdividY(X=X1,Y=Y1)
  Result
}
