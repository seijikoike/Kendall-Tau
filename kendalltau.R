ken.tau <- function(data){
  n <- nrow(data)
  k <- choose(n,2)
  U <- rep(0,k)
  for(i in 1:k){
    compare <- combinations(n,2)[i,]
    x.diff <- data[compare[1],1] - data[compare[2],1]
    y.diff <- data[compare[1],2] - data[compare[2],2]
    if(x.diff * y.diff > 0){U[i] <- 1}
    else if(x.diff * y.diff == 0){U[i] <- 0.5}
  }
  V <- rep(0, n-1)
  for(j in 1:n){
    V[j] <- sum(U[combinations(n,2)[,1] == j])
  }
  tau <- (2 * sum(V)/k) - 1
  return(tau)
}

moisture <- c(355, 370, 380, 380, 380, 400, 400, 415,415, 415, 415, 430, 440, 440, 470)
shrew <- c(.5, 2, 2, .5, 3, 0, 4, .5, 2, 4, 4, 1, 2, 5, 7)

df <- data.frame(moisture, shrew)

ken.tau(df)