ceuc.val <- function(mu, Xtst){
  sol <- matrix(nrow = nrow(Xtst))
  dist <- matrix(ncol = nrow(mu), nrow = nrow(Xtst))
  for(i in 1:nrow(Xtst)){
    for(j in 1:nrow(mu)){
      dist[i,j] <- sqrt(sum((Xtst[i,]-t(mu[j,]))^2))
    }
  }
  # choisir la colonne de la classe
  min <- apply(dist, 1, min)
  ntst <- vector('numeric', nrow(dist))
  for(i in 1:nrow(dist)){
    for(j in 1:ncol(dist)){
      if(min[i]==dist[i,j]){
        ntst[i] = j
      }
    }
  }
  ntst
  #apply(Xtxt, 1, apply, 1, mu, sqrt(sum()))
}
