kppv.val <- function(Xapp, zapp, K, Xtst)
{
  #La seconde fait le classement d’un tableau individus-variables Xtst : elle prend donc en argument
  #Xapp et zapp, le nombre de voisins K, et l’ensemble à évaluer Xtst; elle retourne donc un vecteur
  #(de longueur ntst) d’étiquettes prédites.
  dist <- matrix(ncol = nrow(Xtst), nrow = nrow(Xapp))
  for(i in 1:nrow(Xtst)){
    for(j in 1:nrow(Xapp)){
      dist[j,i] <- sqrt(sum((Xtst[i,]-t(Xapp[j,]))^2))
    }
  }
  tmp <- apply(dist, 2, order)
  tmp2 <- tmp
  for(i in 1:ncol(tmp)){
    tmp2[,i] <- zapp[tmp[,i]]
  }
  tmp2 <- tmp2[1:K,]
  round(apply(tmp2, 2, mean))
}