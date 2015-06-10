ad.val <- function(parametres, Xtst){
    res = matrix(0, nrow=nrow(Xtst), ncol=3)
    f_k = matrix(0, nrow=nrow(Xtst), ncol=2)
    
    for(i in 1:2){
      mu = parametres[[i]]$mu
      sigma = parametres[[i]]$sigma 
      f_k[,i] = mvdnorm(Xtst,mu,sigma)
    }
    
    for(i in 1:2){
      res[,i] = (f_k[,i]*parametres[[i]]$pi) / (f_k[,1]*parametres[[1]]$pi + f_k[,2]*parametres[[2]]$pi)
    }
    res[,3] = max.col(res)
    
    return(res)
  }