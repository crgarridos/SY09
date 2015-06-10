adl.app <- function(Xapp,zapp){
  parametres = list()
  
  sigma = 0
  for(i in 1:2){
    Xapp_i = Xapp[zapp==i,]
    sigma = sigma + var(Xapp_i) * nrow(Xapp_i)
  }
  sigma = sigma / (nrow(Xapp) - 2)
  
  for(i in 1:2){
    pi = sum(zapp == i) / length(zapp)
    mu = colMeans(Xapp[zapp==i,])
    
    parametres[[i]] = list(pi=pi,mu=mu,sigma=sigma)
  }
  
  return(parametres)
}