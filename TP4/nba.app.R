nba.app <- function(Xapp,zapp){
    parametres = list()
    
    for(i in 1:2){
      Xapp_i = Xapp[zapp==i,]
      
      pi = sum(zapp == i) / length(zapp)
      mu = colMeans(Xapp_i)
      sigma = diag(diag(var(Xapp_i) * nrow(Xapp_i)/(nrow(Xapp_i)-1)))
      
      parametres[[i]] = list(pi=pi,mu=mu,sigma=sigma)
    }
    
    return(parametres)
  }