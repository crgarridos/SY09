adq.app <- function(Xapp,zapp){
  res = c()
  n = nrow(Xapp)
  for(g in 1:2){
    Xg = Xapp[zapp==g,]
    elem = NULL 
    elem$pi =  sum(zapp == g) / n
    elem$mu = apply(Xg,2,mean); #colMeans(Xapp_i)
    elem$sigma = var(Xg) * n/(n-1)
    res = rbind(res,elem);
  }
  res
}