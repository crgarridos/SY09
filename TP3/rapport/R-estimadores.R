estimateurAlpha <- function(data){
  f<-function(row) {
    if(row[3] == 1 && row[1] > -0.5) 
    { 
      return(1) 
      } 
    return(0) 
    }
  #resultados
  sum(apply(data, 1,f )) / sum(data[,3] == 1)
}
estimateurBeta <- function(data){
  f <- function(row) {
    if(row[3] == 2 && row[1] < -0.5) 
      { 
      return(1) 
      } 
    return(0) 
    }
  #resultados
  sum(apply(data, 1, f)) / sum(data[,3] == 1)
}
estimateurAlpha(synth1)
estimateurBeta(synth1)

estimateurAlpha(synth100)
estimateurBeta(synth100)

estimateurAlpha(synth500)
estimateurBeta(synth500)

estimateurAlpha(synth1000)
estimateurBeta(synth1000)

load("C:/Users/kefreng/Desktop/sy09/TP3/data.RData")
par(mfrow=c(2,2))
plot(synth1[,2] ~ synth1[,1], col=c("green","blue")[synth1[,3]], xlab="X2",ylab="X1")
abline(v=-0.5, col = "black")
title("Data Synth1 40")

plot(synth100[,2] ~ synth100[,1], col=c("green","blue")[synth100[,3]], xlab="X2",ylab="X1")
abline(v= -1/2, col = "black")
title("Data Synth1 100")

plot(synth500[,2] ~ synth500[,1], col=c("green","blue")[synth500[,3]], xlab="X2",ylab="X1")
abline(v = -1/2, col = "black")
title("Data Synth1 500")

plot(synth1000[,2] ~ synth1000[,1], col=c("green","blue")[synth1000[,3]], xlab="X2",ylab="X1")
abline(v = -1/2, col = "black")
title("Data Synth1 1000")

data
