aftd <- function(D){
  D = as.matrix(D)
  D = D^2
  Q = diag(nrow(D))-matrix(1, nrow(D), nrow(D))/nrow(D)
  W = -1/2*Q%*%D%*%Q
  V = eigen(W/nrow(W))$values
  L = eigen(W/nrow(W))$vectors
  L = sqrt(L)
  L[ is.nan(L) ] <- 0
  L = diag(nrow(D))*L  
  C = V%*%L
  plot(C)
}
