log.val = function(Xtst, beta) {

	#################### INIT
	Xtst = as.matrix(Xtst)
	beta = as.matrix(beta)
	if(ncol(beta)>nrow(Xtst)) {Xtst = as.matrix(cbind(rep(1,nrow(Xtst)),Xtst))}
    probabilites = matrix(0,nrow=nrow(Xtst),ncol=3)
	colnames(probabilites) = c("w1","w2","classement")
	
	#################### classement
	probabilites[,"w1"] = exp(Xtst%*%beta) / (1+exp(Xtst%*%beta))
	probabilites[,"w2"] = 1 - probabilites[,1]
	probabilites[,"classement"] = as.integer( probabilites[,"w1"] < probabilites[,"w2"] ) + 1

	return(probabilites)
}
