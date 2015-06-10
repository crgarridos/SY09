log.app = function(Xapp, zapp, intr, epsi) {

	############ INIT
	if(intr==0) {Xapp = as.matrix(Xapp)} else {Xapp = as.matrix(cbind(Xapp, rep(1,nrow(Xapp))))}
	dim = ncol(Xapp)
	w = matrix(0, nrow=dim)
	w_prec = matrix(1000, nrow=dim)
	t = as.integer(zapp == 1)
	q = 0
	
	while( sqrt(sum((w - w_prec)^2)) > epsi ) { 
		############ pq
		wx = Xapp %*% w
		pq = exp(wx) / (1+exp(wx))
		print(w)
		print(pq)
		############ gradient_Lw
		gradient_Lw = t(Xapp) %*% (t-pq)

		############ Wq
		Wq = diag(as.numeric(pq*(1-pq)))
		
		############ Hq
		print(Wq)
		Hq = t(Xapp) %*% Wq %*% Xapp
		
		############ iteration
		w_prec = w
		print(solve(Hq))
		
		w = w - solve(Hq) %*% gradient_Lw

		print("")

	}
	
	w
}

