log.app = function(Xapp, zapp, intr, epsi) {
    
    ############ INIT
    if(intr==0) {Xapp = as.matrix(Xapp)} else {Xapp = as.matrix(cbind(rep(1,nrow(Xapp)),Xapp))}
    dim = ncol(Xapp)
    w = matrix(0, nrow=dim)
    if(dim == ncol(Xapp)+1) {w[0] = intr}
    w_prec = matrix(1000, nrow=dim)
    t = as.integer(zapp == 1)
    q = 0
    niter = 0
    
    ########### loop
    while( sqrt(sum((w - w_prec)^2)) > epsi ) { 
        ############ pq
        wx = Xapp %*% w
        pq = exp(wx) / (1+exp(wx))

        ############ gradient_Lw
        gradient_Lw = t(Xapp) %*% (t-pq)
        
        ############ Wq
        Wq = diag(as.numeric(pq*(1-pq)))
        
        ############ Hq
        Hq = - t(Xapp) %*% Wq %*% Xapp
        
        ############ iteration
        w_prec = w
        w = w - solve(Hq) %*% gradient_Lw
	  niter = niter + 1
    }
    
    logL = sum(t*wx - log(1+exp(wx)))

    return(list(beta=w,niter=niter,logL=logL))
}
