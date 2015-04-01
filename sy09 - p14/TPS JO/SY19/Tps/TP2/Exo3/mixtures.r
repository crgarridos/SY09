
# modèles de mélange en 1D 
gmixtmono <- function(donnees, K, param=NULL, fCEM=FALSE)
{
	# modèle de mélange gaussien unidimensionnel 
	if (is.vector(donnees))
	{
		n <- length(donnees)

		# initialisation arbitraire des paramètres 
		if (is.null(param))
		{
			for (k in 1:K)
			{
				mu_k <- mean(donnees[round((k-1)*n/K+1):round(k*n/K)])
				Sigma_k <- 1
				# Sigma_k <- var(donnees[round((k-1)*n/K+1):round(k*n/K)])

				param_k <- list(mu = mu_k, Sigma = Sigma_k)
				param[[k]] <- param_k
			}
		}

		# tant que le point de convergence 
		# n'est pas atteint 
		while()
		{
			# étape E 
			
			# étape C 
			if (fCEM)
			{
			}
			
			# étape M 
			
		}
	}

	return(param)
}

# modèles de mélange dans R^p 
gmixtmulti <- function(donnees, K, param=NULL, fCEM=FALSE)
{
	# modèle de mélange gaussien multidimensionnel 
	if (is.matrix(donnees))
	{
		n <- dim(donnees)[1]
		p <- dim(donnees)[2]
		mt <- matrix( nrow = n, ncol = K )
		mz <- matrix( nrow = n, ncol = K )
		resultat <- mat.or.vec(n, 1)
		epsilon <- 10^(-6)
		L <- 0
		nL <- epsilon 
		nbiteration <- 0
		
		# initialisation arbitraire des paramètres 
		if (is.null(param))
		{
			for (k in 1:K)
			{
				mu_k <- apply(donnees[round((k-1)*n/K+1):round(k*n/K),], 2, mean)
				# Sigma_k <- diag(rep(1,p))
				Sigma_k <- cov(donnees[round((k-1)*n/K+1):round(k*n/K),])
				pi_k <- 1/K

				param_k <- list(mu = mu_k, Sigma = Sigma_k, Pi = pi_k)
				param[[k]] <- param_k
			}
		}
		
		# tant que le point de convergence 
		# n'est pas atteint 
		while( abs(nL - L) >= epsilon && nbiteration < 4 )
		{
			# étape E 
			
			L <- nL
			
			somme <- param[[1]]$Pi * mvdnorm( donnees, param[[1]]$mu, param[[1]]$Sigma )
			
			for (l in 2:K)
			{
				somme <- ( param[[l]]$Pi * mvdnorm( donnees, param[[l]]$mu, param[[l]]$Sigma ) ) + somme
			}
			
			for (k in 1:K)
			{
				mt[,k] <- ( param[[k]]$Pi * mvdnorm( donnees, param[[k]]$mu, param[[k]]$Sigma ) ) / somme
			}
			
			# étape C 
			if (fCEM)
			{			
				for (i in 1:n)
				{	
					maxLineValue <- mt[i,1]
					maxLineInd <- 1
					for (k in 2:K)
					{
						if ( mt[i,k] > maxLineValue ) 
						{
							maxLineValue <- mt[i,k]
							maxLineInd <- k
						}
					}					
					for (k in 1:K)
					{
						if ( k == maxLineInd ) 
						{
							mz[i,k] <- 1
						}
						else
						{
							mz[i,k] <- 0
						}
					}
				}
				mt <- mz
			}
			
			# étape M			
			for (k in 1:K)
			{
				# Pi
				pi_k <- sum( mt[,k] ) / n	
				
				# Mu
				somme = mt[1,k] * donnees[1,]
				for (i in 2:n)
				{
					somme <- mt[i,k] * donnees[i,] + somme
				}
				mu_k <- somme / sum( mt[,k] )
				
				# Sigma
				somme = mt[1,k] * ( donnees[1,] - mu_k ) %*% t( donnees[1,] - mu_k )
				for (i in 2:n)
				{
					somme <- mt[i,k] * ( donnees[i,] - mu_k ) %*% t( donnees[i,] - mu_k ) + somme
				}
				Sigma_k <- somme / sum( mt[,k] )
				
				# On met à jour
				param_k <- list(mu = mu_k, Sigma = Sigma_k, Pi = pi_k)
				param_k
				param[[k]] <- param_k
			}
			
			# Calcul de la nouvelle vraissemblance
			nL = 0
			for( k in 1:K )
			{
				nL = nL + log(sum( param[[k]]$Pi * abs( mvdnorm(donnees, param[[k]]$mu, param[[k]]$Sigma) ) ))
			}
			
			nbiteration <- nbiteration + 1
		}
	}
	
	for (i in 1:n)
	{	
		maxLineValue <- mt[i,1]
		maxLineInd <- 1
		for (k in 2:K)
		{
			if ( mt[i,k] > maxLineValue ) 
			{
				maxLineValue <- mt[i,k]
				maxLineInd <- k
			}
		}		
		resultat[i] <- maxLineInd
	}
	return( list( resultat = resultat, nbiteration = nbiteration, nL = nL ) )
}

cCEM4 <- gmixtmulti(as.matrix(crabsquant), 4)