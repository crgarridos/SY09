# modèles de mélange en 1D , K nombre de composants du modèle
gmixtmono <- function(donnees, K, param=NULL, fCEM=FALSE)
{
	# modèle de mélange gaussien unidimensionnel 
	if (is.vector(donnees))
	{
		# initialisation des varibles locales
		n <- length(donnees)
		t_ik <- matrix(NA, n, K)
		c_ik <- matrix(NA, n, K)
		fk <- matrix(NA, n)	
		epsilon <- 10^(-10)
		L <- 0
		nL <- epsilon 
		nbiteration <- 0

		# initialisation arbitraire des paramètres 
		if (is.null(param))
		{
			for( k in 1:K )
			{
				pi_k <- 1/K
				mu_k <- mean( donnees[round((k-1)*n/K+1) : round(k*n/K)] )
				sigma_k <- var( donnees[round((k-1)*n/K+1) : round(k*n/K)] )
				
				param[[k]] <- list(p = pi_k, mu = mu_k, sigma = sigma_k)
			}
		}
		
		# tant que le point de convergence n'est pas atteint
		while( abs(nL - L) >= epsilon && nbiteration < 70 )
		{
			nbiteration = nbiteration + 1
			L <- nL
		
			# étape E 
			for( i in 1:n )
			{
				for( k in 1:K )
					t_ik[i,k] <- param[[k]]$p * dnorm(donnees[i], param[[k]]$mu, param[[k]]$sigma)
					
				t_ik[i,] = t_ik[i,] / sum(t_ik[i,])
			}
				
			# étape C 
			if( fCEM )
			{
				for( i in 1:n )
				{
					maxi <- t_ik[i,1]
					for( k in 1:K )
					{
						c_ik[i,k] <- 0
						if( t_ik[i,k] > maxi )
						{ 
							maxi <- t_ik[i,k]
							classe <- k
						}
					}
					c_ik[i,classe] <- 1
				}
			}
			else
				c_ik <- t_ik
			
			# étape M 
			for( k in 1:K )
			{
				pi_k <- 1 / n * sum(c_ik[,k])
				mu_k <- ( t(c_ik[,k]) %*% donnees ) / sum(c_ik[,k])
				sigma_k <- sqrt( ( t(c_ik[,k]) %*% (donnees - mu_k)^2 ) / sum(c_ik[,k]) )
				
				param[[k]] <- list(p = pi_k, mu = as.numeric(mu_k), sigma = as.numeric(sigma_k))
			}
			
			# Calcul de la nouvelle vraissemblance
			nL = 0
			for( i in 1:n )
			{
				fk[i] = 0
				for( k in 1:K )
					fk[i] = fk[i] + param[[k]]$p * dnorm(donnees[i], param[[k]]$mu, param[[k]]$sigma)
					
				nL = nL + log(fk[i])
			}
		}
		param[[K+1]] <- nbiteration	
		return(param)
	}
}





# modèles de mélange dans R^p 
gmixtmulti <- function(donnees, K, param=NULL, fCEM=FALSE)
{
	# modèle de mélange gaussien multidimensionnel 
	if (is.matrix(donnees))
	{
		n <- dim(donnees)[1]
		p <- dim(donnees)[2]

		# initialisation arbitraire des paramètres 
		if (is.null(param))
		{
			for (k in 1:K)
			{
				mu_k <- apply(donnees[round((k-1)*n/K+1):round(k*n/K),], 2, mean)
				#Sigma_k <- diag(rep(1,p))
				sigma_k <- cov(donnees[round((k-1)*n/K+1):round(k*n/K),])

				param_k <- list(mu = mu_k, sigma = sigma_k)
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