#Parametres
N1 = 100;
N2 = 100;

means1 = c(-1,-1)
means2 = c(-1,0.5)

covariance1 = array(0,c(2,2))
covariances1 <- array(c(1,.85,.85,1),c(2,2))

covariances2 = array(0,c(2,2))
covariances2 <- array(c(1,.85,.85,1),c(2,2))

g <- function(x)
{
	 return( ( logistic_mod$coefficients[1] + logistic_mod$coefficients[2]*x[,1] + logistic_mod$coefficients[3] * x[,2] ) > 0.5 )
}

# estimation de la probabilité d'erreur sur l'ensemble d'apprentissage et l'ensemble test
erreur <- function(x)
{
	return( sum( g( x[1:N1,] ) == 1 ) / N1 + sum( g( x[N1+1:N2,] ) == 0 ) / N2 )
}

# PLOT
plot(x[,1],x[,2])
points(x1[,1],x1[,2],col="green")
points(x2[,1],x2[,2],col="red")

# on met un -0.5 car par rapport à la droite cherchée, il faut comparer à 0.5
abline((logistic_mod$coefficients[1]-0.5)/(-logistic_mod$coefficients[3]), logistic_mod$coefficients[2]/(-logistic_mod$coefficients[3]))

# afficher les résultats

# LDA
print("LDA : ")
print(paste("Risque empirique = ",lda_error))
print(paste("Erreur de test = ",MLDA))
print("*****************")


# QDA
print("QDA : ")
print(paste("Risque empirique = ",qda_error))
print(paste("Erreur de test = ",MQDA))
print("*****************")


# Régression Logistique
print("Regression logistique : ")
print(paste("Risque empirique = ",logistic_error))
print(paste("Erreur de test = ",MGLM))
print("*****************")


# Boucle : 

bestModel <- function()
{
	MLDA = 0
	MQDA = 0
	MGLM = 0
	
	library(MASS)
	
	for( i in 1:50 )
	{
		#echantillon d'apprentissage
		x1 = mvrnorm(N1,means1,covariances1)
		x2 = mvrnorm(N2,means2,covariances2)
		
		x = rbind(x1,x2)
		y = rbind(array(1,c(N1,1)),array(2,c(N2,1))) 
		
		
		#echantillon de test
		x1test = mvrnorm(N1,means1,covariances1)
		x2test = mvrnorm(N2,means2,covariances2)
		xtest = rbind(x1test,x2test)
		
		
		# LDA
		lda_mod <- lda(x,y)
		# risque empirique
		prediction <- predict(lda_mod,x)$class
		lda_error <- mean(factor(y)!=prediction)
		# erreur de test
		prediction_test <- predict(lda_mod,xtest)$class
		lda_error_test <- mean(factor(y)!=prediction_test)
		MLDA <- MLDA + lda_error_test
		
		# QDA
		qda_mod <- qda(x,y)
		# risque empirique
		prediction <- predict(qda_mod,x)$class
		qda_error <- mean(factor(y)!=prediction)
		# erreur de test
		prediction_test <- predict(qda_mod,xtest)$class
		qda_error_test <- mean(factor(y)!=prediction_test)
		MQDA <- MQDA + qda_error_test
		
		
		# LOGISTIC
		z=cbind(y-1,x)# si on ne fait pas y-1, la fonction glm donne erreur car elle estime une proba
		colnames(z)=c("y","d1","d2")
		z=data.frame(z)
		 
		logistic_mod = glm(y ~ d1+d2,z,family=binomial)
			
		logistic_error = erreur(x)
		logistic_error_test = erreur(xtest)
		MGLM <- MGLM + logistic_error_test
	}
	
	MLDA = MLDA / 50
	MQDA = MQDA / 50
	MGLM = MGLM / 50
	
	# quel est le meilleur ?
	methodes <- c("LDA","QDA","Regression logistique")
	erreurs <- c(MLDA,MQDA,MGLM)
	print(paste("La meilleure methode est : ",methodes[erreurs==min(erreurs)][1]))
}
