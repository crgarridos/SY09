#Produit d'une matrice X par sa transposée
prodtrans <- function(X) {
	temp<-t(A);
	result <-temp%*%A;
	return (result)
	}
	
#Calcul d'une matrice centrée en colonne à partir d'une matrice X
centre <- function(X) {
	
	temp = X;
	vect_moy = apply(X,2,mean);
	
	for(i in 1:dim(X)[2])
	{
		temp[,i] = temp[,i] - vect_moy[i];
		}
	
	return(temp)
	
	}
	
	
hist.factor <-function(var_quant,var_qual){
	
	
	
	
	
	}