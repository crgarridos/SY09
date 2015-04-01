mu1 <-

simul<-function(n, pi, mu1, mu2, Sigma1, Sigma2) {

	
	k<-rbinom(1,n,pi);
	mat <- matrix(data = 0, nrow = n, ncol = 3);
	vec1 <-matrix(data = 0, nrow = k, ncol = 3);
	vec2 <-matrix(data = 0, nrow = n-k, ncol = 3);
	temp <- 1:k;
	temp[] = 1;
	vec1 <- cbind(mvrnorm(k, mu1, Sigma1),temp);
	temp <- 1:(n-k);
	temp[] = 2;
	vec2 <- cbind(mvrnorm(n-k, mu2, Sigma2),temp);
	mat<- rbind(vec1,vec2);
	
	return(mat);
}

attach(mtcars)
par(mfrow=c(2,3))

mu1 <- c(0,0)
mu2 <- c(10,0)

a1 = 1;
a2 = 1;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 1")

a1 = 1;
a2 = 6;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 2")

a1 = 1;
a2 = 9;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 3")

a1 = 5;
a2 = 5;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 4")

a1 = 10;
a2 = 10;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 5")

regleEuclidienne<-function(x,mu1,mu2){
	d1 = sqrt((x[1]-mu1[1])^2+(x[2]-mu1[2])^2);
	d2 = sqrt((x[1]-mu2[1])^2+(x[2]-mu2[2])^2);
	cls = (d1>d2) +1
		
	return (cls);
	
	}
	
erreurEstimee = function(ech1,ech2,regle,mu1,mu2){
	
	n=dim(ech1)[1] + dim(ech2)[1];
	classement1 = apply(ech1,1,regle,mu1,mu2);
	classement2 = apply(ech2,1,regle,mu1,mu2); 
	prob = sum((classement1==2),(classement2==1))/n;
	return(prob)
	}
	
	
a1 = 10;
a2 = 10;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
k1 = dim(ech1)[1]
ech2 = subset(mat,mat[,3]==2)
k2 = dim(ech2)[1]

mu1 <- apply (ech1[1:k1/2,-3],2,mean)
mu2 <- apply (ech2[1:k2/2,-3],2,mean)

test <- erreurEstimee(ech1[(k1/2)+1:k1,],ech2[(k2/2)+1:nk2,], regleEuclidienne,mu1,mu2);

a1 = 10;
a2 = 10;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
k1 = dim(ech1)[1]
ech2 = subset(mat,mat[,3]==2)
k2 = dim(ech2)[1]
mu1 <- apply (ech1[1:k1/2,-3],2,mean)
mu2 <- apply (ech2[1:k2/2,-3],2,mean)

boucle <- function(a,b)
{
	res = matrix (nrow=1, ncol=10)
	for (i in 1:10)
	{
		a1 = a;
		a2 = b;
		Sigma1 = a1*diag(2);
		Sigma2 = a2*diag(2);
		mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2)
		ech1 = subset(mat, mat[,3] == 1);
		k1 = dim(ech1)[1]
		ech2 = subset(mat,mat[,3]==2)
		k2 = dim(ech2)[1]
		mu1 <- apply (ech1[1:k1/2,-3],2,mean)
		mu2 <- apply (ech2[1:k2/2,-3],2,mean)
		test <- erreurEstimee(ech1[((k1/2)+1):k1,],ech2[((k2/2)+1):k2,], regleEuclidienne,mu1,mu2);
		res[i] <- test
	}
	return(res)
}

moy <- boucle()


boucle1 <- function(n)
{
		Sigma1 = diag(2);
		Sigma2 = diag(2);
		mat = simul(n,0.5,mu1, mu2, Sigma1, Sigma2);
		ech1 = subset(mat, mat[,3] == 1);
		k1 = dim(ech1)[1];
		ech2 = subset(mat,mat[,3]==2);
		k2 = dim(ech2)[1];
		mu1 <- apply (ech1[1:k1/2,-3],2,mean);
		mu2 <- apply (ech2[1:k2/2,-3],2,mean);
		res$mu1<- mu1;
		res$mu2 <- mu2;
	}
	return(res)
}

mu1 <-c(-1,0)
mu2 <-c(1,0)
mat = simul(600,0.5,mu1, mu2, Sigma1, Sigma2);
ech1 = subset(mat, mat[,3] == 1);
ech2 = subset(mat,mat[,3]==2);
mu11 <- apply (ech1,2,mean);
mu22 <- apply (ech2,2,mean);
var(ech1[,-3]);
var(ech2[,-3]);


plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Visualisation des frontières", xlab="x1", ylab = "x2")
abline(v=0,col="yellow")
abline(v=-1/2 * log(10), col="green")

n=dim(ech1)[1] + dim(ech2)[1];
classement1 = apply(ech1,1,regleEuclidienne,mu1,mu2)
classement2 = apply(ech2,1,regleEuclidienne,mu1,mu2) 
alpha = sum((classement1==2),(classement2==1))/n;
return(prob)
