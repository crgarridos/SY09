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
	
	attach(mtcars)
par(mfrow=c(2,3))


mu1<-c(0,0)
mu2<-c(1,1)
a1 = 1;
a2 = 1;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(1000,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 1")
curve(1-x,add=TRUE)

mu1<-c(0,0)
mu2<-c(1,1)
a1 = 1;
a2 = 1;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(1000,0.1,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 2")
curve(-x-1.2,add=TRUE)


mu1<-c(0,0)
mu2<-c(1,1)
Sigma1 = matrix(c(1,-0.3,-0.3,1), nrow=2, byrow=T)
Sigma2 = matrix(c(1,-0.3,-0.3,1), nrow=2, byrow=T)
mat = simul(1000,0.5,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 3")
curve(1-x,add=TRUE)


mu1<-c(1,1)
mu2<-c(1,1)
a1 = 1;
a2 = 5;
Sigma1 = a1*diag(2);
Sigma2 = a2*diag(2);
mat = simul(1000,0.6,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 4")


mu1<-c(0,0)
mu2<-c(1,1)
a1 = 1;
Sigma1 = a1*diag(2);
Sigma2 = matrix(c(1,0.5,0.5,1), nrow=2, byrow=T)
mat = simul(1000,0.6,mu1, mu2, Sigma1, Sigma2)
plot(mat[,1], mat[,2], col=c("red","blue")[mat[,3]],xlab="x",ylab="y", main="Création d'un echantillon - jeu de données 5")
	
#JEU DE DONNEES 1 : 
mu1<- c(0,0);
mu2 <- c(1,1);
Sigma1 <- diag(2);
Sigma2 <- diag(2);
pi = 0.5;
mat = simul(1000,pi,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
ech2 = subset(mat,mat[,3]==2);
test <- erreurEstimee(ech1,ech2, regleEuclidienne,mu1,mu2);
test

#JEU DE DONNEES 2 : 
mu1<- c(0,0);
mu2 <- c(1,1);
Sigma1 <- diag(2);
Sigma2 <- diag(2);
pi = 0.1;
mat = simul(1000,pi,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
ech2 = subset(mat,mat[,3]==2);
test <- erreurEstimee(ech1,ech2, regleEuclidienne,mu1,mu2);
test

#JEU DE DONNEES 3 : 
mu1<- c(0,0);
mu2 <- c(1,1);
Sigma1 <- matrix(c(1,-0.3,-0.3,1), nrow = 2, ncol = 2);
Sigma2 <- Sigma1;
pi = 0.5;
mat = simul(1000,pi,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
ech2 = subset(mat,mat[,3]==2);
test <- erreurEstimee(ech1,ech2, regleEuclidienne,mu1,mu2);
test

#JEU DE DONNEES 4 : 
mu1<- c(1,1);
mu2 <- c(1,1);
Sigma1 <- diag(2);
Sigma2 <- diag(2)*5;
pi = 0.6;
mat = simul(1000,pi,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
ech2 = subset(mat,mat[,3]==2);
test <- erreurEstimee(ech1,ech2, regleEuclidienne,mu1,mu2);
test

#JEU DE DONNEES 5 :
mu1<- c(0,0);
mu2 <- c(1,1);
Sigma1 <- diag(2);
Sigma2 <- matrix(c(1,0.5,0.5,1), nrow = 2, ncol = 2);
pi = 0.6;
mat = simul(1000,pi,mu1, mu2, Sigma1, Sigma2)
ech1 = subset(mat, mat[,3] == 1);
ech2 = subset(mat,mat[,3]==2);
test <- erreurEstimee(ech1,ech2, regleEuclidienne,mu1,mu2);
test

qnorm
pnorm

#EXERCICE2

########################################################
########################################################
########################################################
library(MASS)
coul=c("black","magenta")

# Donnees "crabes"
# Utilisation des variables FL et RW pour reconnaÓtre le sexe

######################################################################################
# PrÈparation des donnÈes 
data(crabs)
crabs$FL1=crabs$FL/(crabs$FL+ crabs$RW+crabs$CL+crabs$CW+crabs$BD)
crabs$RW1=crabs$RW/(crabs$FL+ crabs$RW+crabs$CL+crabs$CW+crabs$BD)
D=crabs[,c(9,10,2)]
n=dim(D)[1]
plot(D[,1:2],col=coul[D$sex],pch=20)

######################################################################################
# Analyse discriminante lineaire en utilisant tout l'echantillon
D.lda <- lda(D[,1:2],D$sex)

len=50
x1p=seq(min(D$FL1),max(D$FL1),length=len)
x2p=seq(min(D$RW1),max(D$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))

y=predict(D.lda,grille)
yp=y$post[,1]-y$post[,2]
contour(x1p,x2p,matrix(yp,len),add=TRUE,levels=0,drawlabels=FALSE,col='green')

y <- predict(D.lda,D[,1:2])
erreur.lda <- sum(y$class != D$sex)/n

# Analyse discriminante quadratique en utilisant tout l'echantillon
plot(D[,1:2],col=coul[D$sex],pch=20)
D.qda <- qda(D[,1:2],D$sex)

len=50
x1p=seq(min(D$FL1),max(D$FL1),length=len)
x2p=seq(min(D$RW1),max(D$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))

y=predict(D.qda,grille)
yp=y$post[,1]-y$post[,2]
contour(x1p,x2p,matrix(yp,len),add=TRUE,levels=0,drawlabels=FALSE,col='red')

y <- predict(D.qda,D[,1:2])
erreur.qda <- sum(y$class != D$sex)/n

#EXTRAITS D'ECHANTILLONS

extraitQDA<-function(n,prc) {

	temp <- 1:n;
	temp[] = 0;
	
	for(i in 1:n){
	iapprM = sample(1:100, round(prc*100));
	iapprF = sample(1:100, round(prc*100));

	Dappr<-rbind(DM[iapprM,], DF[iapprF,]);
	Dtest <-rbind(DM[-iapprM,],DF[-iapprF,]);
	Dtest.qda <- qda(Dtest[,1:2],Dtest$sex)

	len=50
	x1p=seq(min(Dappr$FL1),max(Dappr$FL1),length=len)
	x2p=seq(min(Dappr$RW1),max(Dappr$RW1),length=len)
	grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))


	y <- predict(Dtest.qda,Dtest[,1:2])
	temp[i] <- sum(y$class != Dtest$sex)/n
	}

	return(mean(temp)*100);
}
extraitLDA<-function(n,prc) {

	temp <- 1:n;
	temp[] = 0;
	
	for(i in 1:n){
	iapprM = sample(1:100, round(prc*100));
	iapprF = sample(1:100, round(prc*100));

	Dappr<-rbind(DM[iapprM,], DF[iapprF,]);
	Dtest <-rbind(DM[-iapprM,],DF[-iapprF,]);
	Dtest.lda <- lda(Dtest[,1:2],Dtest$sex)

	len=50
	x1p=seq(min(Dappr$FL1),max(Dappr$FL1),length=len)
	x2p=seq(min(Dappr$RW1),max(Dappr$RW1),length=len)
	grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))


	y <- predict(Dtest.lda,Dtest[,1:2])
	temp[i] <- sum(y$class != Dtest$sex)/n
	}

	return(mean(temp)*100);
}

#Bonne methode 
DM <- subset(D,sex=="M");
DF<-subset(D,sex=="F");

iapprM = sample(1:100, round(2/3*100));
iapprF = sample(1:100, round(2/3*100));

Dappr<-rbind(DM[iapprM,], DF[iapprF,]);
Dtest <-rbind(DM[-iapprM,],DF[-iapprF,]);

	#CALCUL D'ERREUR pour 2/3
	
Dappr.qda <- qda(Dappr[,1:2],Dappr$sex)

len=50
x1p=seq(min(Dappr$FL1),max(Dappr$FL1),length=len)
x2p=seq(min(Dappr$RW1),max(Dappr$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))


y <- predict(Dappr.qda,Dappr[,1:2])
erreur.qda <- sum(y$class != Dappr$sex)/n
erreur.qda*100
#O,035

Dappr.lda <- lda(Dappr[,1:2],Dappr$sex)

len=50
x1p=seq(min(Dappr$FL1),max(Dappr$FL1),length=len)
x2p=seq(min(Dappr$RW1),max(Dappr$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))


y <- predict(Dappr.lda,Dappr[,1:2])
erreur.lda <- sum(y$class != Dappr$sex)/n
erreur.lda*100
#0,07


#CALCUL D'ERREUR pour 1/3
	
Dtest.qda <- qda(Dtest[,1:2],Dtest$sex)

len=50
x1p=seq(min(Dtest$FL1),max(Dtest$FL1),length=len)
x2p=seq(min(Dtest$RW1),max(Dtest$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))


y <- predict(Dtest.qda,Dtest[,1:2])
erreur.qda <- sum(y$class != Dtest$sex)/n

#0,045

Dtest.lda <- lda(Dtest[,1:2],Dtest$sex)

len=50
x1p=seq(min(Dtest$FL1),max(Dtest$FL1),length=len)
x2p=seq(min(Dtest$RW1),max(Dtest$RW1),length=len)
grille=data.frame(expand.grid(FL1=x1p,RW1=x2p))


y <- predict(Dtest.lda,Dtest[,1:2])
erreur.lda <- sum(y$class != Dtest$sex)/n
#0,045

#QUEST4




#exercice 3

vec1<-c(-0.6,1.3,-0.1,2.4,0.2)
vec2<-c(1.5,1.2,0.1,-1.2,0.9)
mat1<-cbind(vec1,vec2)

vec1<-c(2.3,0.7,2.7,3.6,1.3)
vec2<-c(2.9,2.4,1.0,1.6,2.1)
mat2<-cbind(vec1,vec2)

calcul des Sk : 
S2= t(mat2-mu2)%*%(mat2-mu2)*(5/4)*(1/5)
S1= t(mat1-mu1)%*%(mat1-mu1)*(5/4)*(1/5)

cas quadratique : 
sigma1
vec1    vec2
vec1  1.6322 -1.0420
vec2 -1.0420  1.2548

sigma2
vec1    vec2
vec1  1.3228 -0.4240
vec2 -0.4240  0.5422

cas linéaire
S = (1/8)*(4*S1 + 4*S2)

        vec1    vec2
vec1  1.4775 -0.7330
vec2 -0.7330  0.8985

variance non biaisée : 
var(mat1)
        vec1    vec2
vec1  1.4530 -0.9125
vec2 -0.9125  1.1750

var(mat2)
       vec1   vec2
vec1  1.312 -0.415
vec2 -0.415  0.535

ADL :
(1/8)*(4*var(mat1)+4*var(mat2))
vec1     vec2
vec1  1.38250 -0.66375
vec2 -0.66375  0.85500

