
Y=matrix(c(3,1,2,2,4,4,3,1,3,3,6,4),nrow=4)
n=dim(Y)[1]
X=Y-matrix(1,n,1)%*% apply(Y,2,mean)	#centrage
#X = X/matrix(1,n,1)%*% apply(Y,2,sd)	#réduction
S = (1/n) * t(X) %*% X 					#calcule covariance qui est symétrique, matrice de corrélation
tmp = eigen(S,symmetric=TRUE)
L <- diag(tmp$values)
U <- tmp$vectors
C <- X %*% U							#composantes principales des individus
COR <- diag(1/apply(X^2,1,sum)) %*% C^2
CTR <- (1/n) * C^2 %*% diag(1/diag(L))
D <- diag(1/(sqrt((n-1)/n)*apply(X,2,sd))) %*% U %*% sqrt(L)


U
tmp$values/sum(tmp$values) 				#pourcentages d'inertie
cumsum(tmp$values/sum(tmp$values)) 		#pourcentages d'inertie cumulé
C


plot(C,main="Représentation des individus",type="n",xlab=c("Axe 1"),ylab=c("Axe 2"))
abline(v=0)
abline(h=0)
nom<-c("1","2","3","4")
text(C[,1],C[,2],nom)


plot(-1:1,-1:1, type="n", xlab="Axe 1", ylab="Axe 2",main="Représentation des variables")
curve(sqrt(1-x^2),-1,1,add=T)
curve(-sqrt(1-x^2),-1,1,add=T)
nom_val=c("1","2","3")
text(D[,1],D[,2],nom_val,abline(h=0),abline(v=0))



X1=C[,1]%*%t(U[,1])
X2=X1+C[,2]%*%t(U[,2])
X3=X2+C[,3]%*%t(U[,3])


#Exercice 2.2
#notes
notes=t(matrix(c(6.0,6.0,5.0,5.5,8,8.0,8.0,8.0,8.0,9,6.0,7.0,11.0,9.5,11,14.5,14.5,15.5,15.0,8,14.0,14.0,12.0,12.5,10,11.0,10.0,5.5,7.0,13,5.5,7.0,14.0,11.5,10,13.0,12.5,8.5,9.5,12,9.0,9.5,12.5,12.0,18,9.67,9.83,10.22,10.05,11),5))[1:9,]
rownames(notes)=c("jean","aline","annie","monique","didier","andre","pierre","brigitte","evelyne")
colnames(notes)=c("math","scie","fran","lati","dess")
notes
#centrage du tableau de donnÃ©es
#A= scale(notes,scale=FALSE)
#A


L=(princomp(notes)$sdev)^2
loadings(princomp(notes))		#U=princomp(A)$loadings
#matrice des composantes principales
princomp(notes)$scores

#ACP
plot(princomp(A))
biplot(princomp(A),main="Notes") 					#axes 1 et 2
biplot(princomp(A),choices = c(1,3),main="Notes")	#axes 1 et 3




#plot(C,type="n",xlim=c(-10,10),ylim=c(-10,10))
#nom=c("jean","alin","annie","monique","didier","andr?,"pierre","brigitte","evelyne")
#text(C[,1],C[,2],nom,abline(v=0),abline(h=0))
#% d'inertie
#e=(X$sdev)^2/sum((X$sdev)^2)
#e[1];e[1]+e[2];e[1]+e[2]+e[3];e[1]+e[2]+e[3]+e[4];e[1]+e[2]+e[3]+e[4]+e[5]




#Exercice 2.3
library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
A = crabsquant

spB <- 1:100
spO <- 101:200

biplot(princomp(A)) 					#axes 1 et 2
biplot(princomp(A),choices = c(1,3))	#axes 1 et 3



	Y=as.matrix(A)
	n=dim(Y)[1]
	X=Y-matrix(1,n,1)%*% apply(Y,2,mean)	#centrage
	X = X/matrix(1,n,1)%*% apply(Y,2,sd)	#rÃ©duction
	S = (1/n) * t(X) %*% X 					#matrice de corrÃ©lation
	tmp = eigen(S,symmetric=TRUE)
	L <- diag(tmp$values)
	U <- tmp$vectors
	C <- X %*% U							#composantes principales des individus
	COR <- diag(1/apply(X^2,1,sum)) %*% C^2
	CTR <- (1/n) * C^2 %*% diag(1/diag(L))
	D <- diag(1/(sqrt((n-1)/n)*sd(X))) %*% U %*% sqrt(L)


	blablaa=tmp$values/sum(tmp$values) 				#pourcentages d'inertie
	names(blablaa)=c("Comp1","Comp2","Comp3","Comp4","Comp5")
	barplot(blablaa)
	cumsum(tmp$values/sum(tmp$values)) 		#pourcentages d'inertie cumulÃ©e

	#plot(-1:1,-1:1,type="n",xlab='Axe 1',ylab='Axe 2!!')
	#t(D[,1],D[,2],1:4);abline(h=0);abline(v=0)    

	 plot(C[,1],C[,2],type="n",xlab="Composante 1",ylab="Composante 2");
	 points(C[spO,1],C[spO,2],bg=crabs$sex,pch=21)
	 points(C[spB,1],C[spB,2],col=crabs$sex,pch=24)

dev.new()
	 plot(C[,2],C[,3],type="n",xlab="Composante 2",ylab="Composante 3");
	 points(C[spO,2],C[spO,3],bg=crabs$sex,pch=21)
	 points(C[spB,2],C[spB,3],col=crabs$sex,pch=24)






boxplot(A)
A = A[1:199,]
spB <- 1:100
spO <- 101:199


A=A/A$RW ;  pairs(A)
#pairs(A/A$RW)		#grandecorrelation entre CL et CW
A = A[,c("FL","BD","CW")]
biplot(princomp(A))






