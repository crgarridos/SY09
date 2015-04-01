sizerange <- function()
{
	plot(x,col=couleur)
	colorsToUse <- mat.or.vec(10, 1)
	colorsToUse[1] = "red"
	colorsToUse[2] = "blue"
	colorsToUse[3] = "green"
	colorsToUse[4] = "yellow"
	colorsToUse[5] = "orange"
	colorsToUse[6] = "purple"
	colorsToUse[7] = "pink"
	colorsToUse[8] = "grey"
	colorsToUse[9] = "brown"
	colorsToUse[10] = "black"
	for (i in 1:5)
	{
		T<-class.ind(couleur)
		model<-nnet(x,T,size=i,decay=0,softmax=TRUE,maxit=500)
		# Valeur des poids
		print(model$wts)
		len<-50
		xp<-seq(min(x[,1]),max(x[,1]), length=len)
		yp<-seq(min(x[,2]),max(x[,2]), length=len)
		grille<-expand.grid(z1=xp,z2=yp)
		# Proba. a posteriori d appartenance aux classes
		Z<-predict(model,grille)
		zp<-Z[,4] - pmax(Z[,3],Z[,2], Z[,1])
		contour(xp,yp,col=colorsToUse[i],matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
		zp<-Z[,1] - pmax(Z[,2], Z[,3],Z[,4])
		contour(xp,yp,col=colorsToUse[i],matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
		zp <- Z[,2] - pmax(Z[,1], Z[,3],Z[,4])
		contour(xp,yp,col=colorsToUse[i],matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
	}

	return(TRUE)
}

decayrange <- function()
{
	plot(x,col=couleur)
	colorsToUse <- mat.or.vec(10, 1)
	colorsToUse[1] = "red"
	colorsToUse[2] = "blue"
	colorsToUse[3] = "green"
	colorsToUse[4] = "yellow"
	colorsToUse[5] = "orange"
	colorsToUse[6] = "purple"
	
	decayToUse <- mat.or.vec(10, 1)
	decayToUse[1] = 0.001
	decayToUse[2] = 0.01
	decayToUse[3] = 0.1
	decayToUse[4] = 1
	decayToUse[5] = 10
	decayToUse[6] = 100
	
	for (i in 1:6)
	{
		T<-class.ind(couleur)
		model<-nnet(x,T,size=5,decay=decayToUse[i],softmax=TRUE,maxit=500)
		# Valeur des poids
		print(model$wts)
		len<-50
		xp<-seq(min(x[,1]),max(x[,1]), length=len)
		yp<-seq(min(x[,2]),max(x[,2]), length=len)
		grille<-expand.grid(z1=xp,z2=yp)
		# Proba. a posteriori d appartenance aux classes
		Z<-predict(model,grille)
		zp<-Z[,4] - pmax(Z[,3],Z[,2], Z[,1])
		contour(xp,yp,col=colorsToUse[i],matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
		zp<-Z[,1] - pmax(Z[,2], Z[,3],Z[,4])
		contour(xp,yp,col=colorsToUse[i],matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
		zp <- Z[,2] - pmax(Z[,1], Z[,3],Z[,4])
		contour(xp,yp,col=colorsToUse[i],matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
	}

	return(TRUE)
}
