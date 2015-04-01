p1<-0.5
p2<-0.5

x<- log(cbind(crabs$FL,crabs$RW))
T <- class.ind(crabs$sp)
couleur<- rep('red',n)
couleur[crabs$sp == "O"] <- 'blue'

tmp<-rbind(x[1:50,1:2],x[101:150,1:2])
m1<-apply(tmp,2,mean)
s1<-c(cov(tmp)[1,1],cov(tmp)[2,2])

tmp2<-rbind(x[51:100,1:2],x[151:200,1:2])
m2<-apply(tmp2,2,mean)
s2<-c(cov(tmp2)[1,1],cov(tmp2)[2,2])

s <- rbind(s1,s2)
m <- rbind(m1,m2)

len<-50

xp<-seq(min(x[,1]),max(x[,1]), length=len)
yp<-seq(min(x[,2]),max(x[,2]), length=len)
grille<-expand.grid(z1=xp,z2=yp)

plot(x,col=couleur)
T<-class.ind(couleur)
model<-nnet(x,T,size=1,decay=0,softmax=TRUE,maxit=500)
Z<-predict(model,grille)
zp<-Z[,1] - Z[,2]
contour(xp,yp,matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)

Z<-p1*dnorm(grille[,1],m[1,1],s[1,1])*dnorm(grille[,2],m[1,2],s[1,2])
Z<-cbind(Z,p2*dnorm(grille[,1],m[2,1],s[2,1])*dnorm(grille[,2],m[2,2],s[2,2]))
zp<-Z[,1] - Z[,2]
contour(xp,yp,matrix(zp,len),add=TRUE,levels=0,drawlabels=FALSE)
