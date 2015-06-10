norma = function(x) sqrt(sum(x^2))

X = t(matrix(c(8.5,1.5,3.5,5.0,2.0, 6.5,9.5, 1.5,8.5, 2.5,3.0, 6.5,9.0, 2.5,2.0, 5.5),2,8))
rownames(X)=c("x1","x2","x3","x4","x5","x6","x7","x8")
colnames(X)=c("x","y")
Xc = scale(X, scale = F, center = T) # centrage de la matrice
n = dim(Xc)[1];

# 1)
D2 = dist(Xc)^2;

# 2)
W = Xc%*%t(Xc);
In = diag(n);
Un = as.vector(matrix(1,ncol=1,nrow=n));
Qn = In - (1/n)*Un;
W_ = -(1/2)*Qn%*%as.matrix(D2)%*%Qn;

# 3)
# V = (1/n)*t(Wn)%*%Wn
Wn = (1/n)*W;
cent = scale(Wn, center = TRUE, scale = FALSE)
tmp = eigen(cent, symmetric = T);

# 4)
tr(tmp$values)
L 
#L <- diag(tmp$values)
#U <- tmp$vectors*sqrt(n)


C = U%*%sqrt(round(L,2))

# AFTD = cmdscale(D2,k=2)
plot(X,pch=20,xlim=c(2,10), main="Représentation de X",xlab="",ylab="")
plot(aftd(X)$C,pch=20, main="Représentation de l'AFTD de X",xlab="",ylab="")

mutations <- read.table("mutations2.txt", header=F, row.names=1)
mutations <- as.dist(mutations)
aftd(mutations)
