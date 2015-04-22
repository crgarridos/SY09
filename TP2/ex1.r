X = t(matrix(c(8.5,1.5,3.5,5.0,2.0, 6.5,9.5, 1.5,8.5, 2.5,3.0, 6.5,9.0, 2.5,2.0, 5.5),2,8))
Xc = scale(X, scale = F) # centrage de la matrice
n = dim(Xc)[1];
D = matrix(ncol=n,nrow=n)
#for(i in 1:n)
#  for(j in 1:n)
#    D[i,j] = sqrt(sum((X[i,] - X[j,])^2))
# 1.- Calculer le tableau D² des distances euclidiennes associé à ces données.
D2 = dist(Xc)^2;
# 2. Calculer la matrice W des produits scalaires : 
# d’une part directement à partir de X, d’autre part à partir de D²
W = Xc%*%t(Xc);
W
In = diag(n);
In
Un = as.vector(matrix(1,1,n))#c(1,1,1,1,1,1,1,1);
Un
Qn = In - (1/n)*Un;
W_ = -(1/2)*Qn%*%as.matrix(D2)%*%Qn; 
W_
W
