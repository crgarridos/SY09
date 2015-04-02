A = matrix(c(3, 4, 3, 1, 4, 3, 2, 3, 6, 2, 1, 2), nrow = 4, ncol = 3, byrow = T);
X = scale(A, center = T, scale = F);
n = dim(X)[1]
Dp = diag(n) * 1 / n
V = t(X) %*% Dp %*% X;
tmp = eigen(V, symmetric = T);
L <- diag(tmp$values)
cat("\nValeurs propres\n")
prmatrix(L)
lambdas = tmp$values;
inerties = lambdas/sum(lambdas)
U <- tmp$vectors
cat("\nVecteurs propres\n")
prmatrix(U)
C <- X %*% U
cat("\nComposantes principales\n")
prmatrix(C)
COR <- diag(1 / apply(X^2, 1, sum)) %*% C^2
cat("\nContributions axe/individu\n")
prmatrix(COR)
CTR <- (1 / n) * C^2 %*% diag(1 / diag(L))
cat("\nContributions individu/axe\n")
prmatrix(CTR)
D <- cor(X, C)
cat("\nRepresentation des variables/Matrice des correlations\n")

# dev.off()
plot(princomp(X), main = "Représentation des individus dans le premier plan factoriel", ylab = "axe 2", xlab = "axe 1");
abline(h = 0, v = 0, col = "gray")


plot(D,
     main = "Représentation des trois variables dans le premier plan factoriel",
     xlim= c(-1,1),
     ylim= c(-1,1),
     type = "n",
     xlab = "Axe factoriel 1",
     ylab = "Axe factoriel 2")
text(D[, 1], D[, 2], c(1, 2, 3), col = "red");
text(C[, 1], C[, 2], c(1, 2, 3), col = "blue");
abline(h = 0, v = 0, col = "gray");
curve(sqrt(1 - x^2), -1, 1, add = T, col = "gray");
curve(-sqrt(1 - x^2), -1, 1, add = T, col = "gray");
# par(F);


K1 <- C[, 1] %*% t(U[, 1])
K2 <- K1 + C[, 2] %*% t(U[, 2])
K3 <- K2 + C[, 3] %*% t(U[, 3])
cat("\nK1 <- C[, 1] %*% t(U[, 1])\n")
prmatrix(K1)
cat("\nK2 <- K1 + C[, 2] %*% t(U[, 2])\n")
prmatrix(K2)
cat("\nK3 <- K2 + C[, 3] %*% t(U[, 3])\n")
prmatrix(K3)

