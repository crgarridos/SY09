aftd <- function(D)
{
	D = data.matrix(D);
	D2 = D * D;
	n = dim(D2)[1];
	Qn = diag(n) - 1/n * matrix(1,n,n) ;

	W = -1/2 * Qn %*% D2 %*% Qn;
	Wn = 1 / n * W;

	eig = eigen(Wn);
	val = abs(eig$values);

	l = diag(val);
	v = sqrt(n) * eig$vectors;
	c = v %*% sqrt(l);
	
	plot(c);
	ret <- list(L=l, V=v, C=c);
}