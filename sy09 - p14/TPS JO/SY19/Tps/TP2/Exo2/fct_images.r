
# affichage d'une image noir & blanc 
implotbw <- function(im)
{
	nx <- dim(im)[1]
	ny <- dim(im)[2]

	imaff <- t(im)
	imaff <- as.matrix(imaff[,ny:1])

	image(1:nx, 1:ny, imaff, col=gray((1:255)/255))
}

# image : matrice -> vecteur
immat2imvec <- function(immat)
{
	imvec <- NULL
	imvec$vec <- as.vector(t(immat))
	imvec$nx <- dim(immat)[1]
	imvec$ny <- dim(immat)[2]
	return(list(vec=imvec$vec, nx=imvec$nx, ny=imvec$ny))
}

# image : vecteur -> matrice
imvec2immat <- function(imvec, nx=NA, ny=NA)
{
	if(is.list(imvec))
	{
		vec <- imvec$vec
		nx <- imvec$nx
	}
	else
	{
		vec <- imvec
		nx <- length(imvec)
	}

	immat <- matrix(vec, nrow=nx, byrow=T)
}
