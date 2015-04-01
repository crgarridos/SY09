Exercice 2 (suite)

library(MASS)
data(iris)
d <- dist()*dist(iris[,1:4])
irisclust<-hclust(d)
plot(irisclust)
cl1 <- pam(iris[,1:4], 2)$clustering
cl2 <- pam(iris[,1:4], 3)$clustering
cl3 <- pam(iris[,1:4], 4)$clustering

iris = cbind(iris,cl1);
iris = cbind(iris,cl2);
iris = cbind(iris,cl3);

clusplot(iris[,1:4], cl1, color = TRUE, main = "Clusplot iris", shade = TRUE, labels = 2)
clusplot(iris[,1:4], cl2, color = TRUE, main = "Clusplot iris", shade = TRUE, labels = 2)
clusplot(iris[,1:4], cl3, color = TRUE, main = "Clusplot iris", shade = TRUE, labels = 2)

plot(agnes(iris[,1:4], metric = "euclidian", method = "ward"), main='agnes')
plot(agnes(iris[,1:4], metric = "euclidian", method = "ward"), main='Diana')

#========== données Iris =================
setwd("z:/public_html/sy09/tp02")
library(MASS)
library(lattice)
library(cluster)
data(iris)
#--------------------------------question 1-------------------

k1 = kmeans(iris[,1:4], 2, nstart = 20)
k2 = kmeans(iris[,1:4], 3, nstart = 20)
k3 = kmeans(iris[,1:4], 4, nstart = 20)

plot(iris[,1:4], col = c('green', 'blue')[k1$cluster], main='partition k=2')
plot(iris[,1:4], col = c('green', 'blue', 'red')[k2$cluster], main='partition k=3')
plot(iris[,1:4], col = c('green', 'blue', 'red', 'purple')[k3$cluster], main='partition k=4')

iris = cbind(iris,k1=k1$cluster);
iris = cbind(iris,k2=k2$cluster);
iris = cbind(iris,k3=k3$cluster);

clustersCantities(iris,k){
   ncls = nrow(unique(iris[k]));
   species = levels(iris$Species);
   res = matrix(0,ncol = length(species));
   colnames(res) = species;#c("setosa","versicolor","virginica");
   #rownames(res) <- rownames(res, do.NULL = FALSE,prefix="Cluster ")
   for(i in 1:ncls){
		#j=0;
		#for(s in species)
		#	summ[j<-j+1]= nrow(subset(subset(iris,iris[k]==i),Species==s));
		set = nrow(subset(subset(iris,iris[k]==i),Species=="setosa"));
		ver = nrow(subset(subset(iris,iris[k]==i),Species=="versicolor"));
		vir = nrow(subset(subset(iris,iris[k]==i),Species=="virginica"));
		res = rbind(res,c(set,ver,vir));
   }
   res[-1,];
}
clustersCantities(iris,"k1");
clustersCantities(iris,"k2");
clustersCantities(iris,"k3");
View(clustersCantities(iris,"k1"));
View(clustersCantities(iris,"k2"));
View(clustersCantities(iris,"k3"));

pngHD <- function(f){
	png(file = f,width = 2000, height = 2000, units = "px", pointsize = 12,   bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))
}
png(file = "plots/clusplot_kmeans_1.png")
#pngHD("plots/clusplot_kmeans_1_hd.png");
iris = cbind(iris,kk1=c(matrix(2,50),matrix(1,47),matrix(2,3),matrix(1,50)))
clusplot(iris[,1:4], iris$kk1, color = TRUE, shade = TRUE, labels = 2, main = "Classification Kmeans avec 2 classes")
dev.off()

png(file = "plots/clusplot_kmeans_2.png")
#pngHD("plots/clusplot_kmeans_3_hd.png");
iris = cbind(iris,kk2=c(matrix(2,50),matrix(1,2),matrix(3,48),matrix(1,36),matrix(3,14)))
clusplot(iris[,1:4], iris$kk2, color = TRUE, shade = TRUE, labels = 2, main = "Classification Kmeans avec 3 classes")
dev.off()

png(file = "plots/clusplot_kmeans_3.png")
#pngHD("plots/clusplot_kmeans_3_hd.png");
clusplot(iris[,1:4], iris$k3, color = TRUE, shade = TRUE, labels = 2, main = "Classification Kmeans avec 4 classes")
dev.off()
	
#--------------------------------question 2-------------------
#kmeans depend du premier point choisi!!!!!!!!!!
stabilite <-function(ncl)
{	
	res = c();#matrix(nrow=1, ncol=50)
	for (i in 1:50)
	{
		K<-kmeans(iris[,1:4], ncl, 20);
		res[i]<- sum(K$withinss); #inertie intra-classe
	}
	res;
}
unique(stabilite(3));
#------------------question 3-------------------

inertie <-function()
{	
	inertie = matrix(nrow=100, ncol=9);
	
	for(k in 2:10)
	{
		for (i in 1:100)
		{
			K<-kmeans(donnees$num, k);
			inertie[i,k-1]<- sum(K$withinss); 
		}
	}
	return (inertie);
}

inertie <-function()
{	
	inertie = matrix(nrow=100, ncol=6);
	resmin = matrix(nrow=1, ncol=6);
	for(k in 2:7)
	{	
		for (i in 1:100)
		{	
			
			K<-kmeans(iris[,1:4], k);
			if(i==1)
			{
				resmin[1,k-1] = sum(K$withinss);
			}
			inertie[i,k-1]<- sum(K$withinss);
			if( resmin[1,k-1] > inertie[i,k-1])
				resmin[1,k-1] = inertie[i,k-1];	 
		}
	}
	result<-NULL;
	result$inertie = inertie;
	result$min = resmin;
	return (result);
}

result <- inertie();
##plot(c(2:7),result$min,type="l")
png(file = "plots/plot_inertie_intra_1.png")
plot(c(2:7),result$min,type="l",xlab="nombre de classes",ylab="inertie",main = "Nombre de classes optimal")
dev.off()

--------------------------------question 4-------------------

d = dist(donnees$num)
h = hclust(d, method="single")
cut <- cutree(h,3)

par(mfrow=c(1,2)) 
plot(cut)
plot(kmeans(donnees$num,3)$cluster)


 ========== données Crabe =================
	 
	library(MASS)
	data(crabs)
	crabsquant <- crabs[,4:8]
	crabsquant <- crabsquant/matrix(rep(crabsquant[,4],4),nrow=dim(crabsquant)[1],byrow=F)

	K<- kmeans(crabsquant,4)

	len = dim(crabs)[1]
	class = matrix(nrow=len)

	class[which(crabs$sex=="F" & crabs$sp =="O"),]="F&O"
	class[which(crabs$sex=="F" & crabs$sp =="B"),]="F&B"
	class[which(crabs$sex=="M" & crabs$sp =="O"),]="M&O"
	class[which(crabs$sex=="M" & crabs$sp =="B"),]="M&B"

	table(K$cluster,class)

	d = dist(crabsquant)
	h = hclust(d, method="single")
	cut <- cutree(h,4)
	table(cut,class)
