#EX1 : 

#Question 1 

#extraction des b√àb√às dont les m√ãres ont fumaient/fument
smoking_babies = subset(babies, smoke == "Smoking")
mean(smoking_babies$bwt)
	#equivaut ‚Ä°‚Ä†
	mean(babies[babies$smoke=="Smoking",1], na.rm=T)

#extraction des b√àb√às dont les m√ãres n'ont pas fum√à
nonsmoking_babies = subset(babies, smoke == "NonSmoking")
mean(nonsmoking_babies$bwt)
	#equivaut ‚Ä°
	mean(babies[babies$smoke=="NonSmoking",1], na.rm=T)
	
#info avec mediane/moyenne/quartiles
summary(babies[babies$smoke=="smoking",1])
summary(babies[babies$smoke=="NonSmoking",1])
	
#Boite ‚Ä°‚Ä† moustache des deux echantillons 
boxplot(babies[babies$smoke=="NonSmoking",1], babies[babies$smoke=="Smoking",1], notch=T)
	
	#commentaires : 
	#La moyenne du poids ‚Ä° la naissance pour une m√ãre non fumeur est plus √àl√àv√à que pour une m√ãre non fumeuse
	#Pour une m√ãre fumeuse, l'√àtendue est √àgalement olus √àl√àv√à, et les valeurs plus disper√às
	#Avec le notch, on affiche l'intervalle de confiance de la m√àdiane. Comme les deux intervalles sont disjoints, on peut dire que le fait que la m√ãre soit fumeuse influe sur le poids ‚Ä° la naissance

	#test de Student sur les moyennes
	#impossible de l'utiliser car l'echantillon correspondant aux m√ãres non fumeuses ne suit pas une loi normale



#Question 2

#Moyenne du temps de gestion pour une m√ãre fumeuse
mean(babies[babies$smoke=="Smoking",2], na.rm=T)
#277.9792

#Moyenne du temsp de gestion pour une m√ãre non fumeuse
mean(babies[babies$smoke=="NonSmoking",2], na.rm=T)
#280.1869

#R√àsum√às
	#fumeur
	summary(babies[babies$smoke=="Smoking",2])
	
	#non fumeur
	summary(babies[babies$smoke=="NonSmoking",2])

#boite ‚Ä° moustache
boxplot(babies[babies$smoke=="NonSmoking",2], babies[babies$smoke=="Smoking",2], notch=T)
boxplot(babies$gestation~babies$smoke, notch = T)
	#On observe que les intervalles de confiance pour la moyenne ne sont pas disjoints, donc le fait que la m√ãre fume n'influe pas sur le temps de gestation


#Question 3

pie(table(babies$ed[babies$smoke=="Smoking"]))
barplot(table(babies$smoke,babies$ed))
barplot(table(babies$smoke,babies$ed),beside=T)
#la proportion de mËres fumeuses diminue avec le niveau d'Ètude, c'est ‡ dire que plus on augmente le niveau d'Ètude, moins on a de chance de tomber sur une mËre fumeuse. Donc le niveau d'Ètude a une influence sur le fait que la mËre soit fumeuse ou non.
#Exercice 2

#1]
pairs(crabsquant,main="Donnees sur les crabes - Comparaison en fonction du sexe",pch=21,bg=c("red","green3")[crabs$sex])
pairs(crabsquant,main="Donnees sur les crabes - comparaison en fonction du genre",pch=21,bg=c("blue","orange")[crabs$sp])

#les variables sont corr√©l√©s donc il semble impossible d'affirmer q'il y a une diff√©rence significative en fonction du sexe ou du genre

#etude des variables quantitatives en fonction des variables qualitatives
boxplot(crabs[crabs$sex=="F", 5], crabs[crabs$sex=="M", 5], notch = T)
#permet d'affirmer qu'il y a une diffËrence significative entre les sexes

boxplot(crabs[crabs$sp=="B", 4], crabs[crabs$sp=="O", 4], notch = T)
boxplot(crabs[crabs$sp=="B", 5], crabs[crabs$sp=="O", 5], notch = T)
boxplot(crabs[crabs$sp=="B", 6], crabs[crabs$sp=="O", 6], notch = T)
boxplot(crabs[crabs$sp=="B", 7], crabs[crabs$sp=="O", 7], notch = T)
boxplot(crabs[crabs$sp=="B", 8], crabs[crabs$sp=="O", 8], notch = T)

#diffËrence significative : possibilitÈ de diffËrencier le sexe et l'espece selon une mesure de plusieurs caract√©ristiques 

#calcul de la correlation : 

cor(crabsquant,crabsquant)
#La correlation vient du fait quand on est en presence d'un "petit crabe", l'ensemble de ses attributs (et donc des mesures) est petit.
#Pour mener l'Ètude avec des variables non correlÈes, on va utiliser la methode de l'ACP en rÈduisant le nombre de variable d'Ètude.


#EXERCICE 2 : 

 #1]
 #Matrice de départ
  A = matrix(c(3,1,2,2,4,4,3,1,3,3,6,4), nrow=4,ncol=3)
 #Matrice centrée
  X<-centre(A)
 #matrice de variance
 S = (1/4)*t(X)%*%X
 
 #calcul des axes factoriels :
 U = eigen(S)$vectors
 
 
 #pourcentage d'inertie pour chacun des axes :
 vp = eigen(S)$values
 pr = sum(vp)
 
 
 
 #calcul des composantes principales :
	C = X%*%U
 
 

 #2]
 	#matrice de départ
  	A = matrix(c(6.0,8.0,6.0,14.5,14.0,11.0,5.5,13.0,9.0,6.0,8.0,7.0,14.5,14.0,10.0,7.0,12.5,9.5,5.0,8.0,11.0,15.5,12.0,5.5,14.0,8.5,12.5,5.5,8.0,9.5,15.0,12.5,7.0,11.5,9.5,12.0,8.0,9.0,11.0,8.0,10.0,13.0,10.0,12.0,18.0), nrow=9,ncol=5)
  	
  	#centrage
	X<-centre(A)
	
	#matrice de variance
	S<-(1/9)*t(X)%*%X
	eigen(S)
	
	#Axes principaux d'inerties : 
	
	#vecteur de valeurs propres :
	vp = eigen(S)$values
	#inertie du nuage :
	sum (vp)
	
	#vecteurs propres correspondants :
	u1 = eigen(S)$vectors[,1]
	u2 = eigen(S)$vectors[,2]
	u3 = eigen(S)$vectors[,3]
	u4 = eigen(S)$vectors[,4]
	u5 = eigen(S)$vectors[,5]
	
	#pourcentage d'inertie expliquée par chacun des axes :
	pr = sum(vp)
	(vp/pr)*100 
	
	#pourcentages d'inertie expliquée par les sous espaces vectoriels :
		#utilisation de la fonction prinerexp(X,y)
		prinerexp(vp,pr)
	
	
	#calcul des composantes principales :
	U = eigen(S)$vectors
	C = X%*%U


