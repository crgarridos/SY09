notes<-read.table("notes.txt")
princomp(notes) #calcule les differents weas
(princomp(notes)$sdev)^2 # Valeurs propres;
princomp(notes)$loadings # axes principaux
princomp(notes)$scores # matrice ACP

#centrage du tableau de données
A= scale(notes,scale=FALSE)
biplot(princomp(A),main="Notes") 					#axes 1 et 2
biplot(princomp(A),choices = c(1,3),main="Notes")	#axes 1 et 3

png(file = "plots/2.2_plot.png");
plot(princomp(A));
dev.off();