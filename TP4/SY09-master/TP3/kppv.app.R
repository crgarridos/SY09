kppv.app <- function(Xapp, zapp, Xval, zval, nppv)
{
  #La première fait l’apprentissage du nombre « optimal » de voisins K (choisi parmi un ensemble
  #prédéfini de valeurs possibles) à utiliser dans l’algorithme, à partir d’un ensemble de validation Xval
  #(dimensions nval × p) et des étiquettes zval associées (longueur nval). Elle doit donc prendre en
  #argument d’entrée le tableau individus-variables Xapp, le vecteur zapp des étiquettes associées, le
  #tableau Xval, le vecteur zval, et un ensemble de valeurs nppv. Elle doit retourner la valeur de K
  #choisie (parmi les valeurs contenues dans nppv), c’est-à-dire celle qui donne les meilleurs résultats
  #sur l’ensemble de validation.
  #Xval ensemble de validation
  #zval étiquattes associées
  for(i in nppv){
    min <- i
    erreur_opt <- 1
    tmp <- kppv.val(Xapp, zapp, i, Xval)
    erreur <- sum((tmp == zval)==TRUE)/length(zval)
    if(erreur_opt > erreur){
      erreur_opt <- erreur
      min <- i
    }
  }
  min
}