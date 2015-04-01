library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]
boxplot(crabs$FL[crabs$sp=="B"],crabs$FL[crabs$sp=="O"],crabs$RW[crabs$sp=="B"],crabs$sommeRW[crabs$sp=="O"],crabs$CL[crabs$sp=="B"],crabs$CL[crabs$sp=="O"],crabs$CW[crabs$sp=="B"],crabs$CW[crabs$sp=="O"],crabs$BD[crabs$sp=="B"],crabs$BD[crabs$sp=="O"],col=c("blue","orange"),main="FL, RW, CL, CW, BD en fonction d'espèce",sub="Bleu : espèce bleu ; Orange : espèce orange",names=c("FL","FL","RW","RW","CL","CL","CW","CW","BD","BD"))
boxplot(crabs$FL[crabs$sex=="M"],crabs$FL[crabs$sex=="F"],crabs$RW[crabs$sex=="M"],crabs$RW[crabs$sex=="F"],crabs$CL[crabs$sex=="M"],crabs$CL[crabs$sex=="F"],crabs$CW[crabs$sex=="M"],crabs$CW[crabs$sex=="F"],crabs$BD[crabs$sex=="M"],crabs$BD[crabs$sex=="F"],col=c("grey","pink2"),main="FL, RW, CL, CW, BD en fonction de sexe",sub="Gris : masculin ; Rose : féminin",names=c("FL","FL","RW","RW","CL","CL","CW","CW","BD","BD"))
#boxplot(crabs$RW~crabs$sp,data=crabs)





#correlation


levels(crabs$sp)<-c(1,2)
levels(crabs$sex)<-c(1,2)
a=as.numeric(crabs$sp)
b=as.numeric(crabs$sex)
c=2*a+b-2
pairs(crabsquant,main="Les crabs",pch=21,bg=c("red","green3","blue","orange")[c])
somme=apply(crabsquant,1,sum)
pairs(crabsquant/somme,main="Les crabs",pch=21,bg=c("red","green3","blue","orange")[c])

