#babies<-read.table("C:/Users/sy09p032/Desktop/TP1/babies23.txt",header=T)
#babies<-read.table("K:/SY09TP1/TP1/babies23.txt",header=T)
babies<-read.table("C:/Users/Asus/Desktop/TP1/babies23.txt",header=T)

babies<-babies[c(7,5,8,10,12,13,21,11)]
names(babies)<-c("bwt","gestation","parity","age","height","weight","smoke","education")

babies[babies$bwt == 999, 1] <- NA
babies[babies$gestation == 999, 2] <- NA
babies[babies$age == 99, 4] <- NA
babies[babies$height == 99, 5] <- NA
babies[babies$weight == 999, 6] <- NA
babies[babies$smoke == 9, 7] <- NA
babies[babies$education == 9, 8] <- NA
babies$smoke<-factor(c("NonSmoking","Smoking","NonSmoking","NonSmoking")[babies$smoke+1])
babies$education<-factor(babies$education,ordered=T)

#qestion 1

mpnonsmoke=summary(babies[babies$smoke=="NonSmoking",])
mpsmoke=summary(babies[babies$smoke=="Smoking",])

bwtnonsmoke=babies$bwt[babies$smoke=="NonSmoking"]
bwtsmoke=babies$bwt[babies$smoke=="Smoking"]
gestnonsmoke=babies$gestation[babies$smoke=="NonSmoking"]
gestsmoke=babies$gestation[babies$smoke=="Smoking"]

mpnonsmoke
sd(bwtnonsmoke[!is.na(bwtnonsmoke)])
sd(gestnonsmoke[!is.na(gestnonsmoke)])
mpsmoke
sd(bwtsmoke[!is.na(bwtsmoke)])
sd(gestsmoke[!is.na(gestsmoke)])

#boxplot(bwtnonsmoke,bwtsmoke,main="Comparaison du poid des bébés",sub=("1=smoker"))

boxplot(babies$bwt~babies$smoke,main="Comparaison du poids des bébés")
boxplot(babies$gestation~babies$smoke,main="Comparaison du temps gestation")


#boxplot(gestnonsmoke[!is.na(gestnonsmoke)],gestsmoke[!is.na(gestsmoke)],main="Comparaison du temps gestation")


t.test(gestsmoke,gestnonsmoke)
t.test(bwtsmoke,bwtnonsmoke)

plot(babies$education,babies$smoke,main="Comparaison du niveau d'étude")
